{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_HADDOCK prune #-}


module Redo where

import           Control.Exception    (IOException (..), catch, catchJust)
import           Control.Monad        (filterM, guard, liftM, unless)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import           Data.Map.Lazy        (adjust, fromList, insert, toList)
import           Data.Maybe           (listToMaybe)
--import           Data.Typeable        (typeOf)

import           Debug.Trace          (traceShow)
import           GHC.IO.Exception     (IOErrorType (..))
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       getCurrentDirectory,
                                       getDirectoryContents,
                                       removeDirectoryRecursive, removeFile,
                                       renameFile, setCurrentDirectory)
import           System.Environment   (getArgs, getEnvironment, getProgName,
                                       lookupEnv)
import           System.Exit          (ExitCode (..), exitWith)
import           System.FilePath      (hasExtension, replaceBaseName,
                                       splitFileName, takeBaseName, (</>))
import           System.IO            (IOMode (..), hFileSize, hGetLine,
                                       hPutStrLn, stderr, withFile)
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)

import           System.Process       (CreateProcess (..), createProcess, shell,
                                       waitForProcess)

traceShow' :: Show b =>  b -> b
traceShow' arg = traceShow arg arg

-- | This is the directory that redo will store and read metadata on targets from.
metaDir :: String
metaDir = ".redo"

main :: IO ()
main = do
  topDir <- getCurrentDirectory
  getArgs >>= mapM_ (\arg -> do
    let (dir, file) = splitFileName arg
    setCurrentDirectory dir
    redo file dir
    setCurrentDirectory topDir)
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  case (progName, redoTarget') of
      ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
      ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable"
      _ -> return ()

-- | Rebuild a given target if it's out-of-date or doesn't exist.
redo :: String    -- ^ target (file) name
     -> FilePath  -- ^ the current directory (for output purposes only)
     -> IO ()
redo target dir = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe missingDo redo' =<< doPath target
  where
      redo' :: FilePath -> IO ()
      redo' path = do
        hPutStrLn stderr $ "redo " ++ (if dir == "./" then "" else dir) ++ target
        catchJust (guard . isDoesNotExistError)
                  (removeDirectoryRecursive metaDepsDir)
                  (\_ -> return ())
        createDirectoryIfMissing True metaDepsDir
        writeMD5 target path
        oldEnv <- getEnvironment
        let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
        (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
        exit <- waitForProcess ph
        case exit of
          ExitSuccess -> do
            size <- fileSize tmp
            if size > 0
            then renameFile tmp target
            else removeFile tmp
          ExitFailure code -> do
            hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
            removeFile tmp
            exitWith $ ExitFailure code
      tmp = target ++ "---redoing"
      metaDepsDir = metaDir </> target
      missingDo = do
        exists <- doesFileExist target
        unless exists $ error $ "no .do file found for target '" ++ target ++ "'"
      cmd path = unwords ["sh -e", path, "0", takeBaseName target, tmp, ">", tmp]


-- | The path if a 'do' script exists
doPath :: FilePath -- ^ the path to check for a do script
       -> IO (Maybe FilePath)
doPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

-- | Determines of a dependencies of a given filepath require rebuilding
-- (i.e. contain changes since last build)
upToDate :: FilePath -- ^ the path to build
         -> IO Bool
upToDate target = catch
  (do exists <- doesFileExist target
      if exists
      then do md5s <- getDirectoryContents $ metaDir </> target
              and `liftM` mapM depUpToDate md5s
      else return False)
  (\(_ :: IOException) -> return False)
  where depUpToDate :: String -> IO Bool
        depUpToDate oldMD5 = catch
          (do dep <- withFile (metaDir </> target </> oldMD5) ReadMode hGetLine
              newMD5 <- fileMD5 dep
              doScript <- doPath dep
              case doScript of
                Nothing -> return $ oldMD5 == newMD5
                Just _ -> do upToDate' <- upToDate dep
                             return $ (oldMD5 == newMD5) && upToDate')
          (\e -> return $ ioeGetErrorType e == InappropriateType)

-- | Calculate the MD5 checksum of a file.
--
-- for example
--
-- >>>fileMD5 "redo.hs"
-- "55ac4edcec77043674e3ec8e4a60b3b2"
fileMD5 :: FilePath   -- ^ the file to calculate the checksum of
        -> IO String  -- ^ a 32-character MS5 checksum
fileMD5 path = (show . MD5.md5) `liftM` BL.readFile path


-- | Write out the MD5 checksum of a given dependency of a target
writeMD5 :: FilePath -- ^ the target (file) for this dependency
         -> FilePath -- ^ the file path of the dependency
         -> IO ()
writeMD5 redoTarget dep = do
  md5 <- fileMD5 dep
  writeFile (metaDir </> redoTarget </> md5) dep

-- | Determin a file's size
fileSize :: FilePath   -- ^ the file to determin the size of
         -> IO Integer -- ^ the file's size in bytes
fileSize path = withFile path ReadMode hFileSize



