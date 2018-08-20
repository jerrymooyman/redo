{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

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
import           System.Exit          (ExitCode (..))
import           System.FilePath      (hasExtension, replaceBaseName,
                                       splitFileName, takeBaseName, (</>))
import           System.IO            (IOMode (..), hFileSize, hGetLine,
                                       hPutStrLn, stderr, withFile)
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)

import           System.Process       (CreateProcess (..), createProcess, shell,
                                       waitForProcess)

traceShow' :: Show b =>  b -> b
traceShow' arg = traceShow arg arg

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

redo :: String -> FilePath -> IO ()
redo target dir = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe missingDo redo' =<< doPath target
  where
      redo' :: FilePath -> IO ()
      redo' path = do
        hPutStrLn stderr $ "redo " ++ show (dir </> target)
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
      tmp = target ++ "---redoing"
      metaDepsDir = metaDir </> target
      missingDo = do
        exists <- doesFileExist target
        unless exists $ error $ "no .do file found for target '" ++ target ++ "'"
      cmd path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]


doPath :: FilePath -> IO (Maybe FilePath)
doPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
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

fileMD5 :: FilePath -> IO String
fileMD5 path = (show . MD5.md5) `liftM` BL.readFile path


writeMD5 redoTarget dep = do
  md5 <- fileMD5 dep
  writeFile (metaDir </> redoTarget </> md5) dep

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize



