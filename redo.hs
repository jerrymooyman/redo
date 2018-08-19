{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

import           Control.Exception    (IOException (..), catch, catchJust)
import           Control.Monad        (filterM, guard, liftM, unless)
import qualified Data.ByteString.Lazy as BL
import           Data.Digest.Pure.MD5 (md5)
import           Data.Map.Lazy        (adjust, fromList, insert, toList)
import           Data.Maybe           (listToMaybe)
--import           Data.Typeable        (typeOf)

import           Debug.Trace          (traceShow)
import           GHC.IO.Exception     (IOErrorType (..))
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       getDirectoryContents,
                                       removeDirectoryRecursive, removeFile,
                                       renameFile)
import           System.Environment   (getArgs, getEnvironment)
import           System.Exit          (ExitCode (..))
import           System.FilePath      (hasExtension, replaceBaseName,
                                       takeBaseName, (</>))
import           System.IO            (IOMode (..), hGetLine, hPutStrLn, stderr,
                                       withFile)
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)

import           System.Process       (CreateProcess (..), createProcess, shell,
                                       waitForProcess)

traceShow' :: Show b =>  b -> b
traceShow' arg = traceShow arg arg

main :: IO ()
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target metaDepsDir
  unless upToDate' $ maybe printMissing redo' =<< redoPath target
 where
    redo' :: FilePath -> IO ()
    redo' path = do
      catchJust (guard . isDoesNotExistError)
                (removeDirectoryRecursive metaDepsDir)
                (\_ -> return ())
      createDirectoryIfMissing True metaDepsDir
      writeFile (metaDepsDir </> path) =<< md5' path

      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
      exit <- waitForProcess ph
      case exit of
        ExitSuccess -> renameFile tmp target
        ExitFailure code -> do
          hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
          removeFile tmp
    tmp = target ++ "---redoing"
    metaDepsDir = ".redo" </> target
    printMissing = error $ "no .do file found for target '" ++ target ++ "'"
    cmd path = traceShow' $ unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]


redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: String -> FilePath -> IO Bool
upToDate target metaDepsDir = catch
  (do deps <- getDirectoryContents metaDepsDir
      (traceShow' . all id) `liftM` mapM depUpToDate deps)
  (\(_ :: IOException) -> return False)
    where depUpToDate :: FilePath -> IO Bool
          depUpToDate dep = catch
            (do oldMD5 <- withFile (metaDepsDir </> dep) ReadMode hGetLine
                newMD5 <- md5' dep
                return $ oldMD5 == show newMD5)
            (\e -> return $ ioeGetErrorType e == InappropriateType)

md5' :: FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path

