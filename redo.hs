{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception  (IOException (..), catch)
import           Control.Monad      (filterM, liftM, unless)
import           Data.Map.Lazy      (adjust, fromList, insert, toList)
import           Data.Maybe         (listToMaybe)
import           Data.Typeable      (typeOf)
import           GHC.IO.Exception   (IOErrorType (..))
import           System.Directory   (doesFileExist, getDirectoryContents,
                                     removeFile, renameFile)
import           System.Environment (getArgs, getEnvironment)
import           System.Exit        (ExitCode (..))
import           System.FilePath    (hasExtension, replaceBaseName,
                                     takeBaseName, (</>))
import           System.IO          (hPutStrLn, stderr)
import           System.IO.Error    (ioeGetErrorType)

import           System.Process     (CreateProcess (..), createProcess, shell,
                                     waitForProcess)

main :: IO ()
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe printMissing redo' =<< redoPath target
 where
    redo' :: FilePath -> IO ()
    redo' path = do
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
    printMissing = error $ "no .do file found for target '" ++ target ++ "'"
    cmd path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]


redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: String -> IO Bool
upToDate target = catch
  (do deps <- getDirectoryContents depDir
      all id `liftM` mapM depUpToDate deps)
  (\(_ :: IOException) -> return False)
    where depDir = ".redo/" </> target
          depUpToDate :: FilePath -> IO Bool
          depUpToDate dep = catch
            (do oldMD5 <- readFile $ depDir </> dep
                return False)
            (\e -> return $ ioeGetErrorType e == InappropriateType)

