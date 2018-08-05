import Control.Monad (filterM)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)

main :: IO ()
main = do
  args <- getArgs 
  mapM_ redo args

redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  path <- redoPath target
  (_, _, _, ph) <- createProcess $ shell $ "sh " ++ path ++ " - - " ++ tmp ++ " > " ++ tmp
  exit <- waitForProcess ph
  case exit of
    ExitSuccess -> do renameFile tmp target
    ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                           removeFile tmp

redoPath :: FilePath -> IO FilePath
redoPath target = do
  existingCandidates <- filterM doesFileExist candidates
  return $ head existingCandidates
  where candidates = [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []