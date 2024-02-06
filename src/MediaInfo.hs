-- | Gets the MIME file type of a file.
-- Uses POSIX *file*, in the PATH.

module MediaInfo (getMimeForFile) where

import System.Process
import qualified Data.Text as T
import System.Exit (ExitCode(ExitSuccess))

getMimeForFile :: FilePath -> IO (Either T.Text T.Text)
getMimeForFile file = do
  let stdin' = ""
  (errCode, stdout', stderr') <- readProcessWithExitCode "file" ["--mime-type", file] stdin'
  case errCode of
    ExitSuccess -> return . Right . T.pack . last . words $ stdout'
    _otherError -> return . Left . T.pack $ "Problem executing `file`: " ++ stderr'
