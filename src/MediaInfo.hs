-- | Gets the MIME file type of a file.
-- Uses POSIX *file*, in the PATH.
module MediaInfo (getMimeForFile, getSHA1ForFile) where

import Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process

getMimeForFile :: FilePath -> IO (Either T.Text T.Text)
getMimeForFile file = do
  let stdin' = ""
  (errCode, stdout', stderr') <- readProcessWithExitCode "file" ["--mime-type", file] stdin'
  case errCode of
    ExitSuccess -> return . Right . T.pack . last . words $ stdout'
    _otherError -> return . Left . T.pack $ "Problem executing `file`: " ++ stderr'

getSHA1ForFile :: FilePath -> IO T.Text
getSHA1ForFile file = do
  fileContent <- B.readFile file
  return $ T.pack . show $ hashWith SHA1 fileContent
