{-# LANGUAGE ViewPatterns #-}

-- | Gets file data
-- Uses POSIX *file*, in the PATH, to get the MIME type.
module MediaInfo (getMimeForFile, getSHA1ForFile, getLengthForFile) where

import Crypto.Hash ( SHA1(SHA1), hashWith )
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Directory ( getFileSize )
import System.Exit (ExitCode (ExitSuccess))
import System.Process ( readProcessWithExitCode )

getMimeForFile :: FilePath -> IO (Either T.Text T.Text)
getMimeForFile file = do
  let stdin' = ""
  (errCode, stdout', stderr') <- readProcessWithExitCode "file" ["--mime-type", file] stdin'
  case errCode of
    ExitSuccess -> return . processOutput . words $ stdout'
    _otherError -> return . Left . T.pack $ "Problem executing `file`: " ++ stderr'
  where
    processOutput [] = Left . T.pack $ "Problem: executing `file`: No MIME returned."
    processOutput (last -> mime) = Right . T.pack $ mime

getSHA1ForFile :: FilePath -> IO T.Text
getSHA1ForFile file = do
  fileContent <- B.readFile file
  return $ T.pack . show $ hashWith SHA1 fileContent

getLengthForFile :: FilePath -> IO Integer
getLengthForFile = getFileSize
