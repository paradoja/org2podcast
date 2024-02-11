module Main (main) where

import Data.Map (fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time (getCurrentTime)
import EntriesAtom
import MediaInfo
import Options.Applicative
import ParseOrg
import System.Environment
import System.Exit

data Org2FeedConfig = Org2FeedConfig
  { _orgFile :: !FilePath,
    _feedFile :: !(Maybe FilePath),
    _pretty :: !Bool
  }

main :: IO ()
main = do
  programName <- getProgName
  let opts =
        info
          (org2feedArgs <**> helper)
          ( fullDesc
              <> progDesc "Transforms an org file into a podcast atom feed"
              <> header programName
          )
  processFile =<< execParser opts

org2feedArgs :: Parser Org2FeedConfig
org2feedArgs =
  Org2FeedConfig
    <$> argument
      str
      ( metavar "ORGFILE"
          <> help "Source org file"
      )
    <*> optional
      ( argument
          str
          ( metavar "FEEDFILE"
              <> help "Dest. atom feed file; `stdout` used if not given"
          )
      )
    <*> switch
      ( long "pretty"
          <> short 'p'
          <> help "Pretty print (with lots of whitespace)"
      )

processFile :: Org2FeedConfig -> IO ()
processFile (Org2FeedConfig orgFile maybeFeedFile pretty) = do
  contents <- T.readFile orgFile
  now <- getCurrentTime
  case orgText2entries contents of
    Left errorMsg -> die $ T.unpack errorMsg
    Right entries@(_meta, entries') -> do
      mediaFiles <- fromList <$> mapM getMediaInfoForFile entries'
      let maybeFeed = renderFeed (feedConfig pretty) (FeedData mediaFiles entries now)
      case maybeFeed of
        Just feed -> outProc maybeFeedFile feed >> exitSuccess
        Nothing -> die "Error"

feedConfig :: Bool -> FeedConfig
feedConfig pretty = if pretty then prettyConfig else defaultConfig

outProc :: Maybe FilePath -> TL.Text -> IO ()
outProc Nothing = TL.putStrLn
outProc (Just feedFile) = TL.writeFile feedFile

getMediaInfoForFile :: Entry -> IO (FilePath, (MIME, SHA1, Length))
getMediaInfoForFile (Entry {_mediaPath = mediaPath}) =
  do
    -- TODO mediaPath should be relative to feed, take into account
    Right mime <- getMimeForFile mediaPath -- TODO
    sha1 <- getSHA1ForFile mediaPath
    lengthBytes <- getLengthForFile mediaPath
    return (mediaPath, (mime, sha1, lengthBytes))
