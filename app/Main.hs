{-# LANGUAGE ApplicativeDo #-}
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
import System.Directory (makeAbsolute)
import System.Environment (getProgName)
import System.Exit (die, exitSuccess)
import System.FilePath (takeDirectory, (</>))

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
  basePath <- takeDirectory <$> makeAbsolute orgFile
  contents <- T.readFile orgFile
  now <- getCurrentTime
  let entriesEither = orgText2entries contents
  mediaFilesTuplesEither' <- either (return . Left) (getMediaMap basePath) entriesEither
  let feedEither = do
        -- Either
        entries <- entriesEither
        mediaFiles <- fmap fromList mediaFilesTuplesEither'
        maybe (Left $ T.pack "Error rendering feed") Right $
          renderFeed (feedConfig pretty) (FeedData mediaFiles entries now)
  case feedEither of
    Right feed -> outProc maybeFeedFile feed >> exitSuccess
    Left msg -> die $ T.unpack msg
  where
    getMediaMap :: FilePath -> Entries -> IO (Either T.Text [(FilePath, (MIME, SHA1, Length))])
    getMediaMap basePath (_meta, individualPosts) =
      sequence <$> mapM (getMediaInfoForFile basePath) individualPosts

feedConfig :: Bool -> FeedConfig
feedConfig pretty = if pretty then prettyConfig else defaultConfig

outProc :: Maybe FilePath -> TL.Text -> IO ()
outProc Nothing = TL.putStrLn
outProc (Just feedFile) = TL.writeFile feedFile

getMediaInfoForFile :: FilePath -> Entry -> IO (Either T.Text (FilePath, (MIME, SHA1, Length)))
getMediaInfoForFile basePath (Entry {_mediaPath = mediaPath}) =
  do
    let fullPath = basePath </> mediaPath
    sha1 <- getSHA1ForFile fullPath
    lengthBytes <- getLengthForFile fullPath
    mimeEither <- getMimeForFile fullPath
    return $ fmap (\mime -> (mediaPath, (mime, sha1, lengthBytes))) mimeEither
