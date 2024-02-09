module Main (main) where

import Data.Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import EntriesAtom
import MediaInfo
import ParseOrg
import Data.Time
import System.Environment
import System.Exit
import System.IO

{-
1. read file passed by params
2. parse file and transform to local datatype (ParseOrg.hs)
3. get media data from local datatype and get media info from them (MediaInfo.hs)
4. pass it all to podcast RSS/Atom builder (Podcast.hs)
5. Either print to stdout or write to file.
6. ...
7. Profit!
-}
getMediaInfoForFile :: Entry -> IO (FilePath, (MIME, SHA1, Length))
getMediaInfoForFile (Entry {_mediaPath = mediaPath}) =
  do -- TODO mediaPath should be relative to feed, take into account
    Right mime <- getMimeForFile mediaPath -- TODO
    sha1 <- getSHA1ForFile mediaPath
    lengthBytes <- getLengthForFile mediaPath
    return (mediaPath, (mime, sha1, lengthBytes))

main :: IO ()
main = do
  programName <- getProgName
  args <- getArgs
  parseArgs programName args

processFile orgFile = do
  contents <- T.readFile orgFile
  now <- getCurrentTime
  let ( Right
          entries@( _meta,
                    entries' -- TODO horrible names
                    )
        ) = orgText2entries contents
  mediaFiles <- fromList <$> mapM getMediaInfoForFile entries'

  let feedM = renderFeed prettyConfig (FeedData mediaFiles entries now)
  case feedM of
    Just feed -> TL.putStrLn feed >> exitWith ExitSuccess
    Nothing -> die "Error"

parseArgs _ [orgFile] = processFile orgFile
-- parseArgs _ [orgfile, feedFile] = undefined
parseArgs progName _ = die $ "Usage: " ++ progName ++ " ORGFILE.org [FEED.xml]"
