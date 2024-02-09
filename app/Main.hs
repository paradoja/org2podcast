module Main (main) where

import Data.Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import EntriesAtom
import MediaInfo
import ParseOrg
import System.FilePath
import Data.Time

{-
1. read file passed by params
2. parse file and transform to local datatype (ParseOrg.hs)
3. get media data from local datatype and get media info from them (MediaInfo.hs)
4. pass it all to podcast RSS/Atom builder (Podcast.hs)
5. Either print to stdout or write to file.
6. ...
7. Profit!
-}
getMediaInfoForFile :: FilePath -> Entry -> IO (FilePath, (MIME, SHA1, Length))
getMediaInfoForFile absPath (Entry {_mediaPath = mediaPath}) =
  do
    let fullPath = absPath </> mediaPath
    Right mime <- getMimeForFile fullPath -- TODO
    sha1 <- getSHA1ForFile fullPath
    lengthBytes <- getLengthForFile fullPath
    return (mediaPath, (mime, sha1, lengthBytes))

main :: IO ()
main = do
  contents <- T.readFile "feed.org"
  now <- getCurrentTime
  let ( Right
          entries@( _meta,
                    entries' -- TODO horrible names
                    )
        ) = orgText2entries contents
  mediaDataList <-
    mapM
      (getMediaInfoForFile "/home/paradoja/projects/podcast/") -- TODO Remove
      entries'
  let mediaFiles = fromList mediaDataList

  TL.putStrLn . renderFeed prettyConfig $
    FeedData mediaFiles entries now
