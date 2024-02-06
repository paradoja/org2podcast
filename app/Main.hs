module Main (main) where

import MediaInfo

import qualified Data.Text as T
import qualified Data.Text.IO as T

{-
1. read file passed by params
2. parse file and transform to local datatype (ParseOrg.hs)
3. get media data from local datatype and get media info from them (MediaInfo.hs)
4. pass it all to podcast RSS/Atom builder (Podcast.hs)
5. Either print to stdout or write to file.
6. ...
7. Profit!
-}
main :: IO ()
main = do
  mime <- getMimeForFile "LICENSE"
  case mime of
    Right mime' -> T.putStrLn mime'
    Left errorMsg -> T.putStrLn errorMsg
