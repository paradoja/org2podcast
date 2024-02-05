module Main (main) where

import Lib

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
main = someFunc
