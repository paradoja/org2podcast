{-# LANGUAGE OverloadedStrings #-}
module TestEntries (makeTime, entries, mediaInfo, lastUpdate) where

import Data.Fixed (Pico)
import qualified Data.Map as M
import Data.Time
import EntriesAtom
import ParseOrg

makeTime ::
  (Integer, Int, Int) ->
  (Int, Int, Pico) ->
  UTCTime
makeTime (year, month, day) (hour, minutes, seconds) =
  UTCTime
    (fromGregorian year month day)
    (timeOfDayToTime (TimeOfDay hour minutes seconds))

entries :: Entries
entries =
  ( Meta
      { _podcastTitle = "RSS title",
        _description = "Nice podcast description",
        _email = "me@example.net",
        _fileLocation = "testFiles",
        _feedURL = "https://podcast.example.net/feed.xml",
        _imageURL = "https://podcast.example.net/icon.png",
        _filesURL = "https://podcast.example.net/"
      },
    [ Entry
        { _entryTitle = "Title of a nice Podcast entry",
          _entryDate = makeTime (2024, 2, 3) (11, 31, 0),
          _mediaPath = "testFiles/t1",
          _body = "<p>Some description to be shown.</p>"
        },
      Entry
        { _entryTitle = "Another podcast episode",
          _entryDate = makeTime (2024, 2, 1) (7, 0, 0),
          _mediaPath = "testFiles/t2.xml",
          _body = "<p>This is another description.</p><h2 id=\"org558c12\">Subsection</h2><p>Something else</p>"
        }
    ]
  )

mediaInfo :: M.Map FilePath (MIME, SHA1, Length)
mediaInfo =
  M.fromList
    [ ("testFiles/t1", ("text/plain", "b75e2b1778aa832483ef2190cb0b4b68a8eb072a", 30)),
      ("testFiles/t2.xml", ("text/xml", "8f0e917d605352cbb150315c8291f6a356e35f1a", 90))
    ]

lastUpdate :: UTCTime
lastUpdate = makeTime (2024, 1, 1) (10, 2, 30)
