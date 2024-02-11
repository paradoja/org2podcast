{-# LANGUAGE OverloadedStrings #-}

module EntriesAtom
  ( FeedData (..),
    FeedConfig (..),
    MIME,
    SHA1,
    Length,
    MediaData,
    renderFeed,
    defaultConfig,
    prettyConfig,
  )
where

import Data.Functor ((<&>)) -- flipped fmap
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified ParseOrg as E
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)
import Text.Feed.Types (Feed (AtomFeed))
import Text.XML (def, rsPretty)

type MIME = T.Text

type SHA1 = T.Text

type Length = Integer

type MediaData = M.Map FilePath (MIME, SHA1, Length)

data FeedData = FeedData
  { _mediaData :: !MediaData,
    _entries :: !E.Entries,
    _updatedAt :: !UTCTime
  }
  deriving (Show, Eq)

newtype FeedConfig = FeedConfig
  { prettyPrint :: Bool
  }

defaultConfig, prettyConfig :: FeedConfig
defaultConfig = FeedConfig False
prettyConfig = FeedConfig True

renderFeed :: FeedConfig -> FeedData -> Maybe Lazy.Text
renderFeed
  (FeedConfig pretty)
  (FeedData mediaData (meta, entries) updated) =
    mapM (orgEntry2AtomEntry mediaData meta) entries
      >>= \entries' ->
        Export.textFeedWith def {rsPretty = pretty}
          . AtomFeed
          $ (podcastFeed updated meta)
            { Atom.feedEntries = entries'
            }

podcastFeed :: UTCTime -> E.Meta -> Atom.Feed
podcastFeed updated meta =
  ( Atom.nullFeed
      (E._feedURL meta)
      (Atom.TextString (E._podcastTitle meta))
      (T.pack $ iso8601Show updated)
  )
    { Atom.feedSubtitle = Just (Atom.TextString $ E._description meta),
      Atom.feedIcon = Just (E._imageURL meta)
    }

orgEntry2AtomEntry :: MediaData -> E.Meta -> E.Entry -> Maybe Atom.Entry
orgEntry2AtomEntry
  mediaData
  ( E.Meta
      { E._email = email,
        E._filesURL = filesURL
      }
    )
  ( E.Entry
      { E._entryTitle = entryTitle,
        E._entryDate = entryDate,
        E._mediaPath = mediaPath,
        E._body = body
      }
    ) =
    M.lookup mediaPath mediaData
      <&> ( \(mime, sha1, lengthBytes) ->
              ( Atom.nullEntry
                  ("urn:sha1:" <> sha1)
                  (Atom.TextString entryTitle)
                  (T.pack $ iso8601Show entryDate)
              )
                { Atom.entryAuthors = [Atom.nullPerson {Atom.personEmail = Just email}],
                  Atom.entryContent = Just (Atom.HTMLContent body),
                  Atom.entryLinks =
                    [ (Atom.nullLink $ filesURL <> T.pack mediaPath)
                        { Atom.linkRel = Just (Right "enclosure"),
                          Atom.linkType = Just mime,
                          Atom.linkLength = Just . T.pack $ show lengthBytes
                        }
                    ]
                }
          )
