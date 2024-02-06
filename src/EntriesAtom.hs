{-# LANGUAGE OverloadedStrings #-}

-- | TODO
module EntriesAtom where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified ParseOrg as E
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)
import Text.Feed.Types
import Text.XML (def, rsPretty)
import Data.Time

renderFeed :: E.Entries -> Lazy.Text
renderFeed (meta, entries) =
  fromJust
    . Export.textFeedWith def {rsPretty = True}
    . AtomFeed
    $ (podcastFeed meta)
      { Atom.feedEntries = fmap (orgEntry2AtomEntry meta) entries
      }

podcastFeed :: E.Meta -> Atom.Feed
podcastFeed meta =
  -- (meta, _entries) =
  Atom.nullFeed
    (E.feedURL meta)
    (Atom.TextString (E.podcastTitle meta))
    "2017-08-01" -- TODO fix

orgEntry2AtomEntry :: E.Meta -> E.Entry -> Atom.Entry
orgEntry2AtomEntry
  (E.Meta {
      E.email = email
          })
  ( E.Entry
      { E.entryTitle = entryTitle,
        E.entryDate = entryDate,
        E.mediaName = mediaName,
        E.body = body
      }
    ) =
    ( Atom.nullEntry
        "urn:sha1:2e0f916f94d82a932de1ad33af65a9b5dde8089a" -- TODO The ID field. Must be a link to validate.
        (Atom.TextString entryTitle)
        (T.pack $ formatTime defaultTimeLocale rfc822DateFormat entryDate)
    )
      { Atom.entryAuthors = [Atom.nullPerson {Atom.personEmail = Just email}],
        Atom.entryContent = Just (Atom.HTMLContent body)
      }
