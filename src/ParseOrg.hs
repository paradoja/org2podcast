{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parses a given org-mode format Text to the included Entries datatatype.
-- 'Entries' have general information on the podcast feed, in a 'Meta', and a
-- list of 'Entry', which includes titles, dates, name of the media file and
-- some body.
--
-- For the format of the org-mode file, check the Readme. For the parsing inside
-- each heading the Lucid and org-mode-lucid libraries are used. Due to this,
-- starting headers start at @h2@ onwards. Also, for similar reasons, titles are
-- converted by hand.
module ParseOrg (Entries, Meta (..), Entry (..), orgText2entries) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Org
    ( OrgFile(..),
      OrgDoc(OrgDoc, docSections, docBlocks),
      Section(Section, sectionDoc, sectionHeading, sectionProps),
      Words(..),
      URL(URL),
      org )
import qualified Data.Org.Lucid as OL
import Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Time
import Data.Void (Void)
import Lucid (renderText)
import Text.Megaparsec ( Parsec, optional, parseMaybe, many )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Entry = Entry
  { entryTitle :: !Text,
    entryDate :: !UTCTime,
    mediaName :: !Text,
    body :: !Text
  }
  deriving (Eq, Show)

data Meta = Meta
  { podcastTitle :: !Text,
    description :: !Text,
    email :: !Text,
    fileLocation :: !Text, -- should be a Path
    feedURL :: !Text,
    imageURL :: !Text,
    filesURL :: !Text
  }
  deriving (Eq, Show)

type Entries = (Meta, [Entry])

type Parser = Parsec Void T.Text -- for parsing the datetime

orgText2entries :: Text -> Either Text Entries
orgText2entries text =
  case org text of
    Just parsedOrgFile -> org2entries parsedOrgFile
    Nothing -> Left "Org parsing failing"

org2entries :: OrgFile -> Either Text Entries
org2entries
  ( OrgFile
      { orgMeta = oMeta,
        orgDoc =
          OrgDoc
            { docBlocks = _, -- Not using this for now; could be podcast desc.
              docSections = sections
            }
      }
    ) = do
    meta <- maybe (Left "Missing meta descriptions in file") Right $ map2Meta oMeta
    entries <- mapM mainSection2Entry sections
    return (meta, entries)

map2Meta :: M.Map Text Text -> Maybe Meta
map2Meta metaMap = do
  description' <- M.lookup "DESCRIPTION" metaMap
  email' <- M.lookup "EMAIL" metaMap
  fileLocation' <- M.lookup "FILE_LOCATION" metaMap
  feedURL' <- M.lookup "RSS_FEED_URL" metaMap
  imageURL' <- M.lookup "RSS_IMAGE_URL" metaMap
  filesURL' <- M.lookup "RSS_FILES_URL" metaMap
  title' <- M.lookup "RSS_TITLE" metaMap
  return $
    Meta title' description' email' fileLocation' feedURL' imageURL' filesURL'

mainSection2Entry :: Section -> Either Text Entry
mainSection2Entry
  ( Section
      { sectionHeading = hHead :| hRest,
        sectionProps = properties,
        sectionDoc = orgDocument
      }
    ) = do
    let title = wordList2Text (hHead : hRest)
        errmsg msg = Left $ "Entry '" <>  title <> "': " <> msg
    media <- maybe (errmsg "Missing MEDIA prop") Right $ M.lookup "MEDIA" properties
    pubDate <- maybe (errmsg "Missing PUBDATE prop") Right $ M.lookup "PUBDATE" properties
    date <- maybe (errmsg "Incorrect date") Right $ parseMaybe timestampParser pubDate
    return $
      Entry
        { entryTitle = title,
          entryDate = date,
          mediaName = media,
          body =
            toStrict . renderText $
              OL.body
                renderingOptions
                ( OrgFile
                    { orgMeta = M.empty,
                      orgDoc = orgDocument
                    }
                )
        }

wordList2Text :: [Words] -> Text
wordList2Text = T.intercalate " " . fmap words2Text

words2Text :: Words -> Text
words2Text (Bold text) = "<b>" <> text <> "</b>"
words2Text (Italic text) = "<i>" <> text <> "</i>"
words2Text (Highlight text) = "<span style='text-decoration: underline;'>" <> text <> "</span>"
words2Text (Underline text) = "<span style='text-decoration: underline;'>" <> text <> "</span>"
words2Text (Verbatim text) = "<pre>" <> text <> "</pre>"
words2Text (Strike text) = "<s>" <> text <> "</s>"
words2Text (Link (URL url) (Just text)) = "<a href='" <> url <> "'>" <> text <> "</a>"
words2Text (Link (URL url) Nothing) = "<a href='" <> url <> "'>" <> url <> "</a>"
words2Text (Image (URL url)) = "<img src='" <> url <> "'>"
words2Text (Punct c) = singleton c
words2Text (Plain text) = text

renderingOptions :: OL.OrgStyle
renderingOptions =
  OL.OrgStyle
    { OL.includeTitle = False,
      OL.tableOfContents = OL.TOC 0,
      OL.bootstrap = False,
      OL.bulma = False,
      OL.highlighting = OL.codeHTML,
      OL.sectionStyling = \_ a b -> a >> b,
      OL.separator = Just ' '
    }

-- Timestamp parser

timestampParser :: Parser UTCTime
timestampParser = do
  char '<'
  day <- dateParser
  space1
  many letterChar
  space
  time <- optional timeParser
  char '>'
  return $ UTCTime day (timeOfDayToTime (fromMaybe (TimeOfDay 0 0 0) time))

dateParser :: Parser Day
dateParser = fromGregorian <$> decimal <*> (char '-' *> decimal) <*> (char '-' *> decimal)

timeParser :: Parser TimeOfDay
timeParser = do
  hour <- decimal
  char ':'
  minutes <- decimal
  seconds <- optional (char ':' >> decimal)
  return $ TimeOfDay hour minutes (fromMaybe 0 seconds)
