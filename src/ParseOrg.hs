{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description XXX TODO yes, we redo a lot of the transforming
-- I tried using org-mode date parsing but it's shit
module ParseOrg (Entries, Meta (..), Entry (..), text2entries) where

import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybe, fromMaybe)
import Data.Org
import qualified Data.Org.Lucid as OL
import Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Time
import Data.Void (Void)
import Lucid
import Text.Megaparsec
import Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void T.Text -- for parsing the datetime

data Entry = Entry
  { entryTitle :: !Text,
    entryDate :: !UTCTime,
    mediaName :: !Text,
    body :: !Text
  }
  deriving (Eq, Show)

data Meta = Meta
  { title :: !Text,
    description :: !Text,
    email :: !Text,
    fileLocation :: !Text, -- should be a Path
    feedURL :: !Text,
    imageURL :: !Text,
    filesURL :: !Text
  }
  deriving (Eq, Show)

type Entries = (Meta, [Entry])

text2entries :: Text -> Entries
text2entries = org2entries . fromJust . org

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

org2entries :: OrgFile -> Entries
org2entries
  ( OrgFile
      { orgMeta = oMeta,
        orgDoc =
          OrgDoc
            { docBlocks = _, -- TODO ??
              docSections = sections
            }
      }
    ) =
    let meta = fromJust $ map2Meta oMeta -- TODO fromJust?
     in (meta, fmap mainSection2Entry sections)

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

mainSection2Entry :: Section -> Entry
mainSection2Entry
  ( Section
      { sectionHeading = hHead :| hRest,
        sectionProps = properties,
        sectionDoc =
          orgDoc@OrgDoc
            { docBlocks = innerBlocks,
              docSections = innerSections
            }
      }
    ) =
    let media = fromJust $ M.lookup "MEDIA" properties
        pubDate = fromJust $ M.lookup "PUBDATE" properties
     in Entry
          { entryTitle = wordList2Text (hHead : hRest),
            entryDate =
              let maybeStamp = parseMaybe timestampParser $ pubDate
               in fromJust maybeStamp,
            mediaName = media,
            body =
              toStrict . renderText $ OL.body renderingOptions (OrgFile {orgMeta = M.empty, orgDoc = orgDoc})
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
words2Text (Punct char) = singleton char
words2Text (Plain text) = text

-- Timestampr parser

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
