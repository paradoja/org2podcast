{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module ParseOrgSpec where

import qualified Data.Text as T
import Data.Time
import ParseOrg
import Test.Hspec

testMeta :: T.Text
testMeta =
  T.concat $
    fmap
      (<> "\n")
      [ "#+RSS_TITLE: A nice podcast title",
        "#+EMAIL: person@example.net",
        "#+DESCRIPTION: Podcast description",
        "#+FILE_LOCATION: file_path",
        "#+RSS_FEED_URL: https://podcast.example.net/feed.xml",
        "#+RSS_IMAGE_URL: https://podcast.example.net/icon.png",
        "#+RSS_FILES_URL: https://podcast.example.net/files/"
      ]

testContent :: T.Text
testContent =
  T.intercalate
    "\n"
    [ "* This is a header",
      ":PROPERTIES:",
      ":PUBDATE: <2024-02-06 Di 11:31>",
      ":MEDIA:   file.mp3",
      ":END:",
      "And some text",
      "",
      "* Another",
      ":PROPERTIES:",
      ":PUBDATE: <2023-02-06 Di 13:31>",
      ":MEDIA:   file2.mp3",
      ":END:",
      "Some text",
      "",
      "** Sub section",
      "In the subsection",
      "",
      "*** Sub sub section",
      "In the sub sub section"
    ]

myFirstDate, mySecondDate :: UTCTime
myFirstDate =
  UTCTime
    (fromGregorian 2024 2 6)
    (timeOfDayToTime (TimeOfDay 11 31 0))
mySecondDate =
  UTCTime
    (fromGregorian 2023 2 6)
    (timeOfDayToTime (TimeOfDay 13 31 0))

myMeta :: Meta
myEntries :: [Entry]
myFirstEntry, mySecondEntry :: Entry
Right (myMeta, myEntries) = orgText2entries $ testMeta <> testContent
myFirstEntry : mySecondEntry : _ = myEntries

spec :: Spec
spec = do
  describe "parsing metadata" $ do
    it "gets fields correctly" $ do
      myMeta
        `shouldBe` ( Meta
                       { podcastTitle = "A nice podcast title",
                         email = "person@example.net",
                         description = "Podcast description",
                         fileLocation = "file_path",
                         feedURL = "https://podcast.example.net/feed.xml",
                         imageURL = "https://podcast.example.net/icon.png",
                         filesURL = "https://podcast.example.net/files/"
                       }
                   )
  describe "parsing entries" $ do
    it "parses entryTitle correctly" $ do
      entryTitle myFirstEntry `shouldBe` "This is a header"
      entryTitle mySecondEntry `shouldBe` "Another"
    it "parses entryDate correctly" $ do
      entryDate myFirstEntry `shouldBe` myFirstDate
      entryDate mySecondEntry `shouldBe` mySecondDate
    it "parses mediaName correctly" $ do
      mediaName myFirstEntry `shouldBe` "file.mp3"
      mediaName mySecondEntry `shouldBe` "file2.mp3"
    it "parses body correctly" $ do
      body myFirstEntry `shouldBe` "<p>And some text</p>"
      body mySecondEntry
        `shouldSatisfy` ( \b ->
                            all
                              (`T.isInfixOf` b)
                              [ "Sub section</h2>",
                                -- we start headings from h2
                                "In the subsection",
                                "Sub sub section</h3>",
                                "In the sub sub section"
                              ]
                        )
