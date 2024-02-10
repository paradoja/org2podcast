{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module EntriesAtomSpec where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import EntriesAtom
import Test.Hspec
import TestEntries


spec :: Spec
spec = do
    it "with pretty printing config" $ do
      (Just . TL.strip <$> TL.readFile "test/testFeedPretty.xml")
        `shouldReturn` ( TL.strip
                           <$> renderFeed
                             prettyConfig
                             (FeedData mediaInfo entries lastUpdate)
                       )
    it "with default config" $ do
      (Just . TL.strip <$> TL.readFile "test/testFeedClean.xml")
        `shouldReturn` renderFeed
          defaultConfig
          (FeedData mediaInfo entries lastUpdate)
