{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module MediaInfoSpec where

import MediaInfo
import Test.Hspec

spec :: Spec
spec = do
  describe "getMimeForFile" $ do
    it "get MIME for a file" $ do
      getMimeForFile "test/testFile"
        `shouldReturn` Right "text/plain"
  describe "getSHA1ForFile" $ do
    it "get SHA1 for a file" $ do
      getSHA1ForFile "test/testFile"
        `shouldReturn` "b5647371118f7860fd69024b876e6814a996a8aa"
  describe "getLengthForFile" $ do
    it "get length (size in bytes) for a file" $ do
      getLengthForFile "test/testFile"
        `shouldReturn` 55
