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
        `shouldReturn` (Right "text/plain")
  describe "getSHA1ForFile" $ do
    it "get SHA1 for a file" $ do
      getSHA1ForFile "test/testFile"
        `shouldReturn` "2e0f916f94d82a932de1ad33af65a9b5dde8089a"
