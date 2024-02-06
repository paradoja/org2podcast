{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module MediaInfoSpec where

import Test.Hspec
import MediaInfo

spec :: Spec
spec = do
  describe "getMimeForFile" $ do
    it "get MIME for a file" $ do
      getMimeForFile "LICENSE" `shouldReturn` (Right "text/plain")
