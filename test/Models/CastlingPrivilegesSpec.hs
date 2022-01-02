{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Models.CastlingPrivilegesSpec where

import Models.CastlingPrivileges
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

makeBool :: Gen Bool
makeBool = chooseEnum (True, False)

instance Arbitrary CastlingPrivileges where
  arbitrary = do
    wk <- makeBool
    wq <- makeBool
    bk <- makeBool
    bq <- makeBool
    return (CastlingPrivileges wk wq bk bq)

-- spec = do
--   describe "read" $ do
--     it "is inverse to show" $
--       verboseCheck (\x -> (read . show) x `shouldBe` (x :: CastlingPrivileges))

--     it "throw an error for anything else" $ do
--       let result = readMaybe "wb" :: Maybe CastlingPrivileges
--       result `shouldBe` Nothing
