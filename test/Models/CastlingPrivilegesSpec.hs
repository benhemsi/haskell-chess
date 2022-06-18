module Models.CastlingPrivilegesSpec where

import Models.CastlingPrivileges
import Test.Hspec
import Test.Hspec.QuickCheck

spec = do
  describe "read" $ do prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: CastlingPrivileges)
