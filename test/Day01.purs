module Test.Day01 where

import Prelude
import Day01 (part1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

test :: Spec Unit
test = do
  describe "part 1" do
    it "should multiply the two values that sum to 2020"
      $ part1 "1721\n979\n366\n299\n675\n1456\n" `shouldEqual` 514579
