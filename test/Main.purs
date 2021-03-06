module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Day01 as Day01

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Day01.test
