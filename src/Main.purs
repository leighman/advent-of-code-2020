module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Day01 (part2)

main :: Effect Unit
main =
  launchAff_ do
    fileContents <- readTextFile ASCII "./src/Day01.txt"
    liftEffect $ logShow $ part2 fileContents
