module Day01 where

import Prelude
import Global (readInt)
import Control.MonadZero (guard)
import Data.Array (tail, takeWhile, unsafeIndex)
import Data.Int (floor)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)

part1 :: String -> Int
part1 = vals >>> pairThatSumsTo2020 >>> \(x /\ y) -> x * y

lines :: String -> Array String
lines = split (Pattern "\n") >>> takeWhile ((/=) "")

toInt :: String -> Int
toInt = readInt 10 >>> floor

vals :: String -> Array Int
vals = lines >>> map toInt

pairThatSumsTo2020 :: Array Int -> Tuple Int Int
pairThatSumsTo2020 xs = unsafePartial $ (flip unsafeIndex) 0 do
  x <- xs
  y <- fromMaybe [] $ tail xs
  guard $ x + y == 2020
  pure (x /\ y)
