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

part2 :: String -> Int
part2 = vals >>> tripleThatSumsTo2020 >>> \(x /\ y /\ z) -> x * y * z

lines :: String -> Array String
lines = split (Pattern "\n") >>> takeWhile ((/=) "")

toInt :: String -> Int
toInt = readInt 10 >>> floor

vals :: String -> Array Int
vals = lines >>> map toInt

guaranteedFirst :: forall a. Array a -> a
guaranteedFirst xs = unsafePartial $ unsafeIndex xs 0

guaranteedTail :: forall a. Array a -> Array a
guaranteedTail = tail >>> fromMaybe []

pairThatSumsTo2020 :: Array Int -> Tuple Int Int
pairThatSumsTo2020 xs = guaranteedFirst do
  x <- xs
  y <- ys
  guard $ x + y == 2020
  pure (x /\ y)
  where
    ys = guaranteedTail xs

tripleThatSumsTo2020 :: Array Int -> Tuple Int (Tuple Int Int)
tripleThatSumsTo2020 xs = guaranteedFirst do
  x <- xs
  y <- ys
  z <- zs
  guard $ x + y + z == 2020
  pure (x /\ y /\ z)
  where
    ys = guaranteedTail xs
    zs = guaranteedTail ys
