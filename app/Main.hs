module Main where

import Dice

import Data.List (intercalate)

main :: IO ()
main =
  writeFile "out.txt" . (++ "\n") . intercalate "\n" . map show $
  showFlipsTo 2000
