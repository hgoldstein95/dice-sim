{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dice where

import Control.Monad.Writer

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Data.Function.Memoize (memoFix)

import Text.Printf (printf)

class Monad m => World m where
  collapse :: (a -> b -> Bool) -> m a -> m b -> m (Either a b)
  collapseUniform :: (a -> a -> Bool) -> m a -> m a -> m a
  collapseUniform c x y = either id id <$> collapse c x y

instance Monoid w => World (Writer w) where
  collapse choice ma mb =
    let (a, wa) = runWriter ma
        (b, wb) = runWriter mb
    in writer $ if choice a b then (Left a, wa) else (Right b, wb)

search :: (World m) => (a -> a -> Bool) -> [m a] -> m a
search choice (x : xs) = foldl (collapseUniform choice) x xs
search _ [] = fail "search in empty list"

nextPow :: Int -> Int
nextPow n =
  let e = ceiling $ (log (fromIntegral n) / log 2 :: Double)
  in 2 ^ (e :: Integer)

minIndex :: Ord a => [a] -> Int
minIndex xs = fromJust . elemIndex (minimum xs) $ xs

frontHalf :: [a] -> [a]
frontHalf xs =
  let len = length xs
      amt = (len `quot` 2) + (len `mod` 2)
  in take amt xs

data FlipsResult = FR
  { frIn :: Int
  , frFlips :: Double
  , frPath :: [Int]
  }

instance Show FlipsResult where
  show (FR i f p) = printf "Num: %3d\tFlips: %.4f\tPath: %s" i f (show p)

findOptimalFlips :: Int -> FlipsResult
findOptimalFlips m = uncurry (FR m) . runWriter $ memoFix worker m
  where worker :: (Int -> Writer [Int] Double) -> Int -> Writer [Int] Double
        worker recCall n
          | n < 2 = return 0
          | n == 2 = tell [2] >> return 1
          | otherwise =
              let nums = [n + 1..nextPow n]
                  upTos = map (\(k, fm) -> liftM (* relWeight k) fm) .
                          mapPair recCall $ nums
                  divs = frontHalf . filter (\x -> n `mod` x == 0) $ [2..n - 1]
                  divBys = map (\(d, fm) -> liftM (+ getMEF d) fm) .
                           map (second recCall) .
                           mapPair (quot n) $ divs
              in tell [n] >> search (<) (upTos ++ divBys)
          where mapPair f = map (\x -> (x, f x))
                relWeight k = fromIntegral k / fromIntegral n
                getMEF d = fst . runWriter . recCall $ d

showFlipsTo :: Int -> [FlipsResult]
showFlipsTo n = take (n - 1) . map findOptimalFlips $ [2..]
