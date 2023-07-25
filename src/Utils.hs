{-# LANGUAGE OverloadedStrings #-}

module Utils (
    Utils.decimal,
    readsDecimal,
    intersperseEvery,
) where

import Data.Char (isSpace)
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.Read as T

{-# INLINE decimal #-}
decimal :: T.Text -> Int
decimal = fst . fromRight (-1, "") . T.decimal

{-# INLINE readsDecimal #-}
readsDecimal :: (Integral a) => T.Text -> [(a, T.Text)]
readsDecimal t = case fromRight (-1, t) ((T.decimal . T.dropWhile isSpace) t) of
    (-1, _) -> []
    r -> [r]

intersperseEvery :: Int -> t -> [t] -> [t]
intersperseEvery n x xs = zip xs [1 .. l] >>= ins
  where
    ins (x', k) = if k `mod` n == 0 && k /= l then [x', x] else [x']
    l = length xs
