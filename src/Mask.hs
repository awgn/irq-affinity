module Mask (
    CpuMask,
    showMask,
    readMask,
    makeCpuMask,
    cpusFromMask,
) where

import Data.Bits ((.|.), shiftL, (.&.))
import Numeric (readHex)
import Numeric.Extra (showHex)
import Utils ( intersperseEvery )

type CpuMask = Integer

{-# INLINE showMask #-}
showMask :: CpuMask -> String
showMask mask' = reverse . intersperseEvery 8 ',' . reverse $ showHex mask' ""

{-# INLINE readMask #-}
readMask :: String -> CpuMask
readMask = fst . head . readHex . filter (/= ',')

{-# INLINE makeCpuMask #-}
makeCpuMask :: [Int] -> CpuMask
makeCpuMask = foldr (\cpu mask' -> mask' .|. (1 `shiftL` cpu)) (0 :: CpuMask)

{-# INLINE cpusFromMask #-}
cpusFromMask :: CpuMask -> [Int]
cpusFromMask mask' = [n | n <- [0 .. 4095], let p2 = 1 `shiftL` n, mask' .&. p2 /= 0]
