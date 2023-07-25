{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Proc (
    CpuInfo (..),
    IRQ (..),
    Device,
    QueueType(..),
    getIRQAffinity,
    getInterrupts,
    getNumberOfIRQ,
    getInterruptsByDevice,
    getNumberOfProcessors,
    getIRQCounters,
    getNumberOfQueues,
    getNumberOfChannels,
    getCpuInfo,
    proc_irq,
    proc_interrupt,
    proc_cpuinfo,
)
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO.Unsafe ( unsafePerformIO )

import Data.List.Split (chunksOf)

import Mask ( readMask, cpusFromMask )
import Utils ( decimal, readsDecimal )

import Text.Regex.Posix ((=~))
import System.Directory ( listDirectory )
import Data.List ( isPrefixOf )

type Device = String

proc_interrupt, proc_cpuinfo, proc_irq :: String
proc_irq = "/proc/irq/"
proc_interrupt = "/proc/interrupts"
proc_cpuinfo = "/proc/cpuinfo"

data CpuInfo = CpuInfo
    { processor :: {-# UNPACK #-} !Int
    , physicalId :: {-# UNPACK #-} !Int
    , coreId :: {-# UNPACK #-} !Int
    }
    deriving stock (Show, Eq)

data IRQ = IRQ
    { number :: {-# UNPACK #-} !Int
    , description :: {-# UNPACK #-} !T.Text
    }
    deriving stock (Show, Eq)

data QueueType = Tx | Rx
    deriving stock (Eq)

instance Show QueueType where
    show Tx = "tx"
    show Rx = "rx"

{-# NOINLINE getIRQAffinity #-}
getIRQAffinity :: Int -> [Int]
getIRQAffinity irq =
    unsafePerformIO $
        cpusFromMask . readMask <$> readFile (proc_irq <> show irq <> "/smp_affinity")

{-# NOINLINE getInterrupts #-}
getInterrupts :: [IRQ]
getInterrupts =
    unsafePerformIO $
        T.readFile proc_interrupt >>= \file ->
            return $ map (\(n, d) -> IRQ n ((T.unwords . drop (3 + getNumberOfProcessors) . T.words) d)) (concatMap readsDecimal $ T.lines file :: [(Int, T.Text)])

{-# NOINLINE getInterruptsByDevice #-}
getInterruptsByDevice :: Device -> [(Int, T.Text, [Int])]
getInterruptsByDevice dev =
    unsafePerformIO $
        readFile proc_interrupt >>= \file -> do
            let irqLines = T.pack <$> filter (=~ dev) (lines file)
                irqColumns = T.words <$> irqLines
            return $ map (\cs -> ((decimal . T.takeWhile (/= ':')) (head cs), last cs, decimal <$> (take getNumberOfProcessors . tail) cs)) irqColumns

{-# NOINLINE getNumberOfProcessors #-}
getNumberOfProcessors :: Int
getNumberOfProcessors =
    unsafePerformIO $
        T.readFile proc_cpuinfo >>= \file ->
            return $ length . filter ("processor" `T.isInfixOf`) $ T.lines file

{-# NOINLINE getIRQCounters #-}
getIRQCounters :: Int -> [Int]
getIRQCounters irq =
    unsafePerformIO $
        T.readFile proc_interrupt >>= \file -> do
            let dev = T.pack $ show irq <> ":"
                irqCol = decimal <$> (take getNumberOfProcessors . tail $ concatMap T.words $ filter ((== dev) . head . T.words) (T.lines file)) :: [Int]
            return irqCol

{-# NOINLINE getCpuInfo #-}
getCpuInfo :: [CpuInfo]
getCpuInfo =
    unsafePerformIO $
        T.readFile proc_cpuinfo >>= \file -> do
            let cpuLines = T.lines file
                cpuInfo =
                    chunksOf 3 $
                        decimal . T.drop 2 . T.dropWhile (/= ':')
                            <$> filter (\l -> "processor" `T.isPrefixOf` l || "physical id" `T.isPrefixOf` l || "core id" `T.isPrefixOf` l) cpuLines
            return $
                map
                    ( \case
                        [a, b, c] -> CpuInfo a b c
                        _ -> error "cpuinfo: parse error"
                    )
                    cpuInfo

{-# NOINLINE getNumberOfQueues #-}
getNumberOfQueues :: Device -> QueueType -> Int
getNumberOfQueues dev qt = unsafePerformIO $ do
     ls <- listDirectory $ "/sys/class/net/" <> dev <> "/queues/"
     return $ length $ filter (show qt `isPrefixOf`) ls

{-# NOINLINE getNumberOfIRQ #-}
getNumberOfIRQ :: Device -> Int
getNumberOfIRQ dev =
    unsafePerformIO $
        readFile proc_interrupt >>= \file ->
            return $ length $ filter (=~ dev) $ lines file

getNumberOfChannels :: Device -> QueueType -> Int
getNumberOfChannels dev qt | ':' `elem` dev = getNumberOfIRQ dev
                           | otherwise = getNumberOfQueues dev qt