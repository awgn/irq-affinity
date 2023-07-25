{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Copyright (c) 2015-2023 Nicola Bonelli <nicola@pfq.io>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

module Main (module Main) where

import Control.Exception (ErrorCall (ErrorCall), handle)
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    evalStateT,
    forM,
    forM_,
    unless,
    when,
 )
import Data.List (nub, sort)
import Data.List.Split (splitOneOf)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple.Extra (fst3)
import Paths_affinity qualified as P
import System.Console.ANSI (
    Color (Blue, Green, Red),
    ColorIntensity (Vivid),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (SetColor, SetConsoleIntensity),
    setSGRCode,
 )
import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import Data.Version
import Options.Applicative

import Mask
import Options
import Proc

bold, red, blue, green, reset :: T.Text
bold = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
red = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
blue = T.pack $ setSGRCode [SetColor Foreground Vivid Blue]
green = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
reset = T.pack $ setSGRCode []

putStrBoldLn :: T.Text -> IO ()
putStrBoldLn msg = T.putStrLn $ bold <> msg <> reset

-- Command line options
--

type BindIO = StateT (Options, Maybe Int) IO

data Binding = Binding
    { bStart :: Maybe Int
    , bRange :: (Int, Int)
    , bStep :: Int
    , bMulti :: Int
    , bFilter :: Int -> Bool
    }

instance Show Binding where
    show Binding{..} = "Binding { start:" <>  show bStart <> " range:" <> show bRange <> " step:" <> show bStep <> " multi:" <> show bMulti <> " }"

{-# INLINE (&&&) #-}
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g x = f x && g x

phyFilter :: Maybe Int -> Int -> Bool
phyFilter Nothing _ = True
phyFilter (Just p) x = physicalId (getCpuInfo !! (x `mod` length getCpuInfo)) == p

makeBinding :: [String] -> Maybe Int -> (Int, Int) -> Maybe Int -> Binding
makeBinding ["basic"] first r phy = Binding first r 1 1 (phyFilter phy)
makeBinding ["round-robin"] _ r phy = Binding Nothing r 1 1 (phyFilter phy)
makeBinding ["multiple", m] _ r phy = Binding Nothing r 1 (read m) (phyFilter phy)
makeBinding ["raster", m] first r phy = Binding first r 1 (read m) (phyFilter phy)
makeBinding ["even"] first r phy = Binding first r 1 1 (phyFilter phy &&& even)
makeBinding ["odd"] first r phy = Binding first r 1 1 (phyFilter phy &&& odd)
makeBinding ["any"] _ r phy = Binding (Just 0) r 1 1 (phyFilter phy)
makeBinding ["zero"] _ r _ = Binding (Just 0) r 1 1 (const False)
makeBinding ["step", s] first r phy = Binding first r (read s) 1 (phyFilter phy)
makeBinding ["custom", s, m] first r phy = Binding first r (read s) (read m) (phyFilter phy)
makeBinding _ _ _ _ = error "affinity: unknown IRQ binding strategy"

-- main function
--

main :: IO ()
main = execParser opts >>= mainRun
  where
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc
            <> progDesc "Strategy is one of: basic, round-robin, multiple/n, raster/n, even, odd, any, all-in:id, step:id, custom:step/multi."
            <> header "Affinity: a Linux interrupt affinity binding tool")

mainRun :: Options -> IO ()
mainRun opt = handle (\(ErrorCall msg) -> putStrBoldLn (red <> T.pack msg <> reset) *> exitWith (ExitFailure 1)) $ do
    evalStateT (runCmd opt) (opt, opt.firstCPU)

-- dispatch commands
--

runCmd :: Options -> BindIO ()
runCmd Options{..}
    | version = liftIO $ putStrLn $ showVersion P.version
    | showAll = liftIO $ showAllIRQs (T.pack <$> arguments)
    | (_ : _) <- showCPU = liftIO $ mapM_ (showIRQByCPU (T.pack <$> arguments)) showCPU
    | showIRQ = liftIO $ mapM_ showIRQByName arguments
    | showXPS = liftIO $ mapM_ (showXPSBinding xpsFlavor) arguments
    | Just dev <- bindIRQ = setBinding irqAffinity dev (map read arguments) (fst3 <$> getInterruptsByDevice dev)
    | Just strategy <- irqStrategy = forM_ arguments $ \dev -> applyStrategy strategy Tx (setBinding irqAffinity) dev (fst3 <$> getInterruptsByDevice dev)
    | Just strategy <- xpsStrategy = forM_ arguments $ \dev -> applyStrategy strategy Tx (setBinding xpsAffinity) dev [0..getNumberOfQueues dev Tx-1]
    | (_ : _) <- arguments = liftIO $ mapM_ showBinding arguments
    | otherwise = liftIO $ putStrLn "done."

-- applyStrategy: apply a given strategy to a given device
--

printBindingInfo :: Device -> Binding -> [Int] -> [Int] -> BindIO ()
printBindingInfo dev binding cpus irqs = do
    (opt, start) <- get
    liftIO $ do
        printf "computing the eligible CPUs for dev %s, excluding CPUs %s, starting from %d\n" dev (show opt.exclude) (fromMaybe 0 start)
        printf "num channels (Tx): %d\n" (getNumberOfChannels dev Tx)
        printf "num channels (Rx): %d\n" (getNumberOfChannels dev Rx)
        printf "num IRQ lines (from /proc/interrupts): %d\n" (getNumberOfIRQ dev)
        printf "binding: %s\n" (show binding)
        printf "eligible CPUs: %s\n" (show cpus)
        printf "irq/queues %s\n" (show irqs)

type Binder = (Device -> Int -> [Int] -> BindIO ())

applyStrategy :: String -> QueueType -> (String -> [Int] -> [Int] -> BindIO ()) -> String -> [Int] -> BindIO ()
applyStrategy strategyName qt strategy dev irqs = do
    (opt, start) <- get
    let binding = makeBinding (splitOneOf ":/" strategyName) opt.firstCPU opt.range opt.package
        cpus = mkEligibleCPUs dev (opt.exclude) start binding qt
    when opt.verbose $ printBindingInfo dev binding cpus irqs
    strategy dev cpus irqs

setBinding :: Binder -> Device -> [Int] -> [Int] -> BindIO ()
setBinding setAffinity dev cpus irqs = do
    (opt, _) <- get

    if null irqs
        then error $ "affinity: No eligible IRQs for " <> dev <> "!"
        else liftIO $ putStr ("Binding queues for device " <> dev <> ":") *> putStrLn (if opt.dryRun then " (dry run)" else "")

    let cpus' = case cpus of
                    [] -> repeat Nothing
                    xs -> map Just xs

    if opt.oneToMany
            then forM_ irqs $ \irq -> setAffinity dev irq cpus
            else forM_ (zip irqs cpus') $ \(irq, cpu) -> setAffinity dev irq (maybeToList cpu)

    put (opt, Just (last cpus + 1))
  where

irqAffinity :: Device -> Int -> [Int] -> BindIO ()
irqAffinity _dev irq cpus = do
    (opt, _) <- get
    liftIO $ do
        putStr $ "  irq " <> show irq <> " →  CPU " <> show cpus <> " {mask = " <> mask' <> "} "
        if opt.dryRun
            then putStrLn $ "[ " <> proc_irq <> show irq <> "/smp_affinity" <> " <- " <> mask' <> " ]"
            else putChar '\n' *> writeFile (proc_irq <> show irq <> "/smp_affinity") mask'
  where
    mask' = showMask $ makeCpuMask cpus

-- show IRQ affinity of a given device
--

showBinding :: String -> IO ()
showBinding dev = do
    let irq = getInterruptsByDevice dev
    let cpus = (nub . sort) (concatMap (getIRQAffinity . fst3) irq)

    putStrLn $ "IRQ binding for device " <> dev <> " on cpu " <> show cpus <> " (" <> show (length irq) <> " IRQs): "

    when (null irq) $
        error $
            "affinity: IRQ vector not found for dev " <> dev <> "!"

    forM_ irq $ \(n, descr, _) -> do
        let cs = getIRQAffinity n
        printf "  irq %s%d%s:%s%s%s →  cpu %v\n" red n reset green descr reset (show cs)

-- show cpus irq map
--

showAllIRQs :: [T.Text] -> IO ()
showAllIRQs filts = do
    let irqs = (\irq -> (irq, getIRQAffinity (irq.number))) <$> getInterrupts

    let sfilter = T.unpack <$> filts

    forM_ (reverseMap irqs) $ \(cpu, irqs') -> do
        let irqs'' = filter (\irq -> null sfilter || or ((T.unpack irq.description =~) <$> sfilter)) irqs'
        unless (null irqs'') $ do
            putStr $ "  cpu " <> show cpu <> " →  "
            forM_ irqs'' $ \irq -> do
                printf "%s%d%s:%s%s%s " red irq.number reset green irq.description reset
            T.putStr "\n"

reverseMap :: [(IRQ, [Int])] -> [(Int, [IRQ])]
reverseMap irqs = do
    let cpus = nub $ sort $ concatMap snd irqs
    cpu <- cpus
    let irqs' = filter (\(_, cs) -> cpu `elem` cs) irqs
    return (cpu, fst <$> irqs')

-- show IRQ list of a given cpu
--

showIRQByCPU :: [T.Text] -> Int -> IO ()
showIRQByCPU filts cpu = do
    let irqs = getInterrupts

    putStrLn $ "CPU " <> show cpu <> ":"

    out <-
        catMaybes
            <$> forM
                irqs
                ( \irq -> do
                    let cs = getIRQAffinity irq.number
                    if cpu `elem` cs
                        then return $ Just irq
                        else return Nothing
                )

    forM_ out $ \irq -> do
        let xs = fmap (T.unpack irq.description =~) (T.unpack <$> filts) :: [Bool]
        when (or xs || null filts) $ do
            let cntrs = getIRQCounters irq.number
            printf "  IRQ %s%d%s:%s%s%s →  %d\n" red irq.number reset green irq.description reset (cntrs !! cpu)

-- show IRQ list of a given device
--

showIRQByName :: String -> IO ()
showIRQByName dev = do
    let irq = getInterruptsByDevice dev

    putStrLn $ "IRQ binding for device " <> dev <> ": "

    when (null irq) $
        error $
            "affinity: IRQ vector not found for dev " <> dev <> "!"

    forM_ irq $ \(n, descr, count) -> do
        printf "  irq %s%d%s:%s%s%s →  %d %s\n" red n reset green descr reset (sum count) (show count)

    printf "  irq total: %d\n" (sum $ (\(_, _, count) -> sum count) <$> irq)

-- given a device and a bind-strategy, create the list of eligible cpu
--

mkEligibleCPUs :: Device -> [Int] -> Maybe Int -> Binding -> QueueType -> [Int]
mkEligibleCPUs _ excl _ (Binding Nothing _ 0 0 _) _ = [n | n <- [0 .. getNumberOfProcessors - 1], n `notElem` excl]
mkEligibleCPUs dev excl first (Binding{..}) qt =
    take nqueue
        [ n | let f = fromMaybe (fromMaybe 0 first) bStart
            , x <- [f, f + bStep .. 4096] >>= replicate bMulti
            -- make the list of eligible CPUs
            , let n = x `mod` getNumberOfProcessors, bFilter n, n `notElem` excl, n >= fst bRange, n <= snd bRange -- modulo number of max CPUs
        ]
  where
    nqueue = getNumberOfChannels dev qt

-- XPS affinity binding

xpsAffinity :: Device -> Int -> [Int] -> BindIO ()
xpsAffinity dev queue cpus = do
    (opt, _) <- get
    liftIO $ do
        putStr $ "  queue " <> show queue <> " →  CPU " <> show cpus <> " {mask = " <> mask' <> "} "
        if opt.dryRun
            then putStrLn $ "[ " <> proc opt <> " <- " <> mask' <> " ]"
            else putChar '\n' *> writeFile (proc opt) mask'
    where
        mask' = showMask $ makeCpuMask cpus
        proc :: Options -> String
        proc opt = "/sys/class/net/" <> dev <> "/queues/tx-" <> show queue <> "/xps_" <> fromMaybe "cpu" opt.xpsFlavor <> "s"

showXPSBinding :: Maybe String -> String -> IO ()
showXPSBinding flavor dev = do
    let nq = getNumberOfQueues dev Tx
        fl = fromMaybe "cpu" flavor
    printf "%s (%d queues, kind '%s'):\n" dev nq fl
    forM_ [0 .. nq - 1] $ \q -> do
        let base = "/sys/class/net/" <> dev <> "/queues/tx-" <> show q
        cpus <- cpusFromMask . readMask <$> readFile (base <> "/xps_" <> fl <> "s")
        printf "  %sTx-%d%s -> cpu %s\n" red q reset (show cpus)
