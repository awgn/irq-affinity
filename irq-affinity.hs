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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Main (module Main) where


import Data.Maybe ( catMaybes, fromJust, fromMaybe )
import Data.List(nub, sort)
import Data.List.Split(splitOneOf, chunksOf)
import Control.Monad.State
    ( forM_,
      void,
      forM,
      when,
      MonadIO(liftIO),
      evalStateT,
      MonadState(put, get),
      StateT )
import Control.Exception ( handle, ErrorCall(ErrorCall) )
import Text.Regex.Posix ( (=~) )

import Numeric(showHex,readHex)

import Text.Printf ( printf )
import Data.Bits ( Bits((.&.), (.|.), shiftL) )
import Data.Data ( Data, Typeable )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import System.Console.ANSI
    ( setSGRCode,
      Color(Green, Red, Blue),
      ColorIntensity(Vivid),
      ConsoleIntensity(BoldIntensity),
      ConsoleLayer(Foreground),
      SGR(SetColor, SetConsoleIntensity) )
import System.Console.CmdArgs
    ( (&=),
      cmdArgsMode,
      cmdArgsRun,
      args,
      explicit,
      help,
      name,
      program,
      summary,
      typ,
      Default(def),
      Mode,
      CmdArgs, groupname )
import System.IO.Unsafe ( unsafePerformIO )
import System.Environment (withArgs)
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Data.Bifunctor as B (second)
import Data.Tuple.Extra ( fst3 )
import Data.Either ( fromRight )

bold, red, blue, green, reset :: T.Text
bold  = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
red   = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
blue  = T.pack $ setSGRCode [SetColor Foreground Vivid Blue]
green = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
reset = T.pack $ setSGRCode []

putStrBoldLn :: T.Text -> IO ()
putStrBoldLn msg = T.putStrLn $ bold <> msg <> reset

proc_interrupt, proc_cpuinfo, proc_irq :: String
proc_irq       = "/proc/irq/"
proc_interrupt = "/proc/interrupts"
proc_cpuinfo   = "/proc/cpuinfo"


type Device = String

-- Command line options
--

data Options = Options
    {   firstCPU    :: Maybe Int
    ,   strategy    :: Maybe String
    ,   oneToMany   :: Bool
    ,   showAll     :: Bool
    ,   dryRun      :: Bool
    ,   bindIRQ     :: Device
    ,   exclude     :: [Int]
    ,   package     :: Maybe Int
    ,   range       :: (Int, Int)
    ,   showCPU     :: [Int]
    ,   arguments   :: [String]
    } deriving stock (Data, Typeable, Show)

type BindIO = StateT (Options, Maybe Int) IO

type CpuMask = Integer

-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   firstCPU    = Nothing    &= typ "INT"       &= help "First CPU involved in binding."
    ,   strategy    = Nothing    &= typ "NAME"      &= help "Strategies: basic, round-robin, multiple/n, raster/n, even, odd, any, all-in:id, step:id, custom:step/multi."
    ,   oneToMany   = False   &= explicit           &= name "one-to-many" &= help "Bind each IRQ to every eligible CPU. Note: by default irq affinity is set one-to-one."
    ,   showAll     = False   &= explicit           &= name "show" &= help "Display IRQs for all CPUs available."
    ,   dryRun      = False                         &= help "Dry run, don't actually set affinity."
    ,   bindIRQ     = def     &= explicit           &= name "bind" &= help "Set the IRQs affinity of the given device (e.g., --bind eth0 1 2)."
    ,   exclude     = []         &= typ "INT"       &= groupname "Filters" &= help "Exclude CPUs from binding."
    ,   package     = Nothing &= typ "INT"          &= help "Apply then strategy to the given package (physical id)."
    ,   range       = (0, 4095)  &= typ "MIN,MAX"   &= help "Range of CPUs involved in binding."
    ,   showCPU     = []      &= explicit           &= groupname "Display" &= name "cpu"  &= help "Display IRQs of the given CPUs set."
    ,   arguments   = []                            &= args
    } &= summary "irq-affinity: a Linux interrupt affinity binding tool." &= program "irq-affinity"


-- binding strategy
--

data IrqBinding = IrqBinding
    {   bStart  :: Maybe Int
    ,   bRange  :: (Int, Int)
    ,   bStep   :: Int
    ,   bMulti  :: Int
    ,   bFilter :: Int -> Bool
    }

{-# INLINE (&&&) #-}
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g x = f x && g x


phyFilter :: Maybe Int -> Int -> Bool
phyFilter Nothing  _ = True
phyFilter (Just p) x = physicalId (getCpuInfo !! (x `mod` length getCpuInfo)) == p


makeIrqBinding :: [String] -> Maybe Int -> (Int, Int) -> Maybe Int -> IrqBinding
makeIrqBinding ["basic"]          first  r phy = IrqBinding first    r          1       1       (phyFilter phy)
makeIrqBinding ["round-robin"]    _      r phy = IrqBinding Nothing  r          1       1       (phyFilter phy)
makeIrqBinding ["multiple", m]    _      r phy = IrqBinding Nothing  r          1    (read m)   (phyFilter phy)
makeIrqBinding ["raster", m]      first  r phy = IrqBinding first    r          1    (read m)   (phyFilter phy)
makeIrqBinding ["even"]           first  r phy = IrqBinding first    r          1       1       (phyFilter phy &&& even)
makeIrqBinding ["odd"]            first  r phy = IrqBinding first    r          1       1       (phyFilter phy &&& odd)
makeIrqBinding ["any"]            _      r phy = IrqBinding (Just 0) r          0       0       (phyFilter phy)
makeIrqBinding ["all-in", n]      _      r phy = IrqBinding (Just (read n)) r   0       1       (phyFilter phy)
makeIrqBinding ["step",   s]      first  r phy = IrqBinding first    r       (read s)   1       (phyFilter phy)
makeIrqBinding ["custom", s, m]   first  r phy = IrqBinding first    r       (read s) (read m)  (phyFilter phy)
makeIrqBinding _ _ _ _ =  error "irq-affinity: unknown IRQ binding strategy"

-- CpuInfo

data CpuInfo = CpuInfo
    {
       processor  :: {-# UNPACK #-} !Int
    ,  physicalId :: {-# UNPACK #-} !Int
    ,  coreId     :: {-# UNPACK #-} !Int
    } deriving stock (Show, Eq)

-- main function
--

main :: IO ()
main = handle (\(ErrorCall msg) -> putStrBoldLn (red <> T.pack msg <> reset) *> exitWith (ExitFailure 1)) $ do
    o <- cmdArgsRun options
    evalStateT (runCmd o) (o, o.firstCPU)

-- dispatch commands
--

runCmd :: Options -> BindIO ()
runCmd Options{..}
    | Just _  <- strategy   = forM_ arguments $ \dev -> applyStrategy dev
    | showAll               = liftIO $ showAllCpuIRQs (T.pack <$> arguments)
    | not $ null showCPU    = liftIO $ mapM_ (showIRQ (T.pack <$> arguments)) showCPU
    | not $ null bindIRQ    = runBinding bindIRQ (map read arguments)
    | not $ null arguments  = liftIO $ mapM_ showBinding arguments
    | otherwise             = liftIO $ withArgs ["--help"] $ void (cmdArgsRun options)

-- applyStrategy: apply a given strategy to a given device
--

applyStrategy :: String -> BindIO ()
applyStrategy dev = do
    (op, start) <- get
    let binder  = makeIrqBinding (splitOneOf ":/" $ fromJust (op.strategy)) op.firstCPU op.range op.package
        cpus = mkEligibleCPUs dev (op.exclude) start binder
    runBinding dev cpus


-- runBinding
--

runBinding :: String -> [Int] -> BindIO ()
runBinding dev cpus = do
    (op, _) <- get

    let irqs = fst3 <$> getInterruptsByDevice dev

    if | null irqs -> error $ "irq-affinity: IRQs not found for the " <> dev <> " device!"
       | null cpus -> error "irq-affinity: No eligible CPU found!"
       | otherwise -> liftIO $ putStr ("Setting IRQ binding for device " <> dev <>":") *> putStrLn (if op.dryRun then "  (dry run)" else "")

    (if op.oneToMany
        then bindOneToMany
        else bindOneToOne) irqs cpus

    put (op, Just (last cpus + 1))


{-# INLINE bindOneToOne #-}
bindOneToOne :: [Int] -> [Int] -> BindIO ()
bindOneToOne irqs cpus = forM_ (zip irqs cpus) $ \(irq,cpu) -> setIrqAffinity irq [cpu]

{-# INLINE bindOneToMany #-}
bindOneToMany :: [Int] -> [Int] -> BindIO ()
bindOneToMany irqs cpus = forM_ irqs $ \irq -> setIrqAffinity irq cpus

-- set IRQ affinity for the given (irq,cpu) pair
--

setIrqAffinity :: Int -> [Int] -> BindIO ()
setIrqAffinity irq cpus = do
    (op, _ ) <- get
    liftIO $ do
        putStr $ "  irq " <> show irq <> " →  CPU " <> show cpus <> " {mask = " <> mask' <> "} "
        if op.dryRun
            then putStrLn $ "[ " <> proc_irq <> show irq <> "/smp_affinity" <> " <- " <> mask' <> " ]"
            else putChar '\n' *> writeFile (proc_irq <> show irq <> "/smp_affinity") mask'

      where mask' = showMask $ makeCpuMask cpus

-- show IRQ affinity of a given device
--

showBinding :: String -> IO ()
showBinding dev = do
    let irq = getInterruptsByDevice dev
    let cpus = (nub . sort) (concatMap (getIrqAffinity . fst3) irq)

    putStrLn $ "IRQ binding for device " <> dev <> " on cpu "  <> show cpus <> " (" <>  show (length irq) <> " IRQs): "

    when (null irq) $
        error $ "irq-affinity: IRQ vector not found for dev " <> dev <> "!"

    forM_ irq $ \(n, descr, _) -> do
        let cs = getIrqAffinity n
        printf "  irq %s%d%s:%s%s%s →  cpu %v\n" red  n reset green descr reset (show cs)

-- show cpus irq map
--

showAllCpuIRQs ::[T.Text] -> IO ()
showAllCpuIRQs filts = do
    let irqs = getInterrupts
    forM_ [0..getNumberOfProcessors-1] $ \cpu -> do
        mat <- forM irqs $ \n -> do
                let cs = getIrqAffinity (fst n)
                if cpu `elem` cs
                    then return $ Just (n, cs)
                    else return Nothing
        putStr $ "  cpu " <> show cpu <> " →  "
        forM_ (fst <$> catMaybes mat) $ \(n,descr) -> do
            let xs  = fmap (T.unpack descr =~) (T.unpack <$> filts) :: [Bool]
            when (or xs || null filts) $
                printf "%s%d%s:%s%s%s " red n reset green descr reset
        T.putStr "\n"

-- show IRQ list of a given cpu
--

showIRQ :: [T.Text] -> Int -> IO ()
showIRQ filts cpu = do
    let irqs = getInterrupts
    putStrLn $ "CPU " <> show cpu <> ":"

    mat <- catMaybes <$> forM irqs (\n -> do
        let cs = getIrqAffinity (fst n)
        if cpu `elem` cs
            then return $ Just (n, cs)
            else return Nothing)

    let out = fst <$> mat
    forM_ out $ \(n,descr) -> do
        let xs  = fmap (T.unpack descr =~) (T.unpack <$> filts) :: [Bool]
        when (or xs || null filts) $ do
            let cntrs = getIRQCounters n
            printf "  IRQ %s%d%s:%s%s%s →  %d\n" red n reset green descr reset (cntrs !! cpu)

-- utilities
--

intersperseEvery :: Int -> t -> [t] -> [t]
intersperseEvery n x xs = zip xs [1 .. l] >>= ins
  where ins (x',k) = if k `mod` n == 0 && k /= l then [x',x] else [x']
        l = length xs


showMask :: CpuMask -> String
showMask mask' = reverse . intersperseEvery 8 ',' . reverse $ showHex mask' ""


readMask :: String -> CpuMask
readMask = fst . head . readHex . filter (/= ',')


makeCpuMask :: [Int] -> CpuMask
makeCpuMask = foldr (\cpu mask' -> mask' .|. (1 `shiftL` cpu)) (0 :: CpuMask)


getCpusListFromMask :: CpuMask -> [Int]
getCpusListFromMask mask'  = [ n | n <- [0 .. 4095], let p2 = 1 `shiftL` n, mask' .&. p2 /= 0 ]

-- given a device and a bind-strategy, create the list of eligible cpu
--

mkEligibleCPUs :: Device -> [Int] -> Maybe Int -> IrqBinding -> [Int]
mkEligibleCPUs _ excl _ (IrqBinding Nothing _ 0 0 _) = [ n | n <- [0 .. getNumberOfProcessors-1], n `notElem` excl ]
mkEligibleCPUs dev excl f (IrqBinding{..}) =
    take nqueue [ n | let f' = fromMaybe (fromMaybe 0 f) bStart,
                      x <- [f', f'+ bStep .. 4096] >>= replicate bMulti,    -- make the list of eligible CPUs
                      let n = x `mod` getNumberOfProcessors,                -- modulo number of max CPUs
                      bFilter n,                                            -- whose elements pass the given predicate
                      n `notElem` excl,                                     -- and are not present in the exclusion list
                      n >= fst bRange,
                      n <= snd bRange                                       -- and are in the given range
                ]
        where nqueue = getNumberOfIRQ dev


-- get IRQ affinity, that is the list of the CPUs the given IRQ is bound to

{-# NOINLINE getIrqAffinity #-}
getIrqAffinity :: Int -> [Int]
getIrqAffinity irq =  unsafePerformIO $
    getCpusListFromMask . readMask <$> readFile (proc_irq <> show irq <> "/smp_affinity")


{-# NOINLINE getInterrupts #-}
getInterrupts :: [(Int, T.Text)]
getInterrupts = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (B.second (T.pack . last . words)) (concatMap reads $ lines file :: [(Int, String)])


{-# NOINLINE getNumberOfIRQ #-}
getNumberOfIRQ :: Device -> Int
getNumberOfIRQ dev = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ length $ filter (=~ dev) $ lines file


{-# NOINLINE getInterruptsByDevice #-}
getInterruptsByDevice :: Device -> [(Int, T.Text, [Int])]
getInterruptsByDevice dev = unsafePerformIO $ readFile proc_interrupt >>= \file -> do
    let irqLines  = T.pack <$> filter (=~ dev) (lines file)
        irqColumns = T.words <$> irqLines
    return $ map (\cs -> ((decimal . T.takeWhile (/= ':')) (head cs), last cs, decimal <$> (take getNumberOfProcessors . tail) cs)) irqColumns


{-# NOINLINE getNumberOfProcessors #-}
getNumberOfProcessors :: Int
getNumberOfProcessors = unsafePerformIO $ T.readFile proc_cpuinfo >>= \file ->
    return $ length $ filter ("processor" `T.isInfixOf`) $ T.lines file


{-# NOINLINE getIRQCounters #-}
getIRQCounters :: Int -> [Int]
getIRQCounters irq = unsafePerformIO $ T.readFile proc_interrupt >>= \file -> do
    let dev = T.pack $ show irq <> ":"
        irqCol = decimal <$> (take getNumberOfProcessors . tail $ concatMap T.words $ filter ((== dev) . head . T.words) (T.lines file)) :: [Int]
    return irqCol


{-# NOINLINE getCpuInfo #-}
getCpuInfo :: [CpuInfo]
getCpuInfo = unsafePerformIO $ T.readFile proc_cpuinfo >>= \file -> do
    let cpuLines = T.lines file
        cpuInfo  = chunksOf 3 $ decimal . T.drop 2 . T.dropWhile (/= ':') <$>
                    filter (\l -> "processor" `T.isPrefixOf` l || "physical id" `T.isPrefixOf` l || "core id" `T.isPrefixOf` l) cpuLines
    return $ map (\case
        [a,b,c] -> CpuInfo a b c
        _       -> error "cpuinfo: parse error") cpuInfo

{-# INLINE decimal #-}
decimal :: T.Text -> Int
decimal = fst .  fromRight (-1, "") . T.decimal
