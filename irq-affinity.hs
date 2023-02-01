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

module Main (module Main) where


import Data.Maybe ( catMaybes, fromJust )
import Data.List(nub, sort)
import Data.List.Split(splitOneOf)
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
      CmdArgs )
import System.IO.Unsafe ( unsafePerformIO )
import System.Environment (withArgs)
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Data.Bifunctor as B (second)
import Data.Tuple.Extra ( fst3 )

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
    {   firstCPU    :: Int
    ,   exclude     :: [Int]
    ,   strategy    :: Maybe String
    ,   oneToMany   :: Bool
    ,   showAllCPUs :: Bool
    ,   dryRun      :: Bool
    ,   showCPU     :: [Int]
    ,   bindIRQ     :: Device
    ,   arguments   :: [String]
    } deriving stock (Data, Typeable, Show)

type BindIO = StateT (Options, Int) IO

type CpuMask = Integer

-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   firstCPU    = 0       &= typ "CPU"   &= help "First CPU involved in binding."
    ,   exclude     = []      &= typ "CPU"   &= help "Exclude CPUs from binding."
    ,   strategy    = Nothing                &= help "Strategies: round-robin, naive, multiple/n, raster/n, even, odd, any, all-in:id, step:id, custom:step/multi."
    ,   oneToMany   = False   &= explicit    &= name "one-to-many" &= help "Bind each IRQ to every eligible CPU. Note: by default irq affinity is set one-to-one."
    ,   showAllCPUs = False                  &= help "Display IRQs for all CPUs available."
    ,   dryRun      = False                  &= help "Dry run, don't actually set affinity."
    ,   showCPU     = []      &= explicit    &= name "cpu"  &= help "Display IRQs of the given CPUs set."
    ,   bindIRQ     = def     &= explicit    &= name "bind" &= help "Set the IRQs affinity of the given device (e.g., --bind eth0 1 2)."
    ,   arguments   = []                     &= args
    } &= summary "irq-affinity: a Linux interrupt affinity binding tool." &= program "irq-affinity"


-- binding strategy
--

data IrqBinding = IrqBinding
    {   firstCpu  :: Int
    ,   stepCpu   :: Int
    ,   multi     :: Int
    ,   runFilter :: Int -> Bool
    }

makeIrqBinding :: [String] -> Int -> IrqBinding
makeIrqBinding ["round-robin"]    _      = IrqBinding (-1)      1       1       none
makeIrqBinding ["naive"]          first  = IrqBinding first     1       1       none
makeIrqBinding ["multiple", m]    _      = IrqBinding (-1)      1    (read m)   none
makeIrqBinding ["raster", m]      first  = IrqBinding first     1    (read m)   none
makeIrqBinding ["even"]           first  = IrqBinding first     1       1       even
makeIrqBinding ["odd"]            first  = IrqBinding first     1       1       odd
makeIrqBinding ["any"]            _      = IrqBinding 0         0       0       none
makeIrqBinding ["all-in", n]      _      = IrqBinding (read n)  0       1       none
makeIrqBinding ["step",   s]      first  = IrqBinding first   (read s)  1       none
makeIrqBinding ["custom", s, m]   first  = IrqBinding first   (read s) (read m) none
makeIrqBinding _ _ =  error "irq-affinity: unknown IRQ binding strategy"

none :: b -> Bool
none = const True

-- main function
--

main :: IO ()
main = handle (\(ErrorCall msg) -> putStrBoldLn (red <> T.pack msg <> reset) *> exitWith (ExitFailure 1)) $ do
    opt' <- cmdArgsRun options
    evalStateT (runCmd opt') (opt', firstCPU opt')

-- dispatch commands
--

runCmd :: Options -> BindIO ()
runCmd Options{..}
    | Just _  <- strategy   = forM_ arguments $ \dev -> applyStrategy dev
    | showAllCPUs           = liftIO $ showAllCpuIRQs (T.pack <$> arguments)
    | not $ null showCPU    = liftIO $ mapM_ (showIRQ (T.pack <$> arguments)) showCPU
    | not $ null bindIRQ    = runBinding bindIRQ (map read arguments)
    | not $ null arguments  = liftIO $ mapM_ showBinding arguments
    | otherwise             = liftIO $ withArgs ["--help"] $ void (cmdArgsRun options)


-- applyStrategy: apply a given strategy to a given device
--

applyStrategy :: String -> BindIO ()
applyStrategy dev = do
    (op, start) <- get
    let alg  = makeIrqBinding (splitOneOf ":/" $ fromJust (strategy op)) (firstCPU op)
        cpus = mkEligibleCPUs dev (exclude op) start alg
    runBinding dev cpus

-- runBinding

runBinding :: String -> [Int] -> BindIO ()
runBinding dev cpus = do
    (op, _) <- get

    let irqs = fst3 <$> getInterruptsByDevice dev

    if | null irqs -> error ("irq-affinity: IRQs not found for the " <> dev <> " device!")
       | null cpus -> error "irq-affinity: No eligible CPU found!"
       | otherwise -> liftIO $ putStr ("Setting IRQ binding for device " <> dev <>":") *> putStrLn (if op.dryRun then "  (dry run)" else "")

    let doBind = if op.oneToMany
                    then bindOneToMany
                    else bindOneToOne
    doBind irqs cpus
    put (op, last cpus + 1)


{-# INLINE bindOneToOne #-}
bindOneToOne :: [Int] -> [Int] -> BindIO ()
bindOneToOne irqs cpus = forM_ (zip irqs cpus) $ \(irq,cpu) -> setIrqAffinity irq [cpu]

{-# INLINE bindOneToMany #-}
bindOneToMany :: [Int] -> [Int] -> BindIO ()
bindOneToMany irqs cpus = forM_ irqs $ \irq -> setIrqAffinity irq cpus

-- set IRQ affinity for the given (irq,cpu) pair

setIrqAffinity :: Int -> [Int] -> BindIO ()
setIrqAffinity irq cpus = do
    (op, _ ) <- get
    liftIO $ do
        putStr $ "  irq " <> show irq <> " -> CPU " <> show cpus <> " {mask = " <> mask' <> "} "
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

    forM_ irq $ \(n,descr,_) -> do
        let cs = getIrqAffinity n
        printf "  irq %s%d%s:%s%s%s -> cpu %v\n" red  n reset green descr reset (show cs)

-- show cpus irq map
--

showAllCpuIRQs ::[T.Text] -> IO ()
showAllCpuIRQs filts = do
    let irqs = getInterrupts
    forM_ [0..getNumberOfPhyCores-1] $ \cpu -> do
        mat <- forM irqs $ \n -> do
                let cs = getIrqAffinity (fst n)
                if cpu `elem` cs
                    then return $ Just (n, cs)
                    else return Nothing
        putStr $ "  cpu " <> show cpu <> " -> "
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
            printf "  IRQ %s%d%s:%s%s%s -> %d\n" red n reset green descr reset (cntrs !! cpu)

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

mkEligibleCPUs :: Device -> [Int] -> Int -> IrqBinding -> [Int]
mkEligibleCPUs _ excl _ (IrqBinding 0 0 0 _) = [ n | n <- [0 .. getNumberOfPhyCores-1], n `notElem` excl ]
mkEligibleCPUs dev excl f (IrqBinding f' step multi' filt) =
    take nqueue [ n | let f''= if f' == -1 then f else f',
                      x <- [f'', f''+ step .. ] >>= replicate multi',  -- make the list of eligible CPUs
                      let n = x `mod` getNumberOfPhyCores,             -- modulo number of max CPUs
                      filt n,                                          -- whose elements pass the given predicate
                      n `notElem` excl ]                               -- and are not prensent in the exclusion list
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
getNumberOfIRQ dev = unsafePerformIO $ T.readFile proc_interrupt >>= \file ->
    return $ length $ filter (=~ dev) $ (lines . T.unpack) file


{-# NOINLINE getInterruptsByDevice #-}
getInterruptsByDevice :: Device -> [(Int, T.Text, [Int])]
getInterruptsByDevice dev = unsafePerformIO $ readFile proc_interrupt >>= \file -> do
    let irqLines  = T.pack <$> filter (=~ dev) (lines file)
        irqColumns = T.words <$> irqLines
    return $ map (\cs -> ((read . T.unpack . T.takeWhile (/= ':')) (head cs), last cs, read . T.unpack <$> (take getNumberOfPhyCores . tail) cs)) irqColumns


{-# NOINLINE getNumberOfPhyCores #-}
getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ T.readFile proc_cpuinfo >>= \file ->
    return $ length $ filter ("processor" `T.isInfixOf`) $ T.lines file


{-# NOINLINE getIRQCounters #-}
getIRQCounters :: Int -> [Int]
getIRQCounters irq = unsafePerformIO $ T.readFile proc_interrupt >>= \file -> do
    let dev = T.pack $ show irq <> ":"
        irqCol = read . T.unpack <$> (take getNumberOfPhyCores . tail $ concatMap T.words $ filter ((== dev) . head . T.words) (T.lines file)) :: [Int]
    return $ irqCol