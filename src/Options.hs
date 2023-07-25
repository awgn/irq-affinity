{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Options (
    Options(..)
    , parseOptions
) where

import Options.Applicative
import Proc

data Options = Options
    { irqStrategy :: Maybe String
    , xpsStrategy :: Maybe String
    , xpsFlavor :: Maybe String
    , firstCPU :: Maybe Int
    , oneToMany :: Bool
    , bindIRQ :: Maybe Device
    , dryRun :: Bool
    , showAll :: Bool
    , showCPU :: [Int]
    , showIRQ :: Bool
    , showXPS :: Bool
    , exclude :: [Int]
    , package :: Maybe Int
    , range :: (Int, Int)
    , version :: Bool
    , verbose :: Bool
    , arguments :: [String]
    }
    deriving stock (Show)


parseOptions :: Parser Options
parseOptions = do

     irqStrategy <- optional (strOption ( long "irq-strategy"
           <> short 'i'
           <> metavar "STRATEGY"
           <> help "Strategy for IRQ affinity"))

     xpsStrategy <- optional (strOption ( long "xps-strategy"
           <> short 'x'
           <> metavar "STRAGEY"
           <> help "Strategy for XSP affinty"))

     xpsFlavor <- optional (strOption ( long "xps-flavor"
           <> short 'f'
           <> metavar "STRING"
           <> help "Flavor for the strategy (cpu vs rxq for XPS)"))

     bindIRQ <- optional (strOption ( long "irq"
           <> metavar "NAME"
           <> help "Set the IRQs affinity of the given device (e.g., --bind eth0 1 2)."))

     firstCPU <- optional ( option auto ( long "first-cpu"
           <> metavar "INT"
           <> help "First CPU involved in binding"))

     showCPU <- many (option auto ( long "show-cpu"
           <> metavar "INT"
           <> help "Display the IRQs of the given CPUs set, for every argument passed"))

     showIRQ <- switch ( long "show-irq"
           <> help "Display IRQs by names")

     showXPS <- switch ( long "show-xps"
           <> help "Display XPS binding by names")

     package <- optional ( option auto ( long "package"
           <> short 'p'
           <> help "Apply the strategy to the given package (physical id)"))

     exclude <- many (option auto ( long "exclude"
           <> metavar "[INT]"
           <> help "Exclude CPUs from binding"))

     range <- option auto ( long "range"
           <> metavar "INT,INT"
           <> value (0, 4096)
           <> help "Range of CPUs involved in binding")

     oneToMany <- switch ( long "first-cpu"
           <> help "Bind each IRQ to every eligible CPU. Note: by default irq affinity is set one-to-one")

     dryRun <- switch ( long "dryrun"
           <> help "Dry run, don't actually set IRQ affinity")

     showAll <- switch ( long "show-all"
           <> help "Display IRQs for all CPUs available")

     version <- switch
            ( long "version"
           <> short 'V'
           <> help "Print version")

     verbose <- switch
            ( long "verbose"
           <> short 'v'
           <> help "Verbose mode")

     arguments <- many (argument str (metavar "DEVICES..."))

     return  Options{..}