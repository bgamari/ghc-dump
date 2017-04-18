{-# LANGUAGE CPP #-}

module GhcDump.Plugin where

import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Serialise.CBOR as CBOR
import GhcPlugins hiding (TB)
import DynFlags (getDynFlags, dumpPrefix)
import ErrUtils (showPass)
import CoreMonad (CoreToDo(CoreDoPluginPass))
import Outputable
import Text.Printf

import GhcDump.Convert

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    dflags <- getDynFlags
    return (intersperseDumps dflags todo)

intersperseDumps :: DynFlags -> [CoreToDo] -> [CoreToDo]
intersperseDumps dflags = go 0 "desugar"
  where
    go n phase (todo : rest) = pass n phase : todo : go (n+1) phase' rest
      where phase' = showSDocDump dflags (ppr todo)
    go n phase [] = [pass n phase]

    pass n phase = CoreDoPluginPass "DumpCore" (liftIO . dumpIn dflags n phase)

dumpIn :: DynFlags -> Int -> String -> ModGuts -> IO ModGuts
dumpIn dflags n phase guts = do
    let prefix = fromMaybe "dump" $ dumpPrefix dflags
        fname = printf "%spass-%04u.cbor" prefix n
    showPass dflags $ "GhcDump: Dumping core to "++fname
    BSL.writeFile fname $ CBOR.serialise (cvtModule phase guts)
    return guts
