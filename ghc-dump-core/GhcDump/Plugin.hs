{-# LANGUAGE CPP #-}

module GhcDump.Plugin where

import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Serialise.CBOR as CBOR
import GhcPlugins hiding (TB)
import DynFlags (getDynFlags, dumpPrefix)
import ErrUtils (showPass)
import CoreMonad (CoreToDo(CoreDoPluginPass))

import GhcDump.Convert

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    dflags <- getDynFlags
    return (intersperseDumps dflags todo)

intersperseDumps :: DynFlags -> [CoreToDo] -> [CoreToDo]
intersperseDumps dflags = go 0
  where
    go n (todo : rest) = pass n : todo : go (n+1) rest
    go n [] = [pass n]

    pass n = CoreDoPluginPass "DumpCore" (liftIO . dumpIn dflags n)

dumpIn :: DynFlags -> Int -> ModGuts -> IO ModGuts
dumpIn dflags n guts = do
    let prefix = fromMaybe "dump" $ dumpPrefix dflags
        fname = prefix++"pass-"++show n++".cbor"
    showPass dflags $ "GhcDump: Dumping core to "++fname
    BSL.writeFile fname $ CBOR.serialise (cvtModule guts)
    return guts
