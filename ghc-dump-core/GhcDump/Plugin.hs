{-# LANGUAGE CPP #-}

module GhcDump.Plugin where

import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Serialise.CBOR as CBOR
import GhcPlugins hiding (TB)
import DynFlags (getDynFlags, dumpPrefix)
import CoreMonad (CoreToDo(CoreDoPluginPass))

import GhcDump.Convert

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    dflags <- getDynFlags
    let prefix = fromMaybe "dump" $ dumpPrefix dflags
    return (intersperseDumps prefix todo)

intersperseDumps :: String -> [CoreToDo] -> [CoreToDo]
intersperseDumps dumpPrefix = go 0
  where
    go n (todo : rest) = pass n : todo : go (n+1) rest
    go n [] = [pass n]

    pass n = CoreDoPluginPass "DumpCore" (liftIO . dumpIn dumpPrefix n)

dumpIn :: String -> Int -> ModGuts -> IO ModGuts
dumpIn dumpPrefix n guts = do
    BSL.writeFile (dumpPrefix++"pass-"++show n++".cbor") $ CBOR.serialise (cvtModule guts)
    return guts
