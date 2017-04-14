module Plugin where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Serialise.CBOR as CBOR
import GhcPlugins hiding (TB)
import CoreMonad (CoreToDo(CoreDoPluginPass))

import Convert

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    reinitializeGlobals
    return (intersperseDumps todo)

intersperseDumps :: [CoreToDo] -> [CoreToDo]
intersperseDumps = go 0
  where
    go n (todo : rest) = pass n : todo : intersperseDumps rest
    go n [] = [pass n]

    pass n = CoreDoPluginPass "DumpCore" (liftIO . dumpIn n)

dumpIn :: Int -> ModGuts -> IO ModGuts
dumpIn n guts = do
    putStr $ "Dumping "++show n++"... "
    BSL.writeFile ("dump-"++show n++".cbor") $ CBOR.serialise (cvtModule guts)
    putStrLn $ "done."
    return guts
