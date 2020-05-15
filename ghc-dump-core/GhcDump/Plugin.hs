{-# LANGUAGE CPP #-}

module GhcDump.Plugin where

import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Serialise as Ser
import GhcPlugins hiding (TB)
#if !MIN_VERSION_ghc(8,8,0)
import CoreMonad (pprPassDetails)
#endif
import ErrUtils (showPass)
import Text.Printf
import System.FilePath
import System.Directory

import GhcDump.Convert

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _opts todo = do
    dflags <- getDynFlags
    return (intersperseDumps dflags todo)

intersperseDumps :: DynFlags -> [CoreToDo] -> [CoreToDo]
intersperseDumps dflags = go 0 "desugar"
  where
    go n phase (todo : rest) = pass n phase : todo : go (n+1) phase' rest
      where phase' = showSDocDump dflags (ppr todo GhcPlugins.<> text ":" <+> pprPassDetails todo)
    go n phase [] = [pass n phase]

    pass n phase = CoreDoPluginPass "DumpCore" (liftIO . dumpIn dflags n phase)

dumpIn :: DynFlags -> Int -> String -> ModGuts -> IO ModGuts
dumpIn dflags n phase guts = do
    let prefix = fromMaybe "dump" $ dumpPrefix dflags
        fname = printf "%spass-%04u.cbor" prefix n
    showPass dflags $ "GhcDump: Dumping core to "++fname
    let in_dump_dir = maybe id (</>) (dumpDir dflags)
    createDirectoryIfMissing True $ takeDirectory $ in_dump_dir fname
    BSL.writeFile (in_dump_dir fname) $ Ser.serialise (cvtModule phase guts)
    return guts
