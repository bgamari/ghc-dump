{-# LANGUAGE CPP #-}

module GhcDump.Plugin where

#if MIN_VERSION_ghc(9,2,0)
import GHC (getLogger)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Error (showPass)
import GHC.Plugins hiding (TB)
import qualified GHC.Utils.Outputable as Outputable ((<>))

#else

#if !MIN_VERSION_ghc(8,8,0)
import CoreMonad (pprPassDetails)
#endif
import GhcPlugins hiding (TB)
import qualified GhcPlugins as Outputable ((<>))
import ErrUtils (showPass)
#endif

import Data.Maybe
import Text.Printf

import System.FilePath
import System.Directory

import GhcDump.Ast (writeSModule)
import GhcDump.Convert

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _opts todo = do
    dflags <- getDynFlags
    return (intersperseDumps dflags todo)

showDump :: DynFlags -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
showDump _dflags = showSDocDump defaultSDocContext
#else
showDump dflags = showSDocDump dflags
#endif

intersperseDumps :: DynFlags -> [CoreToDo] -> [CoreToDo]
intersperseDumps dflags = go 0 "desugar"
  where
    go n phase (todo : rest) = pass n phase : todo : go (n+1) phase' rest
      where phase' = showDump dflags (ppr todo Outputable.<> text ":" <+> pprPassDetails todo)
    go n phase [] = [pass n phase]

    pass n phase = CoreDoPluginPass "DumpCore" (dumpIn dflags n phase)

-- Compatibility shim
showPass' :: String -> CoreM ()
showPass' s = do
    dflags <- getDynFlags
#if MIN_VERSION_ghc(9,2,0)
    logger <- getLogger
    liftIO $ showPass logger dflags s
#else
    liftIO $ showPass dflags s
#endif

dumpIn :: DynFlags -> Int -> String -> ModGuts -> CoreM ModGuts
dumpIn dflags n phase guts = do
    let prefix = fromMaybe "dump" $ dumpPrefix dflags
        fname = printf "%spass-%04u.cbor.zstd" prefix n
    showPass' $ "GhcDump: Dumping core to "++fname
    let in_dump_dir = maybe id (</>) (dumpDir dflags)
    liftIO $ createDirectoryIfMissing True $ takeDirectory $ in_dump_dir fname
    liftIO $ writeSModule (in_dump_dir fname) (cvtModule dflags n phase guts)
    return guts
