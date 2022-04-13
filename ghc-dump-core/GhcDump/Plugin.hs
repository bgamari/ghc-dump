{-# LANGUAGE CPP #-}

module GhcDump.Plugin where

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
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Serialise as Ser

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
      where phase' = showSDocDump dflags (ppr todo Outputable.<> text ":" <+> pprPassDetails todo)
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
