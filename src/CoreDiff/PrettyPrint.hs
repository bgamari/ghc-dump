module CoreDiff.PrettyPrint where

import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen


import CoreDiff.XAst


text' = text . T.unpack


-- | Options for pretty-printing Core terms.
data PprOpts = PprOpts
  { pprOptsDisplayUniques :: Bool
  }

-- | The default options for pretty-printing Core terms.
pprOptsDefault = PprOpts True

-- | Stuff that is pretty-printable in respect to some @PprOpts@.
class PprWithOpts a where
  pprWithOpts :: a -> Reader PprOpts Doc


instance PprWithOpts XModule where
  pprWithOpts mod = return $ text' $ xModuleName mod
