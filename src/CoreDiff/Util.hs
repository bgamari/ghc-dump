module CoreDiff.Util where

import GhcDump.Util

import CoreDiff.Convert

readXModule = fmap cvtXModule . readDump
