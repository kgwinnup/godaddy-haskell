module Main where

import DocTest
import System.FilePath.Glob (glob)

main = glob "src/**/*.hs" >>= docTest