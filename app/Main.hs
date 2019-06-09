module Main where

import           Prelude                 hiding ( read )

import           Cloud                          ( cloudEcho )
import           Fusion                         ( fusedEchoIO )
import           Poly                           ( echoPoly )
import           ReaderIO                       ( echoRIO )

cloudMain :: IO ()
cloudMain = cloudEcho

fusedMain :: IO ()
fusedMain = fusedEchoIO

polyMain :: IO ()
polyMain = echoPoly

rioMain :: IO ()
rioMain = echoRIO

main :: IO ()
main = cloudMain
