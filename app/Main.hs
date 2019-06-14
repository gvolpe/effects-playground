module Main where

import           Prelude                 hiding ( read )

import           Cap                            ( echoCap )
import           Cloud                          ( cloudEcho )
import           Fusion                         ( fusedEchoIO )
import           Poly                           ( echoPoly )
import           ReaderIO                       ( echoRIO )
import           Tagless

cloudMain :: IO ()
cloudMain = cloudEcho

fusedMain :: IO ()
fusedMain = fusedEchoIO

polyMain :: IO ()
polyMain = echoPoly

rioMain :: IO ()
rioMain = echoRIO

taglessMain :: IO ()
taglessMain = echoTagless

main :: IO ()
main = echoCap
