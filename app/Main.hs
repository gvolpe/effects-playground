module Main where

import           Prelude                 hiding ( read )

import           Fusion                         ( fusedEchoIO )
import           Poly                           ( echoPoly )
import           ReaderIO                       ( echoRIO )

fusedMain :: IO ()
fusedMain = fusedEchoIO

polyMain :: IO ()
polyMain = echoPoly

rioMain :: IO ()
rioMain = echoRIO

main :: IO ()
main = rioMain
