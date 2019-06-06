module Main where

import           Prelude                 hiding ( read )

import qualified Control.Effect                as F
import qualified Polysemy                      as P
import           Control.Monad.IO.Class         ( liftIO )
import           Fusion
import           Poly                           ( echoIO )

fusedMain :: IO ()
fusedMain = F.runM $ runTeletypeIO fusedEcho

polyMain :: IO ()
polyMain = P.runM echoIO

main :: IO ()
main = fusedMain
