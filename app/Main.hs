module Main where

import           Prelude                 hiding ( read )

import qualified Control.Effect                as F
import qualified Polysemy                      as P
import           Control.Monad.IO.Class         ( liftIO )
import           Fusion
import           Poly                           ( echoIO )
import           ReaderIO
import           RIO                            ( runRIO )

fusedMain :: IO ()
fusedMain = F.runM $ runTeletypeIO fusedEcho

polyMain :: IO ()
polyMain = P.runM echoIO

rioMain :: IO ()
rioMain = runRIO (Env { traceId = "123", other = "" }) echoR

main :: IO ()
main = rioMain
