{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                 hiding ( read )

import qualified Control.Effect                as F
import qualified Polysemy                      as P
import           Control.Monad.IO.Class         ( liftIO )
import           Fusion
import           Poly                           ( echoIO )
import           ReaderIO
import           RIO

fusedMain :: IO ()
fusedMain = F.runM $ runTeletypeIO fusedEcho

polyMain :: IO ()
polyMain = P.runM echoIO

rioMain :: IO ()
rioMain = do
  logging <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True $ setLogUseLoc True logging
  withLogFunc logOptions $ \logFunc -> do
    let env = Env { appTraceId = "123", appLogFunc = logFunc }
    runRIO env echoR

main :: IO ()
main = rioMain
