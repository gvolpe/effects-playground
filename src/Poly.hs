{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Poly
  ( echoPoly
  )
where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

data Teletype m a where
  ReadTTY  ::Teletype m String
  WriteTTY ::String -> Teletype m ()

makeSem ''Teletype

runTeletypeIO :: Member (Lift IO) r => Sem (Teletype ': r) a -> Sem r a
runTeletypeIO = interpret $ \case
  ReadTTY      -> sendM getLine
  WriteTTY msg -> sendM $ putStrLn msg

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo

echoIO :: Sem '[Lift IO] ()
echoIO = runTeletypeIO echo

echoPoly :: IO ()
echoPoly = runM echoIO
