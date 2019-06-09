{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- Slight modification from https://github.com/fused-effects/fused-effects/blob/master/examples/Teletype.hs

module Fusion
  ( fusedEchoIO
  )
where

import           Prelude                 hiding ( read )

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.State
import           Control.Effect.Sum
import           Control.Effect.Writer
import           Control.Monad.IO.Class
import           Data.Coerce

-- Our effect type modeling reads and writes as a higher-order Functor
-- m: models the embedded effect
-- k: models the remainder of the computation (or continuation)
data Teletype (m :: * -> *) k
  = Read (String -> k)
  | Write String k
  deriving (Functor, HFunctor, Effect)

read :: (Member Teletype sig, Carrier sig m) => m String
read = send (Read pure)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (pure ()))

-- A newtype defining IO computations over Teletype
newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

-- We can only handle Read and Write. Any other effects have to be handled by other instances.
-- eff: handles effectful computations
-- sig: represents the remainder of effects
instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  eff (L (Read k   )) = liftIO getLine >>= k
  eff (L (Write s k)) = liftIO (putStrLn s) >> k
  eff (R other      ) = TeletypeIOC (eff (handleCoercible other))

fusedEcho :: (Member Teletype sig, Carrier sig m) => m ()
fusedEcho = do
  i <- read
  case i of
    "" -> pure ()
    _  -> write i >> fusedEcho

fusedEchoIO :: IO ()
fusedEchoIO = runM $ runTeletypeIO fusedEcho

