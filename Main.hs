{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Prelude                 hiding ( read )

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.State
import           Control.Effect.Sum
import           Control.Effect.Writer
import           Control.Monad.IO.Class
import           Data.Coerce

data Teletype (m :: * -> *) k
  = Read (String -> k)
  | Write String k
  deriving (Functor, HFunctor, Effect)

read :: (Member Teletype sig, Carrier sig m) => m String
read = send (Read pure)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (pure ()))

runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  eff (L (Read k   )) = liftIO getLine >>= k
  eff (L (Write s k)) = liftIO (putStrLn s) >> k
  eff (R other      ) = TeletypeIOC (eff (handleCoercible other))

runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  eff (L (Read k)) = do
    i <- TeletypeRetC get
    case i of
      []    -> k ""
      h : t -> TeletypeRetC (put t) *> k h
  eff (L (Write s k)) = TeletypeRetC (tell [s]) *> k
  eff (R other      ) = TeletypeRetC (eff (R (R (handleCoercible other))))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ run (runTeletypeRet ["Testing"] read)
  print $ run (runTeletypeRet ["input"] (write "output"))
  print $ run (runTeletypeRet ["in"] (write "out1" >> write "out2"))
