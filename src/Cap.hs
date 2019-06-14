{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cap
  ( echoCap
  )
where

import           Capability.Reader
import           Capability.State
import           Capability.Stream
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , runReaderT
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Monoid                    ( (<>) )
import           GHC.Generics                   ( Generic )

-- Defining capabilities
class Monad m => ConsoleIn m where
  readLine :: m String

class Monad m => ConsoleOut m where
  writeLine :: String -> m ()

-- newtype to derive reader capabilities
newtype ConsoleReader m a = ConsoleReader (m a)
  deriving (Functor, Applicative, Monad)

-- instances
instance (HasReader "input" (IO String) m, MonadIO m) => ConsoleIn (ConsoleReader m) where
  readLine = coerce (ask @"input" >>= liftIO :: m String)

instance (HasReader "output" (String -> IO ()) m, MonadIO m) => ConsoleOut (ConsoleReader m) where
  writeLine v = coerce (ask @"output" >>= liftIO . ($ v) :: m ())

-- IO instances wrapped in newtypes
newtype InCtx = InCtx { input :: IO String } deriving Generic
newtype OutCtx = OutCtx { output :: String -> IO () } deriving Generic

-- Mixing capabilities
data ConsoleIO = ConsoleIO
  { inCtx :: InCtx
  , outCtx :: OutCtx
  } deriving Generic

-- Combining capabilities using deriving via
newtype AppM m (a :: *) = AppM (ReaderT ConsoleIO m a)
  deriving (Functor, Applicative, Monad)
  deriving ConsoleIn via (ConsoleReader (Field "input" "inCtx" (Field "inCtx" () (MonadReader (ReaderT ConsoleIO m)))))
  deriving ConsoleOut via (ConsoleReader (Field "output" "outCtx" (Field "outCtx" () (MonadReader (ReaderT ConsoleIO m)))))

echo :: (ConsoleIn m, ConsoleOut m) => m ()
echo = do
  i <- readLine
  case i of
    "" -> pure ()
    _  -> writeLine i >> echo

echoApp :: MonadIO m => AppM m a -> m a
echoApp (AppM m) = runReaderT m ConsoleIO
  { inCtx  = InCtx { input = getLine }
  , outCtx = OutCtx { output = putStrLn }
  }

echoCap :: IO ()
echoCap = echoApp echo
