module Main where

import           Prelude                 hiding ( read )

import           Control.Effect                 ( run )
import           Fusion                         ( runTeletypeRet
                                                , read
                                                , write
                                                )

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ run (runTeletypeRet ["input"] read)                        -- single input
  print $ run (runTeletypeRet ["input"] (write "output"))            -- single output
  print $ run (runTeletypeRet ["in"] (write "out1" >> write "out2")) -- multiple outputs
