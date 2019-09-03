module Main where

import Parser

import Control.Monad.Trans
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "$ "
      case input of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> 
          case parseProgram input of
            Right program -> do
              mapM_ (outputStrLn . show) program
              loop
            Left err -> do
              outputStrLn . show $ err
              loop

