{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Console.CmdArgs
import qualified Data.Text as T

data Arguments
  = Resolve { cabalFile :: [FilePath] }
  deriving (Show, Data, Typeable)


resolve = Resolve
  { cabalFile = def &= typ "CABAL" &= args
  } &= help "Resolve all possible dependencies."

allModes = modes [resolve]
  &= summary "Hell Diver (Dependency compatability explorer)"
  &= program "hell-diver"

main :: IO ()
main = do
  options <- cmdArgs allModes
  print options
  putStrLn "hell-diving has begun!"
  case cabalFile options of
    [inputFile] -> do
      print =<< extractDependencies inputFile
    _ -> putStrLn "Error: Expected one and only one cabal file as input"
