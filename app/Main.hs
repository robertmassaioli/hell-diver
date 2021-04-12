{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Console.CmdArgs
import qualified Data.Text as T
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.GZip as GZip
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

data Arguments
  = Resolve { cabalFile :: [FilePath] }
  | UpdateCache
  deriving (Show, Data, Typeable)

resolve = Resolve
  { cabalFile = def &= typ "CABAL FILE" &= args
  } &= help "Resolve all working package selections for a given cabal file."

updateCache = UpdateCache
  &= help "Download all of the cabal files on hackage and unpack them."
  &= name "update-cache"

allModes = modes [resolve, updateCache]
  &= summary "Hell Diver (Dependency compatability explorer)"
  &= program "hell-diver"

main :: IO ()
main = do
  options <- cmdArgs allModes
  print options
  case options of
    Resolve inputFiles -> runResolve inputFiles
    UpdateCache -> runUpdateCache

runResolve :: [FilePath] -> IO ()
runResolve inputFiles = do
  putStrLn "hell-diving has begun!"
  case inputFiles of
    [inputFile] -> do
      potentialPackage <- extractDependencies inputFile
      case potentialPackage of
        Left e -> print e
        Right package -> printPackage package
    _ -> putStrLn "Error: Expected one and only one cabal file as input"

runUpdateCache :: IO ()
runUpdateCache = do
  putStrLn "Updating the hell-diving cache..."
  manager <- HC.newManager TLS.tlsManagerSettings
  request <- HC.parseRequest "https://hackage.haskell.org/01-index.tar.gz"
  response <- HC.httpLbs request manager
  homeDir <- getHomeDirectory
  unpackTarFile (homeDir </> ".hell-diver") . HC.responseBody $ response

unpackTarFile :: FilePath -> BSL.ByteString -> IO ()
unpackTarFile outputDir = Tar.unpack outputDir . Tar.read . GZip.decompress