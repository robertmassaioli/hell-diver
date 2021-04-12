{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( Package(..)
    , Requirements(..)
    , VersionRanges(..)
    , extractDependencies
    , printPackage
    ) where

import qualified Data.Text as T
import Data.Ranges
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.PackageDescription as C
import qualified Distribution.Types.CondTree as C
import qualified Distribution.Types.Dependency as C
import qualified Distribution.Verbosity as C
import qualified Distribution.Types.PackageId as C
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.Version as V
import qualified Distribution.Types.VersionRange as V
import qualified Distribution.Types.VersionRange.Internal as V
import qualified Data.Map as M
import Control.Monad (forM)

data Package = Package
    { pName :: String
    , pRequirements :: Requirements
    } deriving (Show)

printPackage :: Package -> IO ()
printPackage package = do
    putStrLn . pName $ package
    putStrLn ""
    printRequirements . pRequirements $ package

type VersionRanges = Ranges V.Version

data Requirements = Requirements (M.Map String VersionRanges)
    deriving (Show)

printRequirements :: Requirements -> IO ()
printRequirements (Requirements r) = do
    putStrLn "Requirements:"
    putStrLn ""
    forM (M.toList r) $ \(name, ranges) -> do
        putStr name
        putStr " "
        print ranges
    return ()

extractDependencies :: FilePath -> IO (Either T.Text Package)
extractDependencies cabalFile = do
    cabalPackage <- C.readGenericPackageDescription C.normal cabalFile
    let name = C.unPackageName . C.pkgName . C.package . C.packageDescription $ cabalPackage
    case C.condLibrary cabalPackage of
        Nothing -> return . Left . T.pack $ name ++ ": This package had no library."
        Just lib -> do
            let dependencies = fmap convertDependency . C.condTreeConstraints $ lib
            let requirements = Requirements . M.fromList $ dependencies
            return . Right $ Package
                { pName = name
                , pRequirements = requirements
                }

convertDependency :: C.Dependency -> (String, VersionRanges)
convertDependency (C.Dependency packageName versionRange _) = (C.unPackageName packageName, convertVersionRange versionRange)

convertVersionRange :: V.VersionRange -> VersionRanges
convertversionRange V.AnyVersion = inf
convertVersionRange (V.ThisVersion v) = v +=+ v
convertVersionRange (V.LaterVersion v) = lbe v
convertVersionRange (V.OrLaterVersion v) = lbi v
convertVersionRange (V.EarlierVersion v) = ube v
convertVersionRange (V.OrEarlierVersion v) = ubi v
convertVersionRange (V.WildcardVersion v) = v +=* V.wildcardUpperBound v
convertVersionRange (V.MajorBoundVersion v) = v +=* V.majorUpperBound v
convertVersionRange (V.UnionVersionRanges a b) = convertVersionRange a `union` convertVersionRange b
convertVersionRange (V.IntersectVersionRanges a b) = convertVersionRange a `intersection` convertVersionRange b
convertVersionRange (V.VersionRangeParens v) = convertVersionRange v
