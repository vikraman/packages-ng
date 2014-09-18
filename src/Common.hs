{-# LANGUAGE DisambiguateRecordFields #-}

module Common where

import Data.Time.Calendar (Day)

class Named a where
    name :: a -> String

class PackageTC a where
    cp :: a -> String
    cpv :: a -> String
    bugzillaLink :: a -> String
    isVirtual :: a -> Bool


type Arch = String
type PackageVersion = String

data News = News { date :: Day
                 , newsName :: String
                 , title :: String
                 , language :: String
                 , revision :: Integer
                 , message :: String
                 , authors :: [String]
                 , translators :: [String]
                 } deriving Show

instance Named News where
    name = newsName

data Tree = Tree { treeName :: String
                 , treeDescription :: Maybe String
                 , ownerName :: Maybe String
                 , ownerEmail :: Maybe String
                 , treeHomepage :: Maybe String
                 , official :: Bool
                 , stability :: TreeStability
                 , repoType :: RepoType
                 , repoUrl :: String
                 } deriving (Show, Eq)

data Category = Category { categoryName :: String
                         , categoryDescription :: Maybe String
                         } deriving (Show, Eq)

-- 0, 1, and 2 respectively
data TreeStability = Stable | Testing | Experimental
                   deriving (Show, Eq, Enum)

data RepoType = Git | Svn
             deriving (Show, Eq)

data Maintainer = Maintainer { maintainerName :: Maybe String
                             , maintainerEmail :: String
                             } deriving (Show, Eq)

data Herd = Herd { herdName :: String
                 , herdDescription :: Maybe String
                 , herdEmail :: String
                 } deriving (Show, Eq)

data Package = Package { packageTree :: Tree
                       , category :: Category
                       , packageName :: String
                       , changelog :: String
                       , herds :: [Herd]
                       , maintainers :: [Maintainer]
                       , packageHomepages :: [String]
                       , packageDescription :: String
                       , packageDetailedDescription :: Maybe String
                       } deriving (Show, Eq)

data UseFlag = UseFlag { useFlagName :: String
                       , useFlagDescription :: Maybe String
                       } deriving (Show, Eq)

main :: IO ()
main = putStrLn "Hello world!"
