module PackagesNG.Portage (CPV(..), Metadata, Package(..)) where

import qualified Data.ByteString as BS (ByteString ())
import qualified Data.Map.Strict as M (Map ())

data CPV = CPV BS.ByteString BS.ByteString BS.ByteString deriving Show

type Metadata = M.Map BS.ByteString BS.ByteString

data Package = Package { category    :: BS.ByteString
                       , package     :: BS.ByteString
                       , version     :: BS.ByteString
                       , description :: BS.ByteString
                       , eapi        :: Int
                       , homepage    :: [BS.ByteString]
                       , iuse        :: [BS.ByteString]
                       , keywords    :: [BS.ByteString]
                       , license     :: [BS.ByteString]
                       , srcUri      :: [BS.ByteString]
                       , slot        :: BS.ByteString
                       } deriving Show
