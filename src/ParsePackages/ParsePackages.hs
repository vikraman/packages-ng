{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent.Async
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Data.Map.Strict                  as M
import           Prelude                          hiding (takeWhile)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FilePath.Glob
import           System.IO

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

parseCPV :: Parser CPV
parseCPV = do _ <- string "./"
              category <- takeWhile (/= '/')
              _ <- char '/'
              package <- takeWhile (/= '-')
              _ <- char '-'
              version <- takeByteString
              return $ CPV category package version

parseLine :: Parser (BS.ByteString, BS.ByteString)
parseLine = do variable <- takeWhile (/= '=')
               _ <- char '='
               value <- takeWhile (/= '\n')
               return (variable, value)

parseMetadata :: Parser Metadata
parseMetadata = fmap M.fromList . many1 $ parseLine <* endOfLine

orElse :: Maybe a -> String -> Either String a
orElse Nothing err = Left err
orElse (Just a) _  = Right a

lookup' :: (Ord k, Show k) => k -> M.Map k a -> Either String a
lookup' k m = M.lookup k m `orElse` ("Missing field " ++ show k)

lookup'' :: Ord k => a -> k -> M.Map k a -> Either String a
lookup'' a k m = Right $ M.findWithDefault a k m

parseWord :: Parser BS.ByteString
parseWord = takeTill isSpace

parseWords :: Parser [BS.ByteString]
parseWords = parseWord `sepBy` space

parsePackage :: BS.ByteString -> BS.ByteString -> Either String Package
parsePackage cpvString metadataString =
  do CPV category package version <- parseOnly parseCPV cpvString
     metadata <- parseOnly parseMetadata metadataString
     description <- lookup' "DESCRIPTION" metadata
     eapis <- lookup'' "5" "EAPI" metadata
     eapi <- parseOnly decimal eapis
     homepages <- lookup' "HOMEPAGE" metadata
     homepage <- parseOnly parseWords homepages
     iuses <- lookup'' "" "IUSE" metadata
     iuse <- parseOnly parseWords iuses
     keywordss <- lookup'' "" "KEYWORDS" metadata
     keywords <- parseOnly parseWords keywordss
     licenses <- lookup' "LICENSE" metadata
     license <- parseOnly parseWords licenses
     srcUris <- lookup'' "" "SRC_URI" metadata
     srcUri <- parseOnly parseWords srcUris
     slot <- lookup' "SLOT" metadata
     return $ Package category
                      package
                      version
                      description
                      eapi
                      homepage
                      iuse
                      keywords
                      license
                      srcUri
                      slot

printPackage :: FilePath -> IO ()
printPackage path = do metadata <- BS.readFile path
                       let cpv = BSC.pack path
                       either (hPutStrLn stderr . ((path ++ ": ") ++ ))
                              print
                              (parsePackage cpv metadata)

main :: IO ()
main = do args <- getArgs
          let repo = case args of
                       [] -> "/usr/portage"
                       path : _ -> path
          setCurrentDirectory $ repo </> "metadata/md5-cache"
          allFiles <- namesMatching "*/*"
          _ <- printPackage `mapConcurrently` allFiles
          return ()
