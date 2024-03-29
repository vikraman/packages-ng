{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PackagesNG.ParsePackage where

import           Control.Applicative              ((<*))
import qualified Data.Attoparsec.ByteString.Char8 as Atto (Parser (), char,
                                                           char, decimal,
                                                           endOfLine, isSpace,
                                                           many1, parseOnly,
                                                           sepBy, space, string,
                                                           takeByteString,
                                                           takeTill, takeWhile)
import qualified Data.ByteString                  as BS (ByteString (),
                                                         readFile)
import qualified Data.ByteString.Char8            as BSC (pack)
import qualified Data.Map.Strict                  as M (Map (), findWithDefault,
                                                        fromList, lookup)
import           System.IO                        (hPutStrLn, stderr)

import           PackagesNG.Portage               (CPV (..), Metadata,
                                                   Package (..))

parseCPV :: Atto.Parser CPV
parseCPV = do
  _                              <- Atto.string "./"
  category                       <- Atto.takeWhile (/= '/')
  _                              <- Atto.char '/'
  -- TODO: this should be greedy
  package                        <- Atto.takeWhile (/= '-')
  _                              <- Atto.char '-'
  version                        <- Atto.takeByteString

  return $ CPV category package version

parseLine :: Atto.Parser (BS.ByteString, BS.ByteString)
parseLine = do
  variable <- Atto.takeWhile (/= '=')
  _        <- Atto.char '='
  value    <- Atto.takeWhile (/= '\n')
  return (variable, value)

parseMetadata :: Atto.Parser Metadata
parseMetadata = fmap M.fromList . Atto.many1 $ parseLine <* Atto.endOfLine

orElse :: Maybe a -> String -> Either String a
orElse Nothing err = Left err
orElse (Just a) _  = Right a

lookup' :: (Ord k, Show k) => k -> M.Map k a -> Either String a
lookup' k m = M.lookup k m `orElse` ("Missing field " ++ show k)

lookupWithDef :: Ord k => a -> k -> M.Map k a -> Either String a
lookupWithDef a k m = Right $ M.findWithDefault a k m

parseWord :: Atto.Parser BS.ByteString
parseWord = Atto.takeTill Atto.isSpace

parseWords :: Atto.Parser [BS.ByteString]
parseWords = parseWord `Atto.sepBy` Atto.space

parsePackage :: BS.ByteString -> BS.ByteString -> Either String Package
parsePackage cpvString metadataString = do
  CPV category package version <- Atto.parseOnly parseCPV cpvString
  metadata    <- Atto.parseOnly parseMetadata metadataString
  description <- lookup' "DESCRIPTION" metadata
  eapis       <- lookupWithDef "5" "EAPI" metadata
  eapi        <- Atto.parseOnly Atto.decimal eapis
  homepages   <- lookup' "HOMEPAGE" metadata
  homepage    <- Atto.parseOnly parseWords homepages
  iuses       <- lookupWithDef "" "IUSE" metadata
  iuse        <- Atto.parseOnly parseWords iuses
  keywordss   <- lookupWithDef "" "KEYWORDS" metadata
  keywords    <- Atto.parseOnly parseWords keywordss
  licenses    <- lookup' "LICENSE" metadata
  license     <- Atto.parseOnly parseWords licenses
  srcUris     <- lookupWithDef "" "SRC_URI" metadata
  srcUri      <- Atto.parseOnly parseWords srcUris
  slot        <- lookup' "SLOT" metadata
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
printPackage path = do
  metadata <- BS.readFile path
  let cpv = BSC.pack path
  either (hPutStrLn stderr . ((path ++ ": ") ++ ))
         print
         (parsePackage cpv metadata)
