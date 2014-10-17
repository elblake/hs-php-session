{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module : Data.PHPSession
-- Copyright: (c) 2013-2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Types used for representing PHP data types from encoding and decoding a PHP session.
-- 
module Data.PHPSession.Conv (
    -- * PHP session types
    convTo,
    convFrom,
    convFromNullable
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.PHPSession.Types
import Data.Int

class ConversionToPHPValue a where
  convTo :: a -> PHPSessionValue

class ConversionFromPHPValue b where
  convFrom :: PHPSessionValue -> b

class ConversionFromPHPValueNullable b where
  convFromNullable :: PHPSessionValue -> b


instance ConversionToPHPValue [(PHPSessionValue,PHPSessionValue)] where
  convTo var = PHPSessionValueArray var

instance (ConversionToPHPValue a, ConversionToPHPValue b) => ConversionToPHPValue [(a,b)] where
  convTo var = PHPSessionValueArray $ map (\(al,ar) -> (convTo al, convTo ar)) var

instance ConversionToPHPValue Bool where
  convTo var = PHPSessionValueBool var

instance ConversionToPHPValue Double where
  convTo var = PHPSessionValueFloat (Right var)

instance ConversionToPHPValue Int where
  convTo var = PHPSessionValueInt var

instance ConversionToPHPValue a => ConversionToPHPValue (Maybe a) where
  convTo Nothing = PHPSessionValueNull
  convTo (Just var) = convTo var

instance ConversionToPHPValue (PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)]) where
  convTo (cls,arr) = PHPSessionValueObject cls arr

instance ConversionToPHPValue (PHPSessionClassName, LBS.ByteString) where
  convTo (cls,arr) = PHPSessionValueObjectSerializeable cls arr

instance ConversionToPHPValue LBS.ByteString where
  convTo var = PHPSessionValueString var

instance ConversionToPHPValue BS.ByteString where
  convTo var = PHPSessionValueString (LBS.fromChunks [var])

instance ConversionFromPHPValue a => ConversionFromPHPValueNullable (Maybe a) where
  convFromNullable PHPSessionValueNull = Nothing
  convFromNullable var = Just (convFrom var)

instance ConversionFromPHPValue [(PHPSessionValue,PHPSessionValue)] where
  convFrom (PHPSessionValueArray var) = var
  convFrom v = mismatchError v "[(PHPSessionValue,PHPSessionValue)]"

instance ConversionFromPHPValue a => ConversionFromPHPValue [(a,a)] where
  convFrom (PHPSessionValueArray var) = map (\(va,vb) -> (convFrom va, convFrom vb)) var
  convFrom v = mismatchError v "ConversionFromPHPValue a => ConversionFromPHPValue [(a,a)]"

instance ConversionFromPHPValue Bool where
  convFrom (PHPSessionValueBool var) = var
  convFrom v = mismatchError v "Bool"

instance ConversionFromPHPValue Double where
  convFrom (PHPSessionValueFloat (Left var)) = fromIntegral var
  convFrom (PHPSessionValueFloat (Right var)) = var
  convFrom v = mismatchError v "Double"

instance ConversionFromPHPValue Int where
  convFrom (PHPSessionValueInt var) = var
  convFrom v = mismatchError v "Int"

instance ConversionFromPHPValue Int32 where
  convFrom (PHPSessionValueInt var) = fromIntegral var
  convFrom v = mismatchError v "Int32"

instance ConversionFromPHPValue Int64 where
  convFrom (PHPSessionValueInt var) = fromIntegral var
  convFrom v = mismatchError v "Int64"

instance ConversionFromPHPValue (PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)]) where
  convFrom (PHPSessionValueObject cls arr) = (cls,arr)
  convFrom v = mismatchError v "(PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)])"

instance ConversionFromPHPValue (PHPSessionClassName, LBS.ByteString) where
  convFrom (PHPSessionValueObjectSerializeable cls arr) = (cls,arr)
  convFrom v = mismatchError v "(PHPSessionClassName, ByteString)"

instance ConversionFromPHPValue LBS.ByteString where
  convFrom (PHPSessionValueString var) = var
  convFrom v = mismatchError v "ByteString"

instance ConversionFromPHPValue BS.ByteString where
  convFrom (PHPSessionValueString var) = (BS.concat . LBS.toChunks) var
  convFrom v = mismatchError v "ByteString"

mismatchError v totype =
  error $ "Type mismatch converting from (" ++ show v ++ ") to " ++ totype
