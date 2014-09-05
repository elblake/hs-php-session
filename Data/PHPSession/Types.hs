{-# LANGUAGE OverloadedStrings #-}
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

module Data.PHPSession.Types (
    -- * PHP session types
    PHPSessionVariableList,
    PHPSessionClassName (..),
    PHPSessionValue (..),
    PHPSessionAttr (..)
) where

import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Holds the \"top-level\" session variables and their value contents.
type PHPSessionVariableList = [(LBS.ByteString, PHPSessionValue)]

-- | Represents the name of a PHP class.
data PHPSessionClassName =
  PHPSessionClassName LBS.ByteString

instance Show PHPSessionClassName where
  show (PHPSessionClassName str) = "PHPSessionClassName " ++ show str

instance Eq PHPSessionClassName where
  PHPSessionClassName a == PHPSessionClassName b = a == b

-- | 'PHPSessionValue' Represents a PHP value, which may be a number, string,
-- array, object, boolean, null, or references.
--
-- * 'PHPSessionValueArray' represents an array as a list of key-value pairs
--   of values of type 'PHPSessionValue'.
--
-- * 'PHPSessionValueObject' is similar to 'PHPSessionValueArray' but also 
--   includes a class name of type 'PHPSessionClassName'.
--
-- * 'PHPSessionValueObjectSerializeable' represent objects of which their classes
--   implement Serializeable to handle their own serialization and don't use
--   the normal serialization format for its contained objects.
--
-- * 'PHPSessionValueBool', 'PHPSessionValueInt', 'PHPSessionValueFloat',
--   'PHPSessionValueNull', and 'PHPSessionValueString' represent basic types
--   boolean, integer, floats, null and string respectively.
-- 
-- * 'PHPSessionValueFloat' stores the number representation as an 'Either' 'Int' 'Double'
--   to preserve instances where the number representation is actually an integer.
--   It should be noted that the re-encoded value is usually rounded unlike PHP's
--   representation.
--
-- * 'PHPSessionValueMisc' stores a few other types such as references and
--   values which follow the general serialization format but aren't recognized
--   by the decoder. A list of 'PHPSessionAttr' provides the information for
--   reconstructing the serialized representation when re-encoding this type
--   of value.
--
data PHPSessionValue =
  PHPSessionValueArray [(PHPSessionValue,PHPSessionValue)] |
  PHPSessionValueBool Bool |
  PHPSessionValueFloat (Either Int Double) |
  PHPSessionValueInt Int |
  PHPSessionValueNull |
  PHPSessionValueObject PHPSessionClassName [(PHPSessionValue,PHPSessionValue)] |
  PHPSessionValueObjectSerializeable PHPSessionClassName LBS.ByteString |
  PHPSessionValueString LBS.ByteString |
  PHPSessionValueMisc LBS.ByteString [PHPSessionAttr]

instance Show PHPSessionValue where
  show (PHPSessionValueArray arlst) = "PHPSessionValueArray " ++ show arlst
  show (PHPSessionValueBool b) = "PHPSessionValueBool " ++ show b
  show (PHPSessionValueFloat f) = "PHPSessionValueFloat " ++ show f
  show (PHPSessionValueInt i) = "PHPSessionValue " ++ show i
  show (PHPSessionValueNull) = "PHPSessionValueNull"
  show (PHPSessionValueObject cls xs) = "PHPSessionValueObject " ++ show cls ++ " " ++ show xs
  show (PHPSessionValueObjectSerializeable x xs) =
    "PHPSessionValueObjectSerializeable " ++ show x ++ " " ++ show xs
  show (PHPSessionValueString s) = "PHPSessionValueString " ++ show s
  show (PHPSessionValueMisc x xs) = "PHPSessionValueMisc " ++ show x ++ " " ++ show xs

instance Eq PHPSessionValue where
  PHPSessionValueArray a == PHPSessionValueArray b = a == b
  PHPSessionValueBool a == PHPSessionValueBool b = a == b
  PHPSessionValueFloat a == PHPSessionValueFloat b = a == b
  PHPSessionValueInt a == PHPSessionValueInt b = a == b
  PHPSessionValueNull == PHPSessionValueNull = True
  PHPSessionValueObject a b == PHPSessionValueObject c d = a == c && b == d
  PHPSessionValueObjectSerializeable a b == PHPSessionValueObjectSerializeable c d = a == c && b == d
  PHPSessionValueString a == PHPSessionValueString b = a == b
  PHPSessionValueMisc a b == PHPSessionValueMisc c d = False
  _ == _ = False
  
instance Ord PHPSessionValue where
  PHPSessionValueArray a <= PHPSessionValueArray b = a <= b
  PHPSessionValueBool a <= PHPSessionValueBool b = a <= b
  PHPSessionValueFloat a <= PHPSessionValueFloat b = a <= b
  PHPSessionValueInt a <= PHPSessionValueInt b = a <= b
  PHPSessionValueNull <= PHPSessionValueNull = True
  PHPSessionValueObject (PHPSessionClassName a) b <= PHPSessionValueObject (PHPSessionClassName c) d = a <= c && b <= d
  PHPSessionValueObjectSerializeable (PHPSessionClassName a) b <= PHPSessionValueObjectSerializeable (PHPSessionClassName c) d = a <= c && b <= d
  PHPSessionValueString a <= PHPSessionValueString b = a <= b
  PHPSessionValueMisc a b <= PHPSessionValueMisc c d = False
  _ <= _ = False
  
-- | 'PHPSessionAttr' are values associated with 'PHPSessionValueMisc' to inspect and
-- generally re-encode the necessary information for that value.
-- 
data PHPSessionAttr =
  PHPSessionAttrInt Int |
  PHPSessionAttrFloat Double |
  PHPSessionAttrNested [PHPSessionValue]

instance Show PHPSessionAttr where
  show (PHPSessionAttrInt n) = "PHPSessionAttrInt " ++ show n
  show (PHPSessionAttrFloat n) = "PHPSessionAttrFloat " ++ show n
  show (PHPSessionAttrNested ns) = "PHPSessionAttrNested " ++ show ns

instance Eq PHPSessionAttr where
  PHPSessionAttrInt a == PHPSessionAttrInt b = a == b
  PHPSessionAttrFloat a == PHPSessionAttrFloat b = a == b
  PHPSessionAttrNested a == PHPSessionAttrNested b = a == b
  _ == _ = False

