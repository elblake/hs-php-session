{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Data.PHPSession
-- Copyright: (c) 2013-2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Encodes and decodes serialized PHP sessions in the format used by the \"php\" setting
-- for serialize_handler.
-- 

module Data.PHPSession.Types (
    -- * PHP session types
    PHPSessionVariableList,
    PHPSessionClassName (..),
    PHPSessionValue (..),
    PHPSessionAttr (..)
) where

import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Holds the "top-level" session variables and their value contents.
type PHPSessionVariableList = [(LBS.ByteString, PHPSessionValue)]

data PHPSessionClassName =
  PHPSessionClassName LBS.ByteString

instance Show PHPSessionClassName where
  show (PHPSessionClassName str) = "PHPSessionClassName " ++ show str

instance Eq PHPSessionClassName where
  PHPSessionClassName a == PHPSessionClassName b = a == b

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

