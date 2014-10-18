{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Data.PHPSession.ImplicitConv.ConvBool
-- Copyright: (c) 2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Non-coerced and coerced rule sets for converting 'PHPSessionValue' objects to 'Bool'.
-- 
module Data.PHPSession.ImplicitConv.ConvBool (
    -- * Non-coerced from 'PHPSessionValue'
    boolDefaultFalseFrom,
    boolDefaultTrueFrom,
    -- * Type coercion from 'PHPSessionValue'
    boolFromPHPLooseComparisonWithTrue,
    boolFromReducedLooseCoercionSafe,
    boolFromReducedLooseCoercion,
    boolFromESBooleanCoercionRules,
    boolFromPerlBooleanCoercionRules,
    boolFromPythonBooleanCoercionRules,
    boolFromLuaBooleanCoercionRules
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.PHPSession.Types

-- | Returns the value of a boolean value as a 'Bool', or returns False as the default
-- 'Bool' value if the 'PHPSessionValue' is not a boolean type. This function is
-- similar to the \"strict comparison\" operator against the boolean @/TRUE/@ value.
--
boolDefaultFalseFrom :: PHPSessionValue -> Bool
boolDefaultFalseFrom var =
  case var of
    PHPSessionValueBool b -> b
    _ -> False

-- | Returns the value of a boolean value as a 'Bool', or returns True as the default
-- 'Bool' value if the 'PHPSessionValue' is not a boolean type.
-- 
boolDefaultTrueFrom :: PHPSessionValue -> Bool
boolDefaultTrueFrom var =
  case var of
    PHPSessionValueBool b -> b
    _ -> True

-- | Coerces a 'PHPSessionValue' to a 'Bool' based on PHP's conversion rules to
-- convert arbitrary values to boolean for evaluation. This function's behaviour
-- is also reminiscent of PHP's \"loose comparison\" operator against boolean
-- @/TRUE/@.
-- <http://php.net/manual/language.types.boolean.php>
-- 
-- Values that result in 'True' are @/TRUE/@, non-zero numbers, non-empty strings
-- other than \"0\", objects, and non-empty arrays.
-- 
-- A conversion documented in the PHP manual involving @SimpleXML@ objects that
-- coerce to @/FALSE/@ instead of @/TRUE/@ when created with empty tags is not
-- implemented by this function, as SimpleXML objects cannot be serialized directly
-- and so don't have a valid serializable form to convert from in any case.
--
-- Because of the range of valid values of the \"loose comparison\" operator, this
-- function and variations based on other dynamically typed languages is provided 
-- mainly for circumstances where they may be the best fit to map particular data
-- values to a boolean truth value system. 'boolFromReducedLooseCoercionSafe'
-- provides a significantly reduced subset of valid values to coerce from for the
-- purpose of determining a \"confirmation\" value, as opposed to simply determining
-- if a value exists or not.
--
boolFromPHPLooseComparisonWithTrue :: PHPSessionValue -> Bool
boolFromPHPLooseComparisonWithTrue var =
  case var of
    PHPSessionValueBool b -> b
    PHPSessionValueArray arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueFloat (Left i) -> (i /= 0)
    PHPSessionValueFloat (Right f) -> (f /= 0.0)
    PHPSessionValueInt i -> (i /= 0)
    PHPSessionValueNull -> False
    PHPSessionValueString str ->
      case str of
        ""  -> False
        "0" -> False
        _   -> True
    PHPSessionValueObject _ _ -> True
    PHPSessionValueObjectSerializeable _ _ -> True
    PHPSessionValueMisc _ _ -> True

-- | Coerces a 'PHPSessionValue' to a 'Bool' through a reduced subset of valid values.
-- 'boolFromReducedLooseCoercionSafe' and 'boolFromReducedLooseCoercion' are
-- functions intended for testing a variable for a boolean \"confirmation\" from a
-- limited number of probable representations.
--
-- Values that result in 'True' are @/TRUE/@, non-zero numbers, \"1\" and \"-1\".
-- Values that result in 'False' are @/FALSE/@, zero numbers, @/NULL/@, \"0\" and \"\".
-- Values such as arrays, objects and arbitrary strings, are invalid with this
-- function and return an error. In the case of 'boolFromReducedLooseCoercionSafe'
-- this returns a 'Left' with the error message.
--
boolFromReducedLooseCoercionSafe :: PHPSessionValue -> Either String Bool
boolFromReducedLooseCoercionSafe var =
  case var of
    PHPSessionValueArray _ -> mismatchError var "Bool"
    PHPSessionValueBool b -> Right b
    PHPSessionValueFloat (Left i) -> Right (i /= 0)
    PHPSessionValueFloat (Right f) -> Right (f /= 0)
    PHPSessionValueInt i -> Right (i /= 0)
    PHPSessionValueNull -> Right False
    PHPSessionValueObject _ _ -> mismatchError var "Bool"
    PHPSessionValueObjectSerializeable _ _ -> mismatchError var "Bool"
    PHPSessionValueString str ->
      case str of
        "1"  -> Right True
        "-1" -> Right True
        "0"  -> Right False
        ""   -> Right False
        _    -> mismatchError var "Bool"
    PHPSessionValueMisc _ _ -> mismatchError var "Bool"

mismatchError v totype =
  Left $ "Type mismatch converting from (" ++ show v ++ ") to " ++ totype


-- | A version of 'boolFromReducedLooseCoercionSafe' that may throw exceptions.
--
boolFromReducedLooseCoercion :: PHPSessionValue -> Bool
boolFromReducedLooseCoercion var =
    case boolFromReducedLooseCoercionSafe var of
      Left message -> error message
      Right var' -> var'

-- | Alternative boolean coercion based on the JS/ES boolean conversion rules. For
-- these conversion rules, refer to the description in section 9.2 of the ECMA 262 
-- standard. A particular distinction of this function's coercion behaviour
-- compared to the other functions is that floating point NaN returns 'False'
-- instead of 'True'.
--
-- PHP objects, arrays and objects implementing Serializable are all treated as
-- @Object@ for the purpose of this function.
--
boolFromESBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromESBooleanCoercionRules var =
  case var of
    PHPSessionValueArray arr -> True
    PHPSessionValueBool b -> b
    PHPSessionValueFloat (Left i) -> i /= 0
    PHPSessionValueFloat (Right f) ->
      case f of
        0 -> False
        _ | isNaN f -> False
        _ -> True
    PHPSessionValueInt i -> i /= 0
    PHPSessionValueNull -> False
    PHPSessionValueObject _ arr -> True
    PHPSessionValueObjectSerializeable _ bstr -> True
    PHPSessionValueString str ->
      case str of
        ""  -> False
        _   -> True
    PHPSessionValueMisc _ _ -> True

-- | Alternative boolean coercion based on non-overloaded Perl 5 rules. For these
-- conversion rules, refer to the description in the section \"Truth and Falsehood\"
-- of \"perlsyn\".
-- <http://perldoc.perl.org/perlsyn.html#Truth-and-Falsehood>
--
-- The difference between the coercion rules in Perl and Python's is that the string
-- \"0\" is 'False' in Perl, and 'True' in Python.
--
-- PHP objects and arrays are treated as lists and the serialized byte sequences of 
-- objects implementing Serializable are treated as strings for the purpose of this
-- function.
--
boolFromPerlBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromPerlBooleanCoercionRules var =
  case var of
    PHPSessionValueArray arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueBool b -> b
    PHPSessionValueFloat (Left i) -> i /= 0
    PHPSessionValueFloat (Right f) -> f /= 0
    PHPSessionValueInt i -> i /= 0
    PHPSessionValueNull -> False
    PHPSessionValueObject _ arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueObjectSerializeable _ bstr ->
      case bstr of
        "0" -> False
        ""  -> False
        _   -> True
    PHPSessionValueString str ->
      case str of
        "0" -> False
        ""  -> False
        _   -> True
    PHPSessionValueMisc _ _ -> True

-- | Alternative boolean coercion based on Python rules. For these conversion rules
-- refer to the description in \"Truth Value Testing\" in section 3.1 of Python
-- 2.5's Library Reference.
-- <https://docs.python.org/release/2.5.2/lib/truth.html>
--
-- PHP objects are treated as maps and the byte sequences of objects that implement
-- Serializable are treated as strings for the purpose of this function.
--
-- This convenience function does not replicate the behaviour of user-defined class
-- instances that implement @__nonzero__()@ or @__len__()@. Which would be
-- functionality that PHP objects would not implement anyway.
--
boolFromPythonBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromPythonBooleanCoercionRules var =
  case var of
    PHPSessionValueArray arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueBool b -> b
    PHPSessionValueFloat (Left i) -> i /= 0
    PHPSessionValueFloat (Right f) -> f /= 0
    PHPSessionValueInt i -> i /= 0
    PHPSessionValueNull -> False
    PHPSessionValueObject _ arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueObjectSerializeable _ bstr ->
      case bstr of
        "" -> False
        _  -> True
    PHPSessionValueString str ->
      case str of
        "" -> False
        _  -> True
    PHPSessionValueMisc _ _ -> True

-- | Alternative boolean coercion based on Lua rules, all values are 'True' except
-- @/NULL/@ and @/FALSE/@. For these conversion rules, refer to the passage in
-- section 2.4.4 \"Control Structures\" of Lua 5.1's Manual.
-- <http://www.lua.org/manual/5.1/manual.html>
--
boolFromLuaBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromLuaBooleanCoercionRules var =
  case var of
    PHPSessionValueArray _ -> True
    PHPSessionValueBool b  -> b
    PHPSessionValueFloat _ -> True
    PHPSessionValueInt i   -> True
    PHPSessionValueNull    -> False
    PHPSessionValueObject _ _ -> True
    PHPSessionValueObjectSerializeable _ _ -> True
    PHPSessionValueString str -> True
    PHPSessionValueMisc _ _   -> True


