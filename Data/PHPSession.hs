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
-- for session.serialize_handler.
-- 

module Data.PHPSession (
    -- * Decode from 'ByteString'
    decodePHPSession,
    decodePHPSessionValue,
    -- * Encode to 'ByteString'
    encodePHPSession,
    encodePHPSessionValue,
    -- * Decode only part of a 'ByteString'
    decodePartialPHPSessionValue,
    -- * PHP session types
    PHPSessionVariableList,
    PHPSessionClassName (..),
    PHPSessionValue (..),
    PHPSessionAttr (..)
) where

import Data.PHPSession.Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L
import qualified Data.Char as C
import Data.List (foldl')

-- import qualified Text.Regex.Posix as TR

-- | Decodes a 'ByteString' containing a serialization of a list of session variables
-- using the \"php\" session serialization format into a 'PHPSessionVariableList'
decodePHPSession :: LBS.ByteString -> Maybe PHPSessionVariableList
decodePHPSession input =
  case decodePHPSessionEachTopLevel input [] of
    Nothing -> Nothing
    Just (everything,"") -> Just everything
    Just (_, _) -> Nothing

  where
    decodePHPSessionEachTopLevel input lst =
      case input of
        "" -> Just (reverse lst,"")
        _ -> do
          (name, rest) <- decodePHPSessionTopLevelVarName input
          (sym,  rest') <- decodePartialPHPSessionValue rest
          decodePHPSessionEachTopLevel rest' ((name, sym):lst)
    
    decodePHPSessionTopLevelVarName input = do
      (varname,n1) <- get_name_until_vertbar input
      (_,n2) <- get_vertbar n1
      return (varname, n2)
    get_name_until_vertbar input =
      case LBS.takeWhile (/= '|') input of
        l -> Just (l, LBS.drop (LBS.length l) input)
    get_vertbar input = case LBS.take 1 input of "|" -> Just ("|", LBS.drop 1 input); _ -> Nothing

  
-- Not exported and used by decodePartialPHPSessionValue
decodePartialPHPSessionValuesNested :: LBS.ByteString -> [PHPSessionValue] -> Maybe ([PHPSessionValue], LBS.ByteString)
decodePartialPHPSessionValuesNested input lst =
  case input of
    "" -> Just (reverse lst,"")
    _ ->
      case LBS.take 1 input of
        "}" ->
          Just (reverse lst, LBS.drop 1 input)
        _ -> do
          (sym,rest) <- decodePartialPHPSessionValue input
          decodePartialPHPSessionValuesNested rest (sym:lst)


-- | Decodes a 'ByteString' containing a session serialization of a value into a 
-- 'PHPSessionValue'. The format being decoded is similar if not probably the same 
-- format used by PHP's serialize/unserialize functions.
-- 'Nothing' is returned if the input bytestring could not be parsed correctly.
--
decodePHPSessionValue :: LBS.ByteString -> Maybe PHPSessionValue
decodePHPSessionValue input =
  case decodePartialPHPSessionValue input of
    Nothing -> Nothing
    Just (everything,"") -> Just everything
    Just (_, _) -> Nothing

-- | Decodes as much of a 'ByteString' as needed into a 'PHPSessionValue' and returns
-- the rest of the string. Decoding ends at either the end of the string or when the
-- extent of the current nested structure is met when an extra closing curly brace is
-- encountered. The format being decoded is similar if not probably the same 
-- format used by PHP's serialize/unserialize functions.
-- 'Nothing' is returned if the input bytestring could not be parsed correctly.
--
decodePartialPHPSessionValue :: LBS.ByteString -> Maybe (PHPSessionValue, LBS.ByteString)
decodePartialPHPSessionValue "" = Nothing
decodePartialPHPSessionValue input =
  let sertype = LBS.take 1 input
      rest = LBS.drop 1 input
   in case sertype of
        -- "Object implementing Serializeable" (C)
        -- ex: hi|C:9:"ClassName":5:{harr}}
        "C" -> do
          (_classnamelen,cls',numrest) <-
                dec_colon_integer_colon_dquote_classname_dquote_colon rest
          --case rest TR.=~ (":[0-9]+:\"[A-Za-z0-9_]+\":" :: LBS.ByteString) :: (LBS.ByteString,LBS.ByteString,LBS.ByteString) of
          --  ("",cls,numrest) ->
          case LBS.span (/=':') numrest of
                (num, colrest) ->
                  -- let [_,cls',_] = LBS.split '"' cls
                  let num' = read (LBS.unpack num)
                      (dat,rest') = LBS.splitAt num' $ LBS.drop 2 colrest
                      rest'' = LBS.drop 1 rest' 
                   in Just (PHPSessionValueObjectSerializeable (PHPSessionClassName cls') dat, rest'')
                
        -- Object (O) ex: hi|O:9:"ClassName":2:{s:4:"blah";i:1;s:3:"str";s:6:"string";}
        "O" -> do
          (_,cls',attrest) <- 
                dec_colon_integer_colon_dquote_classname_dquote rest
          -- case rest TR.=~ (":[0-9]+:\"[A-Za-z0-9_]+\"" :: LBS.ByteString) :: (LBS.ByteString,LBS.ByteString,LBS.ByteString) of
          let (l,rest') = decodePartialPHPSessionAttr attrest []
               in case l of
                    Nothing -> Nothing -- , input)
                    Just [PHPSessionAttrInt _, PHPSessionAttrNested vals] ->
                      let (al, bl) = L.partition (odd . snd) (zip vals [1..])
                          arlst = map (\[a,b] -> (a,b)) $ L.transpose [ map (\(a,_)->a) al,  map (\(a,_)->a) bl ]
                       in Just ((PHPSessionValueObject (PHPSessionClassName cls') arlst),rest')
                   
        -- String (s) ex: hi|s:6:"string";
        "s" -> do
          (len, strrest) <- dec_colon_integer_colon_dquote rest
          --case rest TR.=~ (":[0-9]+:\"" :: LBS.ByteString) :: (LBS.ByteString,LBS.ByteString,LBS.ByteString) of
          --  ("",strlen,strrest) ->
          --    let [_,len,_] = LBS.split ':' strlen
          let len' = read (LBS.unpack len)
              (str,rest') = LBS.splitAt len' $ strrest
              rest'' = LBS.drop 2 rest' 
           in Just (PHPSessionValueString str,rest'')
               
        _ ->
          let (l,rest') = decodePartialPHPSessionAttr rest []
           in case (sertype, l) of
                -- Array (a) ex: hi|a:3:{i:0;i:1;i:1;i:2;i:2;s:3:"abc";}
                ("a",Just [PHPSessionAttrInt _, PHPSessionAttrNested vals]) -> 
                  let (al, bl) = L.partition (odd . snd) (zip vals [1..])
                      arlst = map (\[a,b] -> (a,b)) $ L.transpose [ map (\(a,_)->a) al,  map (\(a,_)->a) bl ]
                   in Just (PHPSessionValueArray arlst, rest')
                
                -- Boolean (b) ex: hi|b:0; = FALSE -- hi|b:1; = TRUE
                ("b",Just [PHPSessionAttrInt 1]) -> Just (PHPSessionValueBool True,rest')
                ("b",Just [PHPSessionAttrInt 0]) -> Just (PHPSessionValueBool False,rest')
                
                -- Float (d) ex: hi|d:0.1000000000000000055511151231257827021181583404541015625;
                ("d",Just [PHPSessionAttrFloat num]) -> Just ((PHPSessionValueFloat $ Right num),rest')
                ("d",Just [PHPSessionAttrInt num]) -> Just ((PHPSessionValueFloat $ Left num),rest')
                
                -- Integer (i) ex: hi|i:26;
                ("i",Just [PHPSessionAttrInt num]) -> Just (PHPSessionValueInt num,rest')
                
                -- Null (N) ex: hi|N;
                ("N",Just []) -> Just (PHPSessionValueNull,rest')
                
                -- Recursive values and references (r), (R), TODO: PHP 6 encoded String (S)
                _ -> case l of
                       Just l' -> Just ((PHPSessionValueMisc sertype l'),rest')
                       Nothing -> Nothing -- , input)
  where

    decodePartialPHPSessionAttr :: LBS.ByteString -> [PHPSessionAttr] -> (Maybe [PHPSessionAttr],LBS.ByteString)
    decodePartialPHPSessionAttr input l =
      case LBS.take 1 input of
        ";" ->
          (Just $ reverse l,LBS.drop 1 input)
        _ ->
          case dec_colon_and_number input of
            Just (num,rest) ->
          --case input TR.=~ (":[-0-9.]+" :: LBS.ByteString) :: (LBS.ByteString,LBS.ByteString,LBS.ByteString) of
          --  ("",num,rest) ->
              let num' = LBS.unpack $ num -- LBS.drop 1 
               in case LBS.elem '.' num of
                    True ->
                      decodePartialPHPSessionAttr rest ((PHPSessionAttrFloat $ read $ num'):l)
                    False ->
                      decodePartialPHPSessionAttr rest ((PHPSessionAttrInt $ read $ num'):l)
            Nothing ->
              case dec_colon_and_open_curly input of
                Just (":{", rest) ->
                --  case input TR.=~ (":{" :: LBS.ByteString) :: (LBS.ByteString,LBS.ByteString,LBS.ByteString) of
                --    ("",":{",rest) ->
                      let Just (sub,rest') = decodePartialPHPSessionValuesNested rest []
                       in (Just $ reverse (PHPSessionAttrNested sub:l),rest')
                Nothing ->
                  case dec_colon_uppercase_letters input of
                    Just ("NAN", rest)  -> decodePartialPHPSessionAttr rest ((PHPSessionAttrFloat $  0/0):l)
                    Just ("INF", rest)  -> decodePartialPHPSessionAttr rest ((PHPSessionAttrFloat $  1/0):l)
                    Just ("-INF", rest) -> decodePartialPHPSessionAttr rest ((PHPSessionAttrFloat $ -1/0):l)
                    Nothing ->
                      error $ show input
                     -- (Nothing, input)
                  --    error "No good"
    dec_colon_integer_colon_dquote_classname_dquote_colon input = do
      (_0,n0) <- dec_get_colon input
      (len,n1) <- dec_get_integer n0
      (_2,n2) <- dec_get_colon n1
      (_3,n3) <- dec_get_dquote n2
      (classname,n4) <- dec_get_alphanum n3
      (_5,n5) <- dec_get_dquote n4
      (_6,n6) <- dec_get_colon n5
      return (len,classname,n6)
    dec_colon_integer_colon_dquote_classname_dquote input = do
      (_0,n0) <- dec_get_colon input
      (len,n1) <- dec_get_integer n0
      (_2,n2) <- dec_get_colon n1
      (_3,n3) <- dec_get_dquote n2
      (classname,n4) <- dec_get_alphanum n3
      (_5,n5) <- dec_get_dquote n4
      return (len,classname,n5)
    dec_colon_integer_colon_dquote input = do
      (_0,n0) <- dec_get_colon input
      (len,n1) <- dec_get_integer n0
      (_2,n2) <- dec_get_colon n1
      (_3,n3) <- dec_get_dquote n2
      return (len,n3)
    dec_colon_and_number input = do
      (_0,n0) <- dec_get_colon input
      (num,n1) <- dec_get_number n0
      return (num,n1)
    dec_colon_and_open_curly input = do
      (_0,n0) <- dec_get_colon input
      (_1,n1) <- dec_get_openc n0
      return (":{", n1)
    dec_colon_uppercase_letters input = do
      (_0,n0) <- dec_get_colon input
      (a1,n1) <- dec_get_uppercase_letters n0
      return (a1,n1)
    
    dec_one_or_more input a = case a of "" -> Nothing; l -> Just (l, LBS.drop (LBS.length l) input)
    dec_get_number input =
      case dec_get_neg_number input of
        Just result -> Just result
        Nothing ->
          dec_get_pos_number input
    dec_get_neg_number input = do
      (a0,n0) <- dec_get_dash input
      (a1,n1) <- dec_get_pos_number n0
      return (LBS.concat [a0,a1], n1)
    dec_get_pos_number input = do
      (a0,n0) <- dec_get_integer input
      case dec_get_dot n0 of
        Nothing -> return (a0,n0)
        Just (a1,n1) -> do
          (a2,n2) <- dec_get_integer n1
          return (LBS.concat [a0,a1,a2], n2)
    dec_get_alphanum input =
      dec_one_or_more input $ LBS.takeWhile (\a -> (C.isDigit a) || (C.isAsciiLower a) || (C.isAsciiUpper a) || (a == '_')) input
    dec_get_integer input =
      dec_one_or_more input $ LBS.takeWhile C.isDigit input
    dec_get_uppercase_letters input =
      case dec_get_neg_uppercase_letters input of
        Just result -> Just result
        Nothing ->
          dec_get_pos_uppercase_letters input
    dec_get_neg_uppercase_letters input = do
      (a0,n0) <- dec_get_dash input
      (a1,n1) <- dec_one_or_more n0 $ LBS.takeWhile C.isAsciiUpper n0
      return (LBS.concat [a0,a1], n1)
    dec_get_pos_uppercase_letters input =
      dec_one_or_more input $ LBS.takeWhile C.isAsciiUpper input
    dec_get_openc  input = case LBS.take 1 input of "{" -> Just ("{", LBS.drop 1 input); _ -> Nothing
    dec_get_dquote input = case LBS.take 1 input of "\"" -> Just ("\"", LBS.drop 1 input); _ -> Nothing
    dec_get_colon  input = case LBS.take 1 input of ":" -> Just (":", LBS.drop 1 input); _ -> Nothing
    dec_get_dash   input = case LBS.take 1 input of "-" -> Just ("-", LBS.drop 1 input); _ -> Nothing
    dec_get_dot    input = case LBS.take 1 input of "." -> Just (".", LBS.drop 1 input); _ -> Nothing

-- | Encode a 'PHPSessionVariableList' into a 'ByteString' containing the serialization
-- of a list of session variables using the \"php\" session serialization format.
encodePHPSession :: PHPSessionVariableList -> LBS.ByteString
encodePHPSession lst =
  encodePHPSessionTop lst []
encodePHPSessionTop lst outlst =
  case lst of
    [] -> LBS.concat $ reverse outlst
    (name,var):xs ->
      let out'  = (encodePHPSessionVarName name) : outlst
          out'' = (encodePHPSessionValue var) : out'
       in encodePHPSessionTop xs out''
  where
    encodePHPSessionVarName name =
      LBS.concat [name, "|"]

-- | Encode a 'PHPSessionValue' into a 'ByteString' containing the serialization of a
-- PHP value. The format being encoded into is similar if not probably the same 
-- format used by PHP's serialize/unserialize functions.
--
encodePHPSessionValue var =
  case var of
    PHPSessionValueObjectSerializeable (PHPSessionClassName cls) dat ->
      LBS.concat
        [
          "C:", LBS.pack $ show (LBS.length cls), ":\"", cls, "\":",
          LBS.pack $ show (LBS.length dat), ":{", dat, "}"
        ]
    PHPSessionValueArray arlst ->
      let nst = reverse $ foldl' (\l (k,v) -> (v:(k:l))) [] arlst
       in LBS.concat
            [
              "a", encodePHPSessionAttr (PHPSessionAttrInt $ length arlst),
                   encodePHPSessionAttr (PHPSessionAttrNested nst)
            ]
    PHPSessionValueBool b ->
      LBS.concat
        [
          "b", encodePHPSessionAttr (PHPSessionAttrInt $ if b then 1 else 0), ";"
        ]
    PHPSessionValueFloat d ->
      LBS.concat
        [
          "d", encodePHPSessionAttr $
          case d of
            Left i   -> PHPSessionAttrInt i;
            Right d' -> PHPSessionAttrFloat d',
          ";"
        ]
    PHPSessionValueInt i ->
      LBS.concat
        [
          "i", encodePHPSessionAttr (PHPSessionAttrInt i), ";"
        ]
    PHPSessionValueNull ->
      "N;"
    PHPSessionValueObject (PHPSessionClassName cls) proplst ->
      let nst = reverse $ foldl' (\l (k,v) -> (v:(k:l))) [] proplst
          cls' = BS.concat $ LBS.toChunks cls
       in LBS.concat
            [
              "O", encodePHPSessionAttr (PHPSessionAttrInt $ BS.length cls'),
                   ":\"", LBS.fromChunks [cls'], "\"",
                   encodePHPSessionAttr (PHPSessionAttrInt $ length proplst),
                   encodePHPSessionAttr (PHPSessionAttrNested nst)
            ]
    PHPSessionValueString str ->
      let str' = BS.concat $ LBS.toChunks str
       in LBS.concat
            [
              "s", encodePHPSessionAttr (PHPSessionAttrInt $ BS.length str'),
                   ":\"", LBS.fromChunks [str'], "\";"
            ]
    PHPSessionValueMisc sertype atts ->
      LBS.concat $ [sertype] ++ map encodePHPSessionAttr atts ++ [";"]
  where

    encodePHPSessionAttr att =
      case att of
        PHPSessionAttrInt i -> LBS.concat [":", LBS.pack $ show i]
        PHPSessionAttrFloat f | isNaN f -> ":NAN"
        PHPSessionAttrFloat f | isInfinite f ->
          if f < 0
            then ":-INF"
            else ":INF"
        PHPSessionAttrFloat f -> LBS.concat [":", LBS.pack $ show f]
        PHPSessionAttrNested vars ->
          LBS.concat $ [":{"] ++ map encodePHPSessionValue vars ++ ["}"]
