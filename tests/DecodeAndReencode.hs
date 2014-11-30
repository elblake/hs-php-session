--
-- Testing Data.PHPSession
-- 
{-# LANGUAGE OverloadedStrings #-}

import Data.PHPSession

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L
import Data.List (foldl')
import System.IO
import System.Exit (exitFailure)

--
-- Tests that should decode and re-encode
test1 = "v1|s:94:\" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}\";v2|a:3:{i:0;i:10;i:1;i:9;i:2;s:5:\"eight\";}v3|C:9:\"ClassName\":94:{ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}}v4|b:1;v5|i:10;v6|d:5.5;v7|N;"
test2 = "v1|a:3:{i:0;i:10;i:1;i:9;i:2;a:3:{i:0;i:8;i:1;i:7;i:2;a:3:{i:0;i:6;i:1;s:4:\"five\";i:2;d:4;}}}v2|O:9:\"ClassName\":2:{s:4:\"blah\";i:1;s:3:\"str\";s:94:\" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}\";}"
test3 = "v1|s:256:\"00000000000000000000000000000000 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\";"
test4 = "teststr1|s:0:\"\";test_array|a:1:{i:0;a:2:{s:4:\"ABCD\";s:9:\"987654321\";s:0:\"\";b:1;}}test_int|i:1;teststr2|s:4:\"1234\";"
test5 = "testfloat1|d:1.00001121;testfloat2|d:NAN;testfloat3|d:INF;testfloat4|d:-INF;"

tests_that_decodes_and_reencodes =
  [ ("test1", test1), ("test2", test2), ("test3", test3), ("test4", test4), ("test5", test5) ]

--
-- Tests that shouldn't decode.
test1b1 = "v1|s:94:\"1 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}\";v2|a:3:{i:0;i:10;i:1;i:9;i:2;s:5:\"eight\";}v3|C:9:\"ClassName\":94:{ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}}v4|b:1;v5|i:10;v6|d:5.5;v7|N;"
test2b1 = "v1|a:3:{i:0;i:10;i:1;i:9;i:2;a:3:{i:0;i:8;i:1;i:7;i:2;a:3:{i:0;i:6;i:1;s:4:\"ffive\";i:2;d:4;}}}v2|O:9:\"ClassName\":2:{s:4:\"blah\";i:1;s:3:\"str\";s:94:\" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}\";}"
test3b1 = "v1|s:256::\"00000000000000000000000000000000 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\";"
test4b1 = "teststr1|s:0\"\";test_array|a:1:{i:0;a:2:{s:4:\"ABCD\";s:9:\"987654321\";s:0:\"\";b:1;}}test_int|i:1;teststr2|s:4:\"1234\";"
test5b1 = "testfloat1|d::1.00001121;testfloat2|d:NAN;testfloat3|d:INF;testfloat4|d:-INF;"
test5b2 = "testfloat1|do:1.00001121;testfloat2|d:NAN;testfloat3|d:INF;testfloat4|d:-INF;"
test5b3 = "testfloat1|d1.00001121;testfloat2|d:NAN;testfloat3|d:INF;testfloat4|d:-INF;"

tests_that_shouldnt_decode =
  [ ("test1b1", test1b1), 
    ("test2b1", test2b1),
    ("test3b1", test3b1),
    ("test4b1", test4b1),
    ("test5b1", test5b1), ("test5b2", test5b2), ("test5b3", test5b3) ]

main = testcases

testcases :: IO ()
testcases = do
  result <- testcasesWith (\message -> putStrLn message) -- stderr
  if result
    then return ()
    else exitFailure

testcasesWith :: (String -> IO ()) -> IO Bool
testcasesWith issue = do
  let tresults  = map testDecodeAndReencode tests_that_decodes_and_reencodes
      fresults1 = map testDecodeAndReencode tests_that_shouldnt_decode
      fresults2 = map testDecodeOnly        tests_that_shouldnt_decode
  tresults' <- mapM (issueIfDidntDecodedAndReencoded issue) tresults
  fresults1' <- mapM (issueIfDecodedAndReencodedWhenItShouldnt issue) fresults1
  fresults2' <- mapM (issueIfDecodedAndReencodedWhenItShouldnt issue) fresults2
  return $ and tresults' && ((or fresults1') /= True) && ((or fresults2') /= True)
  where
    testDecodeAndReencode (name, test) = 
      case decodePHPSessionEither test of
        Left err -> Left (name, show err)
        Right dec ->
          let result = encodePHPSession dec
           in if result == test
                then Right (name, test, dec, result)
                else Left (name, "Not same: " ++ (LBS.unpack test) ++ "  and  " ++ (LBS.unpack result))
    testDecodeOnly (name, test) =
      case decodePHPSessionEither test of
        Left err -> Left (name, (show err))
        Right dec ->
          Right (name, test, dec, LBS.pack "")

issueIfDidntDecodedAndReencoded issue a =
  case a of
    Left (name, message) -> do
      issue $ "Failed test: failed to decode:"
      issue $ "Test name: " ++ name
      issue $ "Decoding error: " ++ message
      exitFailure -- return False
    Right (name, test, decoded, reencoded) -> return True

issueIfDecodedAndReencodedWhenItShouldnt issue a =
  case a of
    Right (name, test, decoded, reencoded) -> do
      issue $ "Failed test: Decoded when it shouldnt:"
      issue $ "Test name: " ++ name
      issue $ "Test: " ++ (show test)
      issue $ "Decoded: " ++ (show decoded)
      if reencoded /= ""
        then issue $ "Re-encoded: " ++ (show reencoded)
        else issue $ "Could not re-encode"
      exitFailure -- return False
    Left (name, message) -> return False

