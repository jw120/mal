{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module ReaderSpec (spec) where

import Data.Text (isInfixOf, Text)

import Test.Hspec

import Reader

-- Helper function to shorten tests
test :: Text -> AST -> Expectation
test t u = malRead t `shouldBe` Right (Just u)
testNothing :: Text -> Expectation
testNothing t = malRead t `shouldBe` Right Nothing
isErrorMatching :: Text -> Either Text (Maybe AST) -> Bool
isErrorMatching x (Left t) = x `isInfixOf` t
isErrorMatching _ (Right _) = False
keyword :: Text -> AST
keyword t = ASTStringLit (keywordPrefix <> t)

spec :: Spec
spec = do
  describe "Reader function" $ do
    it "parses an integer" $ do
      malRead "123" `shouldBe` Right (Just (ASTIntLit 123))
    it "parses a signed integer" $ do
      malRead "-1234" `shouldBe` Right (Just (ASTIntLit (-1234)))
    it "parses an integer with leading space" $ do
      malRead "   123" `shouldBe` Right (Just (ASTIntLit 123))
    it "parses an integer with trailing space" $ do
      malRead "123 " `shouldBe` Right (Just (ASTIntLit 123))
    it "parses a string literal" $ do
      malRead "\"xyz\"" `shouldBe` Right (Just (ASTStringLit "xyz"))
    it "parses a string literal with an escaped quote" $ do
      malRead "\"x\\\"yz\"" `shouldBe` Right (Just (ASTStringLit "x\"yz"))
    it "parses a normal alphabetic atom" $ do
      malRead "abc" `shouldBe` Right (Just (ASTSymbol "abc"))
    it "handles comments" $ do
      malRead "(+ ; comment\n3)" `shouldBe` Right (Just (ASTList [ASTSymbol "+", ASTIntLit 3]))
    it "handles empty input" $ do
      malRead "" `shouldBe` Right Nothing
    it "detects mismatched parens" $ do
      malRead "(+ 1 2" `shouldSatisfy` isErrorMatching "end of input"

  describe "standard step1 tests" $ do
    it "Testing read of numbers" $ do
      test "1" $ ASTIntLit 1
      test "   7   " $ ASTIntLit 7
      test "-123" $ ASTIntLit (-123)
    it "Testing read of symbols" $ do
      test "+" $ ASTSymbol "+"
      test "abc" $ ASTSymbol "abc"
      test "   def  " $ ASTSymbol "def"
      test "abc5" $ ASTSymbol "abc5"
      test "abc-def" $ ASTSymbol "abc-def"
    it "Testing non-numbers starting with a dash." $ do
      test "-" $ ASTSymbol "-"
      test "-abc" $ ASTSymbol "-abc"
      test "->>" $ ASTSymbol "->>"
    it "Testing read of lists" $ do
      test "(+ 1 2)" $ ASTList [ASTSymbol "+",ASTIntLit 1, ASTIntLit 2]
      test "()" $ ASTList []
      test "( )" $ ASTList []
      test "(nil)" $ ASTList [ASTSpecialLit MalNil]
      test "((3 4))" $
        ASTList [ASTList [ASTIntLit 3, ASTIntLit 4]]
      test "(+ 1 (+ 2 3))" $
        ASTList [ASTSymbol "+", ASTIntLit 1 , ASTList [ASTSymbol "+", ASTIntLit 2, ASTIntLit 3]]
      test "  ( +   1   (+   2 3   )   )  " $
        ASTList [ASTSymbol "+", ASTIntLit 1 , ASTList [ASTSymbol "+", ASTIntLit 2, ASTIntLit 3]]
      test "(* 1 2)" $
        ASTList [ASTSymbol "*", ASTIntLit 1, ASTIntLit 2]
      test "(** 1 2)" $
        ASTList [ASTSymbol "**", ASTIntLit 1, ASTIntLit 2]
      test "(* -3 6)" $
        ASTList [ASTSymbol "*", ASTIntLit (-3), ASTIntLit 6]
      test "(()())" $
        ASTList [ASTList[], ASTList[]]
    it "Test commas as whitespace" $ do
      test "(1 2, 3,,,,),," $
        ASTList [ASTIntLit 1, ASTIntLit 2, ASTIntLit 3]
    it "Testing read of nil/true/false" $ do
      test "nil" $ ASTSpecialLit MalNil
      test "true" $ ASTSpecialLit MalTrue
      test "false" $ ASTSpecialLit MalFalse
    it "Testing read of comments" $ do
       testNothing "  ;; whole line comment (not an exception)"
       test " 1 ; comment after expression" $ ASTIntLit 1
       test "1; comment after expression" $ ASTIntLit 1
    it "Testing read of quoting" $ do
      test "'1" $ ASTList [ASTSymbol "quote", ASTIntLit 1]
      test "'(1 2 3)" $ ASTList [ASTSymbol "quote", ASTList [ASTIntLit 1, ASTIntLit 2, ASTIntLit 3]]
      test "`1" $ ASTList [ASTSymbol "quasiquote", ASTIntLit 1]
      test "`(1 2 3)" $ ASTList [ASTSymbol "quasiquote", ASTList [ASTIntLit 1, ASTIntLit 2, ASTIntLit 3]]
      test "~1" $ ASTList [ASTSymbol "unquote", ASTIntLit 1]
      test "~(1 2 3)" $ ASTList [ASTSymbol "unquote", ASTList [ASTIntLit 1, ASTIntLit 2, ASTIntLit 3]]
      test "~@(1 2 3)" $ ASTList [ASTSymbol "splice-unquote", ASTList [ASTIntLit 1, ASTIntLit 2, ASTIntLit 3]]
    it "Testing keywords" $ do
      test ":kw" $ keyword "kw"
      test "(:kw1 :kw2 :kw3)" $
        ASTList [keyword "kw1", keyword "kw2", keyword "kw3"]
    it "Testing read of vectors" $ do
      test "[+ 1 2]" $ ASTVector [ASTSymbol "+", ASTIntLit 1, ASTIntLit 2]
      test "[]" $ ASTVector []
      test "[ ]" $ ASTVector []
      test "[[3 4]]" $ ASTVector [ASTVector [ASTIntLit 3, ASTIntLit 4]]
      test "[+ 1 [+ 2 3]]" $
        ASTVector [ASTSymbol "+", ASTIntLit 1, ASTVector [ASTSymbol "+", ASTIntLit 2, ASTIntLit 3]]
      test "  [ +   1   [+   2 3   ]   ]" $
        ASTVector [ASTSymbol "+", ASTIntLit 1, ASTVector [ASTSymbol "+", ASTIntLit 2, ASTIntLit 3]]
      test "([])" $ ASTList [ASTVector []]
    it  "Testing read of hash maps" $ do
      test "{}" $ ASTMap []
      test "{ }" $ ASTMap []
      test "{\"abc\" 1}" $ ASTMap [ASTStringLit "abc", ASTIntLit 1]
      test "{\"a\" {\"b\" 2}}" $ ASTMap [ASTStringLit "a", ASTMap [ASTStringLit "b", ASTIntLit 2]]
      test "{\"a\" {\"b\" {\"c\" 3}}}" $
        ASTMap [ASTStringLit "a", ASTMap [ASTStringLit "b", ASTMap [ASTStringLit "c", ASTIntLit 3]]]
      test "      {  \"a\"  {\"b\"   {  \"cde\"     3   }  }}" $
        ASTMap [ASTStringLit "a", ASTMap [ASTStringLit "b", ASTMap [ASTStringLit "cde", ASTIntLit 3]]]
      test "{\"a1\" 1 \"a2\" 2 \"a3\" 3}" $ -- This may go out of order
        ASTMap [ASTStringLit "a1", ASTIntLit 1, ASTStringLit "a2", ASTIntLit 2, ASTStringLit "a3", ASTIntLit 3]
      test "{  :a  {:b   {  :cde     3   }  }}" $
        ASTMap [keyword "a", ASTMap [keyword "b", ASTMap [keyword "cde", ASTIntLit 3]]]
      test "{\"1\" 1}" $ ASTMap [ASTStringLit "1", ASTIntLit 1]
      test "({})" $ ASTList [ASTMap []]
    it "Testing read of ^/metadata" $ do
      test "^{\"a\" 1} [2 3]" $
        ASTList [ASTSymbol "with-meta",
          ASTVector [ASTIntLit 2, ASTIntLit 3],
          ASTMap [ASTStringLit "a", ASTIntLit 1]]

{-
        ;>>> deferrable=True

        ;;
        ;; -------- Deferrable Functionality --------
        ;; Testing read of strings
        "abc"
        ;=>"abc"
           "abc"
        ;=>"abc"
        "abc (with parens)"
        ;=>"abc (with parens)"
        "abc\"def"
        ;=>"abc\"def"
        ""
        ;=>""
        "\\"
        ;=>"\\"
        "\\\\\\\\\\\\\\\\\\"
        ;=>"\\\\\\\\\\\\\\\\\\"
        "&"
        ;=>"&"
        "'"
        ;=>"'"
        "("
        ;=>"("
        ")"
        ;=>")"
        "*"
        ;=>"*"
        "+"
        ;=>"+"
        ","
        ;=>","
        "-"
        ;=>"-"
        "/"
        ;=>"/"
        ":"
        ;=>":"
        ";"
        ;=>";"
        "<"
        ;=>"<"
        "="
        ;=>"="
        ">"
        ;=>">"
        "?"
        ;=>"?"
        "@"
        ;=>"@"
        "["
        ;=>"["
        "]"
        ;=>"]"
        "^"
        ;=>"^"
        "_"
        ;=>"_"
        "`"
        ;=>"`"
        "{"
        ;=>"{"
        "}"
        ;=>"}"
        "~"
        ;=>"~"

        ;; Testing reader errors
        (1 2
        ;/.*(EOF|end of input|unbalanced).*
        [1 2
        ;/.*(EOF|end of input|unbalanced).*

        ;;; These should throw some error with no return value
        "abc
        ;/.*(EOF|end of input|unbalanced).*
        "
        ;/.*(EOF|end of input|unbalanced).*
        "\"
        ;/.*(EOF|end of input|unbalanced).*
        "\\\\\\\\\\\\\\\\\\\"
        ;/.*(EOF|end of input|unbalanced).*
        (1 "abc
        ;/.*(EOF|end of input|unbalanced).*
        (1 "abc"
        ;/.*(EOF|end of input|unbalanced).*





        ;; Testing read of @/deref
        @a
        ;=>(deref a)

        ;>>> soft=True
        ;>>> optional=True
        ;;
        ;; -------- Optional Functionality --------

        ;; Testing read of ^/metadata
        ^{"a" 1} [1 2 3]
        ;=>(with-meta [1 2 3] {"a" 1})


        ;; Non alphanumerice characters in strings
        ;;; \t is not specified enough to be tested
        "\n"
        ;=>"\n"
        "#"
        ;=>"#"
        "$"
        ;=>"$"
        "%"
        ;=>"%"
        "."
        ;=>"."
        "\\"
        ;=>"\\"
        "|"
        ;=>"|"

        ;; Non alphanumeric characters in comments
        1;!
        ;=>1
        1;"
        ;=>1
        1;#
        ;=>1
        1;$
        ;=>1
        1;%
        ;=>1
        1;'
        ;=>1
        1;\
        ;=>1
        1;\\
        ;=>1
        1;\\\
        ;=>1
        1;`
        ;=>1
        ;;; Hopefully less problematic characters
        1; &()*+,-./:;<=>?@[]^_{|}~

        ;; FIXME: These tests have no reasons to be optional, but...
        ;; fantom fails this one
        "!"
        ;=>"!"
-}

