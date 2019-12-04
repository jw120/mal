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
keyword t = s (magicKeywordPrefix <> t)
i :: Int -> AST
i = ASTIntLit
s :: Text -> AST
s = ASTStringLit
list :: [AST] -> AST
list = ASTList
sym :: Text -> AST
sym = ASTSymbol

spec :: Spec
spec = do
  describe "Reader function" $ do
    it "parses an integer" $ do
      malRead "123" `shouldBe` Right (Just (i 123))
    it "parses a signed integer" $ do
      malRead "-1234" `shouldBe` Right (Just (i (-1234)))
    it "parses an integer with leading space" $ do
      malRead "   123" `shouldBe` Right (Just (i 123))
    it "parses an integer with trailing space" $ do
      malRead "123 " `shouldBe` Right (Just (i 123))
    it "parses a string literal" $ do
      malRead "\"xyz\"" `shouldBe` Right (Just (s "xyz"))
    it "parses a string literal with an escaped quote" $ do
      malRead "\"x\\\"yz\"" `shouldBe` Right (Just (s "x\"yz"))
    it "parses a normal alphabetic atom" $ do
      malRead "abc" `shouldBe` Right (Just (sym "abc"))
    it "handles comments" $ do
      malRead "(+ ; comment\n3)" `shouldBe` Right (Just (list [sym "+", i 3]))
    it "handles empty input" $ do
      malRead "" `shouldBe` Right Nothing
    it "detects mismatched parens" $ do
      malRead "(+ 1 2" `shouldSatisfy` isErrorMatching "end of input"

  describe "standard step1 tests" $ do
    it "Testing read of numbers" $ do
      test "1" $ i 1
      test "   7   " $ i 7
      test "-123" $ i (-123)
    it "Testing read of symbols" $ do
      test "+" $ sym "+"
      test "abc" $ sym "abc"
      test "   def  " $ sym "def"
      test "abc5" $ sym "abc5"
      test "abc-def" $ sym "abc-def"
    it "Testing non-numbers starting with a dash." $ do
      test "-" $ sym "-"
      test "-abc" $ sym "-abc"
      test "->>" $ sym "->>"
    it "Testing read of lists" $ do
      test "(+ 1 2)" $ list [sym "+",i 1, i 2]
      test "()" $ list []
      test "( )" $ list []
      test "(nil)" $ list [ASTSpecialLit MalNil]
      test "((3 4))" $
        list [list [i 3, i 4]]
      test "(+ 1 (+ 2 3))" $
        list [sym "+", i 1 , list [sym "+", i 2, i 3]]
      test "  ( +   1   (+   2 3   )   )  " $
        list [sym "+", i 1 , list [sym "+", i 2, i 3]]
      test "(* 1 2)" $
        list [sym "*", i 1, i 2]
      test "(** 1 2)" $
        list [sym "**", i 1, i 2]
      test "(* -3 6)" $
        list [sym "*", i (-3), i 6]
      test "(()())" $
        list [list[], list[]]
    it "Test commas as whitespace" $ do
      test "(1 2, 3,,,,),," $
        list [i 1, i 2, i 3]
    it "Testing read of nil/true/false" $ do
      test "nil" $ ASTSpecialLit MalNil
      test "true" $ ASTSpecialLit MalTrue
      test "false" $ ASTSpecialLit MalFalse
    it "Testing read of comments" $ do
       testNothing "  ;; whole line comment (not an exception)"
       test " 1 ; comment after expression" $ i 1
       test "1; comment after expression" $ i 1
    it "Testing read of quoting" $ do
      test "'1" $ list [sym "quote", i 1]
      test "'(1 2 3)" $ list [sym "quote", list [i 1, i 2, i 3]]
      test "`1" $ list [sym "quasiquote", i 1]
      test "`(1 2 3)" $ list [sym "quasiquote", list [i 1, i 2, i 3]]
      test "~1" $ list [sym "unquote", i 1]
      test "~(1 2 3)" $ list [sym "unquote", list [i 1, i 2, i 3]]
      test "~@(1 2 3)" $ list [sym "splice-unquote", list [i 1, i 2, i 3]]
    it "Testing keywords" $ do
      test ":kw" $ keyword "kw"
      test "(:kw1 :kw2 :kw3)" $
        list [keyword "kw1", keyword "kw2", keyword "kw3"]
    it "Testing read of vectors" $ do
      test "[+ 1 2]" $ ASTVector [sym "+", i 1, i 2]
      test "[]" $ ASTVector []
      test "[ ]" $ ASTVector []
      test "[[3 4]]" $ ASTVector [ASTVector [i 3, i 4]]
      test "[+ 1 [+ 2 3]]" $
        ASTVector [sym "+", i 1, ASTVector [sym "+", i 2, i 3]]
      test "  [ +   1   [+   2 3   ]   ]" $
        ASTVector [sym "+", i 1, ASTVector [sym "+", i 2, i 3]]
      test "([])" $ list [ASTVector []]
    it  "Testing read of hash maps" $ do
      test "{}" $ ASTMap []
      test "{ }" $ ASTMap []
      test "{\"abc\" 1}" $ ASTMap [s "abc", i 1]
      test "{\"a\" {\"b\" 2}}" $ ASTMap [s "a", ASTMap [s "b", i 2]]
      test "{\"a\" {\"b\" {\"c\" 3}}}" $
        ASTMap [s "a", ASTMap [s "b", ASTMap [s "c", i 3]]]
      test "      {  \"a\"  {\"b\"   {  \"cde\"     3   }  }}" $
        ASTMap [s "a", ASTMap [s "b", ASTMap [s "cde", i 3]]]
      test "{\"a1\" 1 \"a2\" 2 \"a3\" 3}" $ -- This may go out of order
        ASTMap [s "a1", i 1, s "a2", i 2, s "a3", i 3]
      test "{  :a  {:b   {  :cde     3   }  }}" $
        ASTMap [keyword "a", ASTMap [keyword "b", ASTMap [keyword "cde", i 3]]]
      test "{\"1\" 1}" $ ASTMap [s "1", i 1]
      test "({})" $ list [ASTMap []]
    it "Testing read of ^/metadata" $ do
      test "^{\"a\" 1} [2 3]" $
        list [sym "with-meta",
          ASTVector [i 2, i 3],
          ASTMap [s "a", i 1]]
    it "Testing read of strings" $ do
      test "\"abc\"" $ s "abc"
      test "    \"abc\"" $ s "abc"
      test "\"abc (with parens)\"" $ s "abc (with parens)"
      test "\"abc\\\"def\"" $ s "abc\"def"
      test "\"\"" $ s ""
      test "\"\\\\\"" $ s "\\"
      test "\"\\\\\\\\\\\\\"" $ s "\\\\\\"
      test "\"&\"" $ s "&"
      test "\"'\"" $ s "'"
      test "\"(\"" $ s "("
      test "\")\"" $ s ")"
      test "\"*\"" $ s "*"
      test "\"+\"" $ s "+"
      test "\",\"" $ s ","
      test "\"-\"" $ s "-"
      test "\"/\"" $ s "/"
      test "\":\"" $ s ":"
      test "\";\"" $ s ";"
      test "\"<\"" $ s "<"
      test "\">\"" $ s ">"
      test "\"?\"" $ s "?"
      test "\"@\"" $ s "@"
      test "\"[\"" $ s "["
      test "\"]\"" $ s "]"
      test "\"^\"" $ s "^"
      test "\"_\"" $ s "_"
      test "\"`\"" $ s "`"
      test "\"{\"" $ s "{"
      test "\"}\"" $ s "}"
      test "\"~\"" $ s "~"
    it "Testing reader errors" $ do
      malRead "(1 2" `shouldSatisfy` isErrorMatching "end of input"
      malRead "[1 2" `shouldSatisfy` isErrorMatching "end of input"
      malRead "\"ab" `shouldSatisfy` isErrorMatching "end of input"
      malRead "\"" `shouldSatisfy` isErrorMatching "end of input"
      malRead "\"\\" `shouldSatisfy` isErrorMatching "unexpected"
      malRead "\"\\\\\\\\\\\"" `shouldSatisfy` isErrorMatching "end of input"
      malRead "(1 \"abc" `shouldSatisfy` isErrorMatching "end of input"
      malRead "(1 \"abc\"" `shouldSatisfy` isErrorMatching "end of input"
    it "Testing read of @/deref" $ do
      test "@a" $ list [sym "deref", sym "a"]
    it "Non alphanumerice characters in strings" $ do
      test "\"\\n\"" $ s "\n"
      test "\"#\"" $ s "#"
      test "\"$\"" $ s "$"
      test "\"%\"" $ s "%"
      test "\".\"" $ s "."
      test "\"\\\\\"" $ s "\\"
      test "\"|\"" $ s "|"
    it "Non alphanumeric characters in comments" $ do
      test "1;!" $ i 1
      test "1;\"" $ i 1
      test "1;#" $ i 1
      test "1;$" $ i 1
      test "1;%" $ i 1
      test "1;'" $ i 1
      test "1;\\" $ i 1
      test "1;\\\\" $ i 1
      test "1;`" $ i 1
      test "2; &()*+,-./:;<=>?@[]^_{|}~" $ i 2
      test "\"!\"" $ s "!"
