{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module ReaderSpec (spec) where

import Data.Text (Text)

import Test.Hspec

import Reader

-- Helper function to shorten tests
test :: Text -> AST -> Expectation
test t u = malRead t `shouldBe` Right u

spec :: Spec
spec = do
  describe "Reader function" $ do
    it "parses an integer" $ do
      malRead "123" `shouldBe` Right (ASTInt 123)
    it "parses a signed integer" $ do
      malRead "-1234" `shouldBe` Right (ASTInt (-1234))
    it "parses an integer with leading space" $ do
      malRead "   123" `shouldBe` Right (ASTInt 123)
    it "parses an integer with trailing space" $ do
      malRead "123 " `shouldBe` Right (ASTInt 123)
    it "parses a string literal" $ do
      malRead "\"xyz\"" `shouldBe` Right (ASTString "xyz")
    it "parses a string literal with an escaped quote" $ do
      malRead "\"x\\\"yz\"" `shouldBe` Right (ASTString "x\"yz")
    it "parses a normal alphabetic atom" $ do
      malRead "abc" `shouldBe` Right (ASTSymbol "abc")
  describe "step1 tests" $ do
    it "Testing read of numbers" $ do
      test "1" $ ASTInt 1
      test "   7   " $ ASTInt 7
      test "-123" $ ASTInt (-123)
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
{-
    ;; Testing read of lists
    (+ 1 2)
    ;=>(+ 1 2)
    ()
    ;=>()
    ( )
    ;=>()
    (nil)
    ;=>(nil)
    ((3 4))
    ;=>((3 4))
    (+ 1 (+ 2 3))
    ;=>(+ 1 (+ 2 3))
      ( +   1   (+   2 3   )   )
    ;=>(+ 1 (+ 2 3))
    (* 1 2)
    ;=>(* 1 2)
    (** 1 2)
    ;=>(** 1 2)
    (* -3 6)
    ;=>(* -3 6)
    (()())
    ;=>(() ())

    ;; Test commas as whitespace
    (1 2, 3,,,,),,
    ;=>(1 2 3)


    ;>>> deferrable=True

    ;;
    ;; -------- Deferrable Functionality --------

    ;; Testing read of nil/true/false
    nil
    ;=>nil
    true
    ;=>true
    false
    ;=>false

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

    ;; Testing read of quoting
    '1
    ;=>(quote 1)
    '(1 2 3)
    ;=>(quote (1 2 3))
    `1
    ;=>(quasiquote 1)
    `(1 2 3)
    ;=>(quasiquote (1 2 3))
    ~1
    ;=>(unquote 1)
    ~(1 2 3)
    ;=>(unquote (1 2 3))
    `(1 ~a 3)
    ;=>(quasiquote (1 (unquote a) 3))
    ~@(1 2 3)
    ;=>(splice-unquote (1 2 3))


    ;; Testing keywords
    :kw
    ;=>:kw
    (:kw1 :kw2 :kw3)
    ;=>(:kw1 :kw2 :kw3)

    ;; Testing read of vectors
    [+ 1 2]
    ;=>[+ 1 2]
    []
    ;=>[]
    [ ]
    ;=>[]
    [[3 4]]
    ;=>[[3 4]]
    [+ 1 [+ 2 3]]
    ;=>[+ 1 [+ 2 3]]
      [ +   1   [+   2 3   ]   ]
    ;=>[+ 1 [+ 2 3]]
    ([])
    ;=>([])

    ;; Testing read of hash maps
    {}
    ;=>{}
    { }
    ;=>{}
    {"abc" 1}
    ;=>{"abc" 1}
    {"a" {"b" 2}}
    ;=>{"a" {"b" 2}}
    {"a" {"b" {"c" 3}}}
    ;=>{"a" {"b" {"c" 3}}}
    {  "a"  {"b"   {  "cde"     3   }  }}
    ;=>{"a" {"b" {"cde" 3}}}
    ;;; The regexp sorcery here ensures that each key goes with the correct
    ;;; value and that each key appears only once.
    {"a1" 1 "a2" 2 "a3" 3}
    ;/{"a([1-3])" \1 "a(?!\1)([1-3])" \2 "a(?!\1)(?!\2)([1-3])" \3}
    {  :a  {:b   {  :cde     3   }  }}
    ;=>{:a {:b {:cde 3}}}
    {"1" 1}
    ;=>{"1" 1}
    ({})
    ;=>({})

    ;; Testing read of comments
     ;; whole line comment (not an exception)
    1 ; comment after expression
    ;=>1
    1; comment after expression
    ;=>1

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
