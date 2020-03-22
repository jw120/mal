#lang brag

; Parser for the mal language

mal-expr:  INTEGER | STRING |
mal-special1 | mal-sym | mal-keyword |
mal-def! |
mal-list | mal-vec | mal-map |
mal-error-eof

mal-def!: /OPEN-PAREN DEF! mal-expr* /CLOSE-PAREN

mal-list: /OPEN-PAREN mal-expr* /CLOSE-PAREN

mal-vec: /OPEN-SQUARE mal-expr* /CLOSE-SQUARE

mal-map: /OPEN-CURLY mal-expr* /CLOSE-CURLY

mal-keyword: KEYWORD

mal-special1: SPECIAL mal-expr

mal-sym: SYMBOL

mal-error-eof: UNTERMINATED-STRING | OPEN-PAREN mal-expr* | OPEN-SQUARE mal-expr* | OPEN-CURLY mal-expr*
