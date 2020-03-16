#lang brag

; Parser for the mal language

mal-expr:  INTEGER | STRING |
           mal-special | mal-sym | mal-keyword | mal-list | mal-vec | mal-map | mal-error

mal-list: /OPEN-PAREN mal-expr* /CLOSE-PAREN

mal-vec: /OPEN-SQUARE mal-expr* /CLOSE-SQUARE

mal-map: /OPEN-CURLY mal-expr* /CLOSE-CURLY

mal-keyword: KEYWORD

mal-special: SPECIAL

mal-sym: SYMBOL

mal-error: UNTERMINATED-STRING
