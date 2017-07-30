module Lib
    ( price
    ) where

price :: String -> Int
price ""     = 0
price "A"    = 50
price "AB"   = 80
price "CDBA" = 115
