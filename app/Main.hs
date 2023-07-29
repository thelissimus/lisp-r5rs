module Main where

import Lib

main :: IO ()
main = getLine >>= print . eval . readExpr
