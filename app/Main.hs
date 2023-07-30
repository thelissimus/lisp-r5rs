module Main where

import Lib

main :: IO ()
main = do
  ln <- getLine
  let res =
        extractValue
          . trapError
          . fmap show
          $ (eval =<< readExpr ln)
  putStrLn res
