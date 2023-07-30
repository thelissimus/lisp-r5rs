{-# LANGUAGE Safe #-}

module Main (main) where

import Data.Either (fromRight)
import Lib (eval, readExpr, trapError)

main :: IO ()
main = do
  ln <- getLine
  putStrLn . fromRight "" . trapError . fmap show $ (eval =<< readExpr ln)
