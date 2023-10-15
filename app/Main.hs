module Main (main) where

import Data.Either (fromRight)
import Data.Text.IO qualified as TIO

import Lib (eval, readExpr, trapError)

main :: IO ()
main = do
  ln <- TIO.getLine
  putStrLn . fromRight "" . trapError . fmap show $ (eval =<< readExpr ln)
