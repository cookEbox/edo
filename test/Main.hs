module Main (main) where
import Test.DocTest (doctest)
import System.Environment (getArgs)

main :: IO ()
main = do 
  cli <- getArgs
  doctest $
    [ "-XQualifiedDo"
    , "src"
    ] ++ cli

