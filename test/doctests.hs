module Main (main) where

import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "indigo" =<< getArgs
