-- {-# OPTIONS_GHC -F -pgmF doctest-discover #-}

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src"]
