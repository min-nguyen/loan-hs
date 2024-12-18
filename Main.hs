module Main where

import           LoanInterest

main :: IO ()
main = initLoan >>= loopUpdate
