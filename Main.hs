module Main where

import           LoanInterest

main :: IO ()
main = do
    putStrLn $ "Enter the index of a field name to update:\n\t" ++  show indexedLoanFields
    loan <- initLoan
    loopUpdate loan
