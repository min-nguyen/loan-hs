{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           LoanInterest

import           Data.Time

import           Debug.Trace     (trace)
import           LoanInterest
import           Test.QuickCheck

-- Arbitrary instance for Day
instance Arbitrary Day where
  arbitrary = do
      year <- choose (0, 2100)
      month <- choose (1, 12)
      day <- choose (1, 28)
      return $ fromGregorian year month day

genPosFloat :: Gen Double
genPosFloat = arbitrary `suchThat` (> 0)

genValidLoan :: Gen Loan
genValidLoan = do
  start_date <- arbitrary
  end_date   <- arbitrary `suchThat` (\end_date -> end_date >= start_date)
  Loan start_date end_date <$> genPosFloat <*> arbitrary <*> genPosFloat  <*> genPosFloat

prop_computeDailyInterest_Length :: Property
prop_computeDailyInterest_Length =
    forAll genValidLoan $ \loan ->
        let Loan{start, end} = loan
            days = fromIntegral $ diffDays end start
        in  length (computeDailyInterest loan) == days + 1

prop_computeDailyInterest_DailyInterest :: Property
prop_computeDailyInterest_DailyInterest =
    forAll genValidLoan $ \loan ->
        let Loan{amount, end, base_interest, margin_interest} = loan
            Daily {daily_base_interest, daily_total_interest, total_interest, days_elapsed} = head $ computeDailyInterest loan
        in     daily_base_interest          == (amount * base_interest) / 365.0
            && daily_total_interest         == (amount * (base_interest + margin_interest)) / 365.0
            && roundToTwoDP total_interest  == roundToTwoDP (daily_total_interest * fromIntegral days_elapsed)

main :: IO ()
main = do
    quickCheck prop_computeDailyInterest_Length
    quickCheck prop_computeDailyInterest_DailyInterest

