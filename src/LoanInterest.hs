{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LoanInterest where

import           Data.Time
import           Text.Read hiding (step)

data Loan = Loan {
    start           :: Day,
    end             :: Day,
    amount          :: Float,
    currency        :: String,
    base_interest   :: Float,
    margin_interest :: Float
}

defaultLoan :: Loan
defaultLoan = Loan {
    start = fromGregorian 2000 1 1,
    end   = fromGregorian 2000 12 31,
    amount   = 0.0,
    currency  = "USD",
    base_interest = 0.0,
    margin_interest = 0.0
}

data LoanField = StartDate | EndDate | LoanAmount | Currency | Base | Margin deriving (Enum, Show)

indexedLoanFields = zip [0..] (enumFrom StartDate)

instance Show Loan where
    show (Loan {start, end, amount, currency, base_interest, margin_interest}) =
        unlines
            [ "Loan:"
            , "\tStart Date:           " ++ show start
            , "\tEnd Date:             " ++ show end
            , "\tLoan Amount:          " ++ show amount ++ " " ++ currency
            , "\tBase Interest Rate:   " ++ show base_interest ++ "%"
            , "\tMargin Interest Rate: " ++ show margin_interest ++ "%"
            ]

data DailyInterest = Daily {
    daily_base_interest  :: Float,
    daily_total_interest :: Float,
    date_accrued         :: Day,
    days_elapsed         :: Int,
    total_interest       :: Float
}

instance Show DailyInterest where
    show (Daily {daily_base_interest, daily_total_interest, date_accrued, days_elapsed, total_interest}) =
        unlines
            [ "Daily Interest:"
            , "\tDate Accrued:         " ++ show date_accrued
            , "\tDays Elapsed:         " ++ show days_elapsed
            , "\tDaily Base Interest:  " ++ show daily_base_interest ++ " units"
            , "\tDaily Total Interest: " ++ show daily_total_interest ++ " units"
            , "\tTotal Interest:       " ++ show (roundToTwoDP total_interest) ++ " units (to 2 dec places)"
            ]

-- Prompt the user for a date in the format YYYY-MM-DD with an optional lower-bound
promptDate :: String -> Maybe Day -> IO Day
promptDate prompt lower_bound = do
    putStrLn prompt
    day_str :: Maybe Day <- getLine >>= pure . parseTimeM True defaultTimeLocale "%0Y-%-m-%-d"
    case (day_str, lower_bound) of
        (Just date, Nothing)
            -> pure date
        (Just date, Just start_date)
           | date >= start_date
            -> pure date
           | otherwise
            ->  do  putStrLn ("End date must follow the start date: " ++ show start_date)
                    promptDate prompt lower_bound
        (Nothing, _)
            -> putStrLn "Invalid date format" >> promptDate prompt lower_bound

-- Prompt the user for a non-neg decimal (e.g. percentage or loan amount)
promptPosFloat :: String -> IO Float
promptPosFloat prompt = do
    putStrLn prompt
    dec_str :: Maybe Float <- getLine >>= (pure . readMaybe)
    case dec_str of
        Just dec | dec >= 0 -> pure dec
        Just _              -> putStrLn "Number must be non-negative" >> promptPosFloat prompt
        Nothing             -> putStrLn "Invalid number" >> promptPosFloat prompt

promptLoanFieldIndex :: IO LoanField
promptLoanFieldIndex
 = do
    putStrLn $ "Enter the index of a field name to update:\n\t" ++  show indexedLoanFields
    n <- getLine >>= pure . readMaybe
    case n of
        Just idx
            -> if idx >= 0 && idx < length indexedLoanFields then pure (toEnum idx)
               else putStrLn ("Index out of range: " ++ show idx) >> promptLoanFieldIndex

        Nothing
            -> putStrLn "Couldn't parse index. " >> promptLoanFieldIndex


roundToTwoDP :: Float -> Float
roundToTwoDP x = (fromIntegral . round) (x * 100) / 100

-- Update a single field in an existing Loan
updateLoan :: Loan -> LoanField -> IO Loan
updateLoan loan field  = case field of
    StartDate -> do
        date <- promptDate "Enter start date in \"YYYY-MM-DD\" format (e.g., 1995-12-28)" Nothing
        pure loan { start = date }
    EndDate -> do
        date <- promptDate "Enter end date in \"YYYY-MM-DD\" format (e.g., 2020-01-28)" (Just (start loan))
        pure loan { end = date }
    LoanAmount -> do
        amt <- promptPosFloat "Enter loan amount in whole or decimal format (e.g., 15000, 70.00)" >>= pure . roundToTwoDP
        pure loan { amount = amt}
    Currency -> do
        curr <- putStrLn "Enter currency (e.g., USD, GBP, EUR)" >> getLine
        pure loan { currency = curr }
    Base -> do
        base_intr_rate <- promptPosFloat "Enter base interest rate (%) in whole or decimal format (e.g., 5, 2.5)"
        pure loan { base_interest = base_intr_rate}
    Margin -> do
        mrg <- promptPosFloat "Enter margin interest rate (%) in whole or decimal format (e.g., 5, 2.5)"
        pure loan { margin_interest = mrg}

-- Prompt the user to enter each of the fields in a Loan
initLoan :: IO Loan
initLoan = foldl (\mloan field -> mloan >>= flip updateLoan field) (pure defaultLoan) (enumFrom StartDate)

-- From an initial loan, accumulate the daily accrued interest between a start and end date
computeDailyInterest :: Loan -> [DailyInterest]
computeDailyInterest (Loan {start, end, amount, base_interest, margin_interest}) =
    step [Daily base_daily total_daily start 0 0]
    where
    base_daily  =  (amount * base_interest) / 365.0
    total_daily =  (amount * (base_interest + margin_interest)) / 365.0

    -- Prepend the interest of the next day
    step :: [DailyInterest] -> [DailyInterest]
    step (day : days)
        | date_accrued day < end =
            step  (day { date_accrued = 1 `addDays` date_accrued day
                        , days_elapsed = 1 + days_elapsed day
                        , total_interest = total_daily + total_interest day} : day : days)
        | otherwise =
            day : days

-- Print historical daily interest for a given loan, and prompt the user to update a loan field.
loopUpdate :: Loan -> IO b
loopUpdate loan = do
    mapM_ print (reverse $ computeDailyInterest loan)
    putStrLn   "-------------------------------------------------"
    putStrLn $ "\nCurrent loan information used:\n " ++ show loan
    promptLoanFieldIndex
        >>= updateLoan loan
        >>= loopUpdate
