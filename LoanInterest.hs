{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.List
import Data.Time
import Text.Read hiding (step)

data Loan = Loan {
    start :: Day,
    end   :: Day,
    amount   :: Float,
    currency  :: String,
    base_interest :: Float,
    margin      :: Float
} deriving Show

loanFields :: [String]
loanFields = ["start", "end", "amount", "currency", "base_interest", "margin"]

defaultLoan :: Loan
defaultLoan = Loan {
    start = fromGregorian 2000 1 1,
    end   = fromGregorian 2000 12 31,
    amount   = 0.0,
    currency  = "USD",
    base_interest = 0.0,
    margin = 0.0
}

data DailyInterest = Daily {
    daily_base_interest :: Float,
    daily_total_interest :: Float,
    date_accrued       :: Day,
    days_elapsed       :: Int,
    total_interest     :: Float
} deriving Show

parseDate :: String -> Maybe Day -> IO Day
parseDate prompt lower_bound = do
    putStrLn prompt
    day_str :: Maybe Day <- getLine >>= pure . parseTimeM True defaultTimeLocale "%-d-%-m-%0Y"
    case (day_str, lower_bound) of
        (Just date, Nothing)
            -> do   pure date
        (Just date, Just start_date)
           | date >= start_date
            ->  do  pure date
           | otherwise
            ->  do  putStrLn ("End date must follow the start date: " ++ show start_date)
                    parseDate prompt lower_bound
        (Nothing, _)
            -> putStrLn "Invalid date format" >> parseDate prompt lower_bound

parseDec :: String -> IO Float
parseDec prompt = do
    putStrLn prompt
    dec_str :: Maybe Float <- getLine >>= (pure . readMaybe)
    case dec_str of
        Just dec | dec >= 0 -> pure (roundTo2Decs dec)
        Just _              -> putStrLn "Number must be non-negative" >> parseDec prompt
        Nothing             -> putStrLn "Invalid number" >> parseDec prompt

roundTo2Decs :: Float -> Float
roundTo2Decs x = (fromIntegral . round) (x * 100) / 100

updateLoan :: Loan -> String -> IO Loan
updateLoan loan field  = case field of
    "start" -> do
        date <- parseDate "Enter start date in \"DD-MM-YYYY\" format (e.g., 07-10-1995)" Nothing
        pure loan { start = date }
    "end" -> do
        date <- parseDate "Enter end date in \"DD-MM-YYYY\" format (e.g., 20-10-2020)" (Just (start loan))
        pure loan { end = date }
    "amount" -> do
        amt <- parseDec "Enter loan amount in whole or decimal format (e.g., 15000, 70.00)" >>= pure . roundTo2Decs
        pure loan { amount = amt}
    "currency" -> do
        curr <- putStrLn "Enter currency (e.g., USD, GBP, EUR)" >> getLine
        pure loan { currency = curr }
    "base_interest" -> do
        base_intr_rate <- parseDec "Enter base interest rate (%) in whole or decimal format (e.g., 5, 2.5)"
        pure loan { base_interest = base_intr_rate}
    "margin" -> do
        mrg <- parseDec "Enter margin interest rate (%) in whole or decimal format (e.g., 5, 2.5)"
        pure loan { margin = mrg}
    _ -> do
        putStrLn $ "Invalid field name: " ++ field ++ ". Enter a field to update: " ++ unwords (intersperse "," loanFields)
        getLine >>= updateLoan loan

initLoan :: IO Loan
initLoan = foldl (\mloan field -> mloan >>= flip updateLoan field) (pure defaultLoan) loanFields

computeDailyInterest :: Loan -> [DailyInterest]
computeDailyInterest (Loan {start, end, amount, base_interest, margin}) =
    step [Daily base_daily total_daily start 0 0]
    where
        base_daily  =  (amount * base_interest) / 365.0
        total_daily =  (amount * (base_interest + margin)) / 365.0

        step :: [DailyInterest] -> [DailyInterest]
        step (day : days)
            | date_accrued day < end =
                step  (day { date_accrued = 1 `addDays` date_accrued day
                           , days_elapsed = 1 + days_elapsed day
                           , total_interest = roundTo2Decs $ total_daily + total_interest day} : day : days)
            | otherwise =
                day : days

updateLoop :: Loan -> IO b
updateLoop loan = do
    mapM_ print (reverse $ computeDailyInterest loan)
    putStrLn $ "Current Loan Information:\n " ++ show loan
    putStrLn $ "Write a field name to update:\n " ++ unwords (intersperse "," loanFields)
    getLine >>= updateLoan loan >>= updateLoop

main :: IO ()
main = do
    loan <- initLoan
    updateLoop loan