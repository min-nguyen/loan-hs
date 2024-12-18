In Haskell.

Requires Cabal and GHC version > 9. (I'm using GHC-9.4.8)

From this directory, you should build and run from the project directly using `cabal`:
```sh
cabal build
cabal run
```

You may also be able to run straight from the executable:
```
./LoanInterest
```

## check list

### User Journey
- [x] A user can provide input parameters to calculate a loan.
    - The input for a loan should contain:
        1. Start Date (date)
        2. End Date (date)
        3. Loan Amount (amount field)
        4. Loan Currency (currency)
        5. Base Interest Rate (percentage)
        6. Margin (percentage), where Total Interest Rate = Base Interest Rate + Margin
    - Constraints implemented:
        - [x] Date follows format "YYYY-MM-DD"
        - [x] Start date preceeds end date
        - [x] Loan amount, base interest, and margin interest are non-negative floats
        - [x] Loan amount is rounded to 2 dp
        - [x] Currency can be arbitrary
- [x] The system should generate an output containing the loan calculation results.
    - The output data structure should include the daily accrued interest for each day between the start and end date of the loan:
        1. Daily Interest Amount without margin
        2. Daily Interest Amount Accrued
        3. Accrual Date
        4. Number of Days elapsed since the Start Date of the loan
        5. Total Interest - calculated over the given period
- [x] A user can access historic calculations that they can update with new input parameters.
    - [ ] By entering an index ([0 .. 5]) corresponding to a loan field.

### User Journey
// what i would do to make sure its production ready