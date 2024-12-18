I implemented this in Haskell; I haven't actually used Haskell in a few months, so I've prioritised keeping code clear rather than very clever.


This requires Cabal and GHC version > 9. (I'm using GHC-9.4.8)

From this directory, you should build and run from the project directly using `cabal`:
```sh
cabal build
cabal run
```

You may also be able to run straight from the executable:
```
./LoanInterest
```

## Check list

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
    - [x] By entering a single index ([0 .. 5]) corresponding to a loan field.

### What i would further do to make sure its production ready.

General design. For example:
- Better orthonogalise pure code from side-effectful code.
      - I'm not happy that IO is scattered in too many places right now.
      - Error propagation can be better done.
            For example, currently, the app responds to invalid user inputs by returning `Nothing` and printing an error message. We could use `Either e a` and implement a custom error ADT for `e`.
- ADT for the set of valid currencies.
- I'm sure there's a lot of elegant functional/monadic abstractions for reducing boilerplate.

User experience. For example:
- Ability to update multiple indexes/fields in one go.
- Additional feature to view a condensed representation of the historical data.

Performance. For example:
-   I might use criterion and observe whether the time to generate the historical data scales linearly.
-   I might test at what point the list of historical data becomes problematic for in-memory/stack space.
        A solution could be to either:
            1. Iteratively compute a temporary batch of historical data, print it to console, and compute the next temporary batch of data
            2. Write data to a local file or upload to a data base.
-   I suspect this will become IO bound for logging large amounts of historical data to the console.
        A solution could be to both:
            1. print a summary of the data to the console, allowing the user to immediately be prompted for a new interaction.
            2. asynchronously write the complete historical data to a local file.

Safety. For example:
If I had more time, I would implement some:
- Unit tests, e.g., ruling out invalid loan information.
- Property tests.
    - I've implemented a property test to show you how I might have done this, but