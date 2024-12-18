I implemented this in Haskell. I haven't actually used Haskell in about a year, so I've prioritised keeping code clear rather than very clever.


This requires Cabal and GHC version > 9. (I'm using GHC-9.4.8)
#### Executable
From this directory, you should build and run from the project directly using `cabal`:
```sh
cabal build
cabal run
```
You may also be able to run straight from the executable:
```sh
./LoanInterest
```

#### Tests
This took me past the rough 2-3 hour time limit, but just to show how I might've continued, I also implemented a couple of basic property-based tests.
```sh
cabal run test:tests
```
This was a bit useful: I had to switch from  `Float` to `Double` to maintain a precise total interest accrued over a very large period of time (e.g. that $TotalInterest = DailyTotalInterest * Days$).

### User Journey
- [x] A user can provide input parameters to calculate a loan.
    - Constraints:
        - [x] Date follows format "YYYY-MM-DD"
        - [x] Start date preceeds end date
        - [x] Loan amount, base interest, and margin interest are non-negative decimals
        - [x] Loan amount is (implicitly) rounded to 2 dp
        - [x] Currency can be arbitrary
- [x] The system should generate an output containing the loan calculation results.
- [x] A user can access historic calculations that they can update with new input parameters.
    - [x] By entering a single index ([0 .. 5]) corresponding to a loan field.

### What i might further do to make sure its production ready

__General design__. For example:
- Better orthonogalise pure code from side-effectful code.
    - I'm not happy that `IO` is scattered in too many places right now.
    - Error propagation can be better done.
        For example, currently, the app responds to invalid user inputs by returning `Nothing` and printing an error message. We could use `Either e a` and implement a custom error ADT for `e`.
- ADT for the set of valid currencies.
- I'm sure there's a lot of elegant functional/monadic abstractions for reducing boilerplate.

__User experience__. For example:
- Ability to update multiple indexes/fields in one go.
- Additional feature to view a condensed representation of the historical data.

__Performance__. For example:
-   I might use criterion to observe whether the time to generate the historical data scales linearly.
-   I might test at what point the list of historical data becomes problematic for in-memory/stack space.

    A solution could be to:
    - Iteratively compute a temporary batch of historical data, print it to console, and compute the next temporary batch of data

-  I'm sure there's some parallelism that can be performed too:
    - We could divide the loan information into $n$  batches of start_dates and end_dates, and build each fragment of historical data beginning from different values for `days_elapsed` and `total_interest`.

-   I suspect this will become `IO` bound for logging large amounts of historical data to the console.

    A solution could be to both:
    1. print a condensed summary of the data to the console, allowing the user to immediately be prompted for a new interaction.
    2. asynchronously write the full historical data to a local file.


__Safety__. For example:
If I had more time, I would implement some:
- Unit tests.
    - For ruling out invalid loan information, and for simple pure functions like `roundTo2DP`.
- Property tests.
    - Other general properties.