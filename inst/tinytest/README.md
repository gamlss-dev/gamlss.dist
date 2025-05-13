

# `gamlss.dist` tests

This folder contains a series of files using `tinytest` for automated testing
of all families. There are four main sets of files, each serving a dedicated
purpose.

| Description            | File naming convention               |
|:-----------------------|:-------------------------------------|
| Auxilary functions     | `helperfunctions.R`                  |
| Configurations         | `config/config_<familyname>.R`       |
| Automated tests        | `test-auto-<ID>-<name>.R`            |
| Family-specific tests  | `test-dist-<familyname>.R`           |


## Auxilary functions

The R script `helperfunctions.R` contains a series of custom functions
used to run the tests. These include, for example:

- `get_testconfig()`: Loads one or more configuration files
  from the `config/*` folder and checks whether they are set up properly.
- `get_testgrid_*()`: Returns a `data.frame` with a series of unique
  combinations of parameters for testing the `dpqr` functions.


## Configurations

Each family to be tested via the automated tests must have a configuration
file (`config/config_<familyname>.R`). This file contains a list with,
for example, default function calls, the definition of the family's support,
its parameters, as well as valid ("inside") and invalid ("outside") values
used for testing.

The config file can contain a list entry `disabled`. If set `TRUE`, the file
will be ignored by the automated tests (for development purposes).


## Automated tests

When the tests are run, all `test-*` files are sorted lexicographically and
executed sequentially. As a result, all automated tests (`test-auto-*`) are
run before the family-specific tests (see below). The `<ID>` in the name of
each `test-auto-*` file ensures that the tests are executed in a specific order.

The following list provides an overview of these automated tests, their
file-naming convention, and the purpose of each test file.


**Testing Family Constructor**

- `test-auto-01-availability.R`: For each family to be tested (see
  *Configurations*), this script checks whether all required functions are
  available--specifically the family constructor function (e.g., `NO()`) and the
  corresponding `dpqr` functions (`dNO()`, `pNO()`, `qNO()`, `rNO()`).

- `test-auto-02-objects.R`: Creates a "default family object" using default
  arguments (i.e., `obj <- NO()`) and verifies that the object is correctly set
  up and contains all required elements.

- `test-auto-03-links.R`: Tests whether the family constructor function
  supports all (combinations of) links specified in the configuration file. It
  also checks that the link and inverse link functions are correctly defined in
  the family object and that `linkfun(linkinv(x)) == x` holds.

- `test-auto-0[4-9]*`: Reserved for additional automated tests if needed.


**Testing `dpqr` Functions**

The files `test-auto-[1-4]*.R` are organized into four blocks:

- **10–19**: Tests for the density function (`d`)
- **20–29**: Tests for the distribution function (`p`)
- **30–39**: Tests for the quantile function (`q`)
- **40–49**: Tests for the random number generator (`r`)

Each block follows a consistent structure:

- `test-auto-*0-<p>.R`: Tests that:
  - The default values of the function have not changed.
  - The function runs silently and returns numeric values when called
    with all combinations of valid input values.

- `test-auto-*1-<p>-invalid.R`: Tests that:
  - The function throws an error if called without any arguments.
  - The function issues warnings and returns `NA_real_` when called with
    invalid parameter specifications.

- `test-auto-*2-<p>-support.R`: Tests that:
  - The function behaves correctly when called with arguments outside the support:
    - For the density function (`d`): returns a missing value and triggers a warning.
    - For the distribution function (`p`): returns `0` if `q` is below the lower support,
      and `1` if `q` is above the upper support.
    - For the quantile function (`q`): throws an error if `p < 0` or `p > 1`.
  - The quantile function (`q`) returns a value within the defined support when 
    called with `p >= 0 & p <= 1`.

- `test-auto-*3-<p>-returnlength.R`: Tests that:
  - The function returns (i) numeric results of (ii) the correct length
    depending on the input arguments.
  - **TODO(R):** Currently, only the main argument (e.g., `x =`, `p =`, `q =`) is varied.
    We previously agreed that:
    - Either all arguments must be of length 1,
    - Or all must be of the same length `N`,
    - Or all but one must be of length 1, in which case the shorter arguments are  
      recycled via `rep()` to match the length.
    - Otherwise, the function should throw an error.
    This logic is not yet implemented.

- **TODO(R):** `test-auto-4*` currently draws random numbers without setting a
  seed. Consider using a custom environment or explicitly setting/resetting the
  random number generator to make these tests deterministic.


## Family-Specific Tests

After all `test-auto-*` files have been executed, the `test-dist-*.R` files are
run (in lexicographic order). These files contain family-specific tests that
are not covered by the automated tests.

**TODO(R):** Add concrete examples. These scripts must include all tests
necessary to verify that the functions return the *correct* results. Note that
the automated tests primarily check for functionality and structure, but not
for mathematical correctness or precision.


