# -------------------------------------------------------------------
# Testing limits of discrete distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) {
    library("tinytest")
    library("gamlss.dist")
}

# Helper functions
source("functions.R")

# Limits to be tested
limits <- get_limits("discrete")
eps <- sqrt(.Machine$double.eps)

# TODO(R): A few handselected families which do not crash my simple tests
families <- c("BB", "BI", "NBI", "NBII", "PO")

# Getting required functions
for (f in families) {
    fam  <- get(f)()
    dfun <- get(paste0("d", f))
    qfun <- get(paste0("q", f))
    pfun <- get(paste0("p", f))

    # Checking family object properties (few of them)
    my_expect_inherits(f, fam, "gamlss.family")
    my_expect_inherits(f, fam, "family")
    my_expect_identical(f, fam$type, "Discrete")

    my_expect_inherits(f, fam$parameters, "list")
    my_expect_inherits(f, names(fam$parameters), "character")
    my_expect_true(f, length(fam$parameters) > 0)

    # Testing distribution function
    my_expect_silent(f, pfun(limits$y.range[1]))
    my_expect_silent(f, pfun(limits$y.range[2]))
    my_expect_silent(f, pfun(limits$y.range[1] - 1))
    my_expect_silent(f, pfun(limits$y.range[2] + 1))
    my_expect_silent(f, pfun(limits$y.range[1] - eps))
    my_expect_silent(f, pfun(limits$y.range[2] + eps))

    # Testing density function
    my_expect_silent(f, dfun(limits$y.range[1]))
    my_expect_silent(f, dfun(limits$y.range[2]))
    my_expect_silent(f, dfun(limits$y.range[1] - 1))
    my_expect_silent(f, dfun(limits$y.range[2] + 1))
    my_expect_warning(f, dfun(-0.5)) # non-integer warning
    my_expect_warning(f, dfun(0.5))  # non-integer warning

    # Testing quantile function
    my_expect_silent(f, qfun(limits$p.range[1]))
    my_expect_silent(f, qfun(limits$p.range[2]))
    my_expect_identical(f, qfun(limits$p.range[1] - 1), NaN)
    my_expect_identical(f, qfun(limits$p.range[2] + 1), NaN)
    my_expect_identical(f, qfun(limits$p.range[1] - eps), NaN)
    my_expect_identical(f, qfun(limits$p.range[2] + eps), NaN)

    # Testing whether the sum over the density sums up to 1.
    my_expect_equal(f, sum(dfun(0:1000)), 1)
    my_expect_equal(f, sum(pfun(1000)), 1)
}

