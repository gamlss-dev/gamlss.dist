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
    my_expect_inherits(fam, "gamlss.family", family = f)
    my_expect_inherits(fam, "family", family = f)
    my_expect_identical(fam$type, "Discrete", family = f)

    my_expect_inherits(fam$parameters, "list", family = f)
    my_expect_inherits(names(fam$parameters), "character", family = f)
    my_expect_true(length(fam$parameters) > 0, family = f)

    # Testing distribution function
    my_expect_silent(pfun(limits$y.range[1]), family = f)
    my_expect_silent(pfun(limits$y.range[2]), family = f)
    my_expect_silent(pfun(limits$y.range[1] - 1), family = f)
    my_expect_silent(pfun(limits$y.range[2] + 1), family = f)
    my_expect_silent(pfun(limits$y.range[1] - eps), family = f)
    my_expect_silent(pfun(limits$y.range[2] + eps), family = f)

    # Testing density function
    my_expect_silent(dfun(limits$y.range[1]), family = f)
    my_expect_silent(dfun(limits$y.range[2]), family = f)
    my_expect_silent(dfun(limits$y.range[1] - 1), family = f)
    my_expect_silent(dfun(limits$y.range[2] + 1), family = f)
    my_expect_warning(dfun(-0.5), family = f) # non-integer warning
    my_expect_warning(dfun(0.5), family = f)  # non-integer warning

    # Testing quantile function
    my_expect_silent(qfun(limits$p.range[1]), family = f)
    my_expect_silent(qfun(limits$p.range[2]), family = f)
    my_expect_identical(qfun(limits$p.range[1] - 1), NaN, family = f)
    my_expect_identical(qfun(limits$p.range[2] + 1), NaN, family = f)
    my_expect_identical(qfun(limits$p.range[1] - eps), NaN, family = f)
    my_expect_identical(qfun(limits$p.range[2] + eps), NaN, family = f)

    # Testing whether the sum over the density sums up to 1.
    my_expect_equal(sum(dfun(0:1000)), 1, family = f)
    my_expect_equal(sum(pfun(1000)), 1, family = f)
}

