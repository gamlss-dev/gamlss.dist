# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) {
    library("tinytest")
    library("gamlss.dist")
}

# Helper functions
source("functions.R")

# Limits to be tested
limits <- get_limits("continuous")
eps <- sqrt(.Machine$double.eps)

# TODO(R): A few hand selected distributions which do not fail these basic tests.
families <- c(
    "NO", "LO", "LOGNO"
)

# Getting required functions
for (f in families) {
    #x <- readline(paste("Starting with ", f, "?  [enter] "))
    my_expect_silent(fam  <- get(f)(), family = f)
    dfun <- get(paste0("d", f))
    qfun <- get(paste0("q", f))
    pfun <- get(paste0("p", f))

    # Checking family object properties (few of them)
    my_expect_inherits(fam, "gamlss.family", family = f)
    my_expect_inherits(fam, "family", family = f)
    my_expect_identical(fam$type, "Continuous", family = f)

    my_expect_inherits(fam$parameters, "list", family = f)
    my_expect_inherits(names(fam$parameters), "character", family = f)
    my_expect_true(length(fam$parameters) > 0, family = f)
    my_expect_inherits(fam$nopar, "numeric", family = f)
    my_expect_identical(length(fam$parameters), as.integer(fam$nopar), family = f)

    # Testing distribution function
    my_expect_silent(pfun(limits$y.range[1]), family = f)
    my_expect_silent(pfun(limits$y.range[2]), family = f)
    my_expect_silent(pfun(limits$y.range[1] - eps), family = f)
    my_expect_silent(pfun(limits$y.range[2] + eps), family = f)

    # Testing density function
    my_expect_silent(dfun(limits$y.range[1]), family = f)
    my_expect_silent(dfun(limits$y.range[2]), family = f)
    ##my_expect_silent(f, dfun(limits$y.range[1] - eps), family = f)
    my_expect_silent(dfun(limits$y.range[2] + eps), family = f)

    # Testing quantile function
    my_expect_silent(qfun(limits$p.range[1]), family = f)
    my_expect_silent(qfun(limits$p.range[2]), family = f)
    my_expect_identical(qfun(limits$p.range[1] - 1), NaN, family = f)
    my_expect_identical(qfun(limits$p.range[2] + 1), NaN, family = f)
    my_expect_identical(qfun(limits$p.range[1] - eps), NaN, family = f)
    my_expect_identical(qfun(limits$p.range[2] + eps), NaN, family = f)

    # Testing whether the sum over the density sums up to 1.
    # Families with one parameter
    if (fam$nopar == 1L) {
        for (mu in limits$mu.val) {
            tmp <- try(integrate(function(x) dfun(x = x, mu = mu),
                                 lower = limits$y.range[1],
                                 upper = limits$y.range[2],
                                 subdivisions = 1000L))
            my_expect_equal(f, tmp, 1)
        }
        my_expect_equal(pfun(limits$y.range[1]), 0, family = f)
        my_expect_equal(pfun(limits$y.range[2]), 1, family = f)
    }

}

