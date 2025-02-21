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
    my_expect_silent(f, fam  <- get(f)())
    dfun <- get(paste0("d", f))
    qfun <- get(paste0("q", f))
    pfun <- get(paste0("p", f))

    # Checking family object properties (few of them)
    my_expect_inherits(f, fam, "gamlss.family")
    my_expect_inherits(f, fam, "family")
    my_expect_identical(f, fam$type, "Continuous")

    my_expect_inherits(f, fam$parameters, "list")
    my_expect_inherits(f, names(fam$parameters), "character")
    my_expect_true(f, length(fam$parameters) > 0)
    my_expect_inherits(f, fam$nopar, "numeric")
    my_expect_identical(f, length(fam$parameters), as.integer(fam$nopar))

    # Testing distribution function
    my_expect_silent(f, pfun(limits$y.range[1]))
    my_expect_silent(f, pfun(limits$y.range[2]))
    my_expect_silent(f, pfun(limits$y.range[1] - eps))
    my_expect_silent(f, pfun(limits$y.range[2] + eps))

    # Testing density function
    my_expect_silent(f, dfun(limits$y.range[1]))
    my_expect_silent(f, dfun(limits$y.range[2]))
    ##my_expect_silent(f, dfun(limits$y.range[1] - eps))
    my_expect_silent(f, dfun(limits$y.range[2] + eps))

    # Testing quantile function
    my_expect_silent(f, qfun(limits$p.range[1]))
    my_expect_silent(f, qfun(limits$p.range[2]))
    my_expect_identical(f, qfun(limits$p.range[1] - 1), NaN)
    my_expect_identical(f, qfun(limits$p.range[2] + 1), NaN)
    my_expect_identical(f, qfun(limits$p.range[1] - eps), NaN)
    my_expect_identical(f, qfun(limits$p.range[2] + eps), NaN)

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
        my_expect_equal(f, pfun(limits$y.range[1]), 0)
        my_expect_equal(f, pfun(limits$y.range[2]), 1)
    }

}

