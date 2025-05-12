# -------------------------------------------------------------------
# Testing distribution functions; check behavior when the density is
# calculated outside the support of the distribution.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("helperfunctions.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL, verbose = FALSE)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting p<FAM> function (distribution)
    cdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing multi-return
    # ---------------------------------------------------------------

    # Multiple x's
    for (n in c(3L, 7L)) {
        # Getting n 'valid' values for the current parameter
        vals  <- rep(conf$y$inside, length.out = n)
        pinfo <- sprintf("p%s(%s)", family, sprintf("q = %s", deparse(vals)))

        # Testing ...
        expect_silent(tmp <- cdf(q = vals), info = sprintf("'%s' expected to run silent.", pinfo))
        expect_true(is.double(tmp),         info = sprintf("'%s' should return object of type double.", pinfo))
        expect_identical(length(tmp), n,    info = sprintf("Length of return of '%s' should be identical to %d.", pinfo, n))
    }

    # Multiple 'parameter's
    qval <- unname(quantile(conf$y$inside, p = 0.5, type = 2))
    for (p in conf$params) {
        for (n in c(3L, 7L)) {
            # Getting n 'valid' values for the current parameter
            vals  <- rep(conf[[c(p, "inside")]], length.out = n)
            pinfo <- sprintf("%s = %s", p, deparse(vals))

            # Testing ...
            expect_silent(tmp <- do.call(cdf, setNames(list(qval, vals), c("q", p))),
                    info = sprintf("Expected '%s' to run silent.", pinfo))
            expect_true(is.double(tmp),
                    info = sprintf("Expected '%s' to return double.", pinfo))
            expect_identical(length(tmp), n,
                    info = sprintf("Return length of '%s' not identical to %dL.", pinfo, n))
        }
    }

}
