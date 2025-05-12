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

    # Getting p<FAM> function (qfun)
    qfun <- get(sprintf("q%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing multi-return
    # ---------------------------------------------------------------

    # Multiple probabilities
    for (n in c(3L, 7L)) {
        p     <- seq(0, 1, length.out = n)
        qinfo <- sprintf("q%s(p = %s)", family, deparse(p))

        # Testing ...
        expect_silent(tmp <- qfun(p = p),        info = sprintf("'%s' expected to run silent.", qinfo))
        expect_true(is.double(tmp),              info = sprintf("'%s' should return object of type double.", qinfo))
        expect_identical(length(tmp), length(p), info = sprintf("Length of return of '%s' should be identical to %d.", qinfo, n))
    }

    # Multiple 'parameter's
    for (p in conf$params) {
        for (n in c(3L, 7L)) {
            # Getting n 'valid' values for the current parameter
            vals  <- rep(conf[[c(p, "inside")]], length.out = n)
            qinfo <- sprintf("q%s(p = 0.42, %s)", family, sprintf("%s = %s", p, deparse(vals)))

            # Testing ...
            expect_silent(tmp <- do.call(qfun, setNames(list(0.42, vals), c("p", p))),
                        info = sprintf("Expected '%s' to run silent.", qinfo))
            expect_true(is.double(tmp),
                        info = sprintf("Expected '%s' to return object of type double.", qinfo))
            expect_identical(length(tmp), n,
                        info = sprintf("Return length of '%s' expected to be identical to %d.", qinfo, n))
        }
    }

}
