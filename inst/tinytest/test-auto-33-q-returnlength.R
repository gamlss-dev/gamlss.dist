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
        dinfo <- sprintf("p = %s", deparse(p))

        # Testing ...
        expect_silent(tmp <- qfun(p = p),
            info = sprintf("Expected 'q%s(%s)' to run silent.", family, dinfo))
        expect_true(is.double(tmp),
            info = sprintf("Expected 'q%s(%s)' to return double.", family, dinfo))
        expect_identical(length(tmp), length(p),
            info = sprintf("Return length of 'q%s(%s)' not identical to %d.", family, dinfo, n))
    }

    # Multiple 'parameter's
    for (p in conf$params) {
        for (n in c(3L, 7L)) {
            # Getting n 'valid' values for the current parameter
            vals  <- rep(conf[[c(p, "inside")]], length.out = n)
            dinfo <- sprintf("%s = %s", p, deparse(vals))

            # Testing ...
            expect_silent(tmp <- do.call(qfun, setNames(list(0.42, vals), c("p", p))),
                info = sprintf("Expected 'q%s(p = 0.42, %s)' to run silent.", family, dinfo))
            expect_true(is.double(tmp),
                info = sprintf("Expected 'q%s(p = 0.42, %s)' to return double.", family, dinfo))
            expect_identical(length(tmp), n,
                info = sprintf("Return length of 'q%s(p = 0.42, %s)' identical to %d.", family, dinfo, n))
        }
    }

}
