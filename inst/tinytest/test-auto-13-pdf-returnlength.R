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

    # Getting d<FAM> function (density)
    pdf <- get(sprintf("d%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing multi-return
    # ---------------------------------------------------------------

    # Multiple x's
    for (n in c(3L, 7L)) {
        # Getting n 'valid' values for the current parameter
        vals  <- sample(conf$y$inside, n, replace = TRUE)
        dinfo <- sprintf("x = %s", deparse(vals))

        # Testing ...
        expect_silent(tmp <- pdf(x = vals),
            info = sprintf("Expected 'd%s(%s)' to run silent.", family, dinfo))
        expect_true(is.double(tmp),
            info = sprintf("Expected 'd%s(%s)' to return double.", family, dinfo))
        expect_identical(length(tmp), n,
            info = sprintf("Return length of 'd%s(%s)' identical to %d.", family, dinfo, n))
    }

    # Multiple 'parameter's
    xval <- unname(quantile(conf$y$inside, p = 0.5, type = 2))
    for (p in conf$params) {
        for (n in c(3L, 7L)) {
            # Getting n 'valid' values for the current parameter
            vals  <- rep(conf[[c(p, "inside")]], length.out = n)
            dinfo <- sprintf("%s = %s", p, deparse(vals))

            # Testing ...
            expect_silent(tmp <- do.call(pdf, setNames(list(xval, vals), c("x", p))),
                info = sprintf("Expected 'p%s(x = %s, %s)' to run silent.", family, fmt(xval), dinfo))
            expect_true(is.double(tmp),
                info = sprintf("Expected 'p%s(x = %s, %s)' to return double.", family, fmt(xval), dinfo))
            expect_identical(length(tmp), n,
                info = sprintf("Return length of 'p%s(x = %s, %s)' not identical to %d.", family, fmt(xval), dinfo, n))
        }
    }

}
