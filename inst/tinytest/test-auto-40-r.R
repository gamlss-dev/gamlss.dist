# -------------------------------------------------------------------
# Testing functions to draw random samples.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("helperfunctions.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting q<FAM> function (quantile function)
    random <- get(sprintf("r%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(random)
    expect_identical(as.list(f), as.list(conf$arguments$r),
        info = sprintf("Testing that default arguments of 'r%s()' have not changed!", family))

    # ---------------------------------------------------------------
    # Get grid of valid parameter values
    # ---------------------------------------------------------------
    args <- as.data.frame(setNames(lapply(conf$params, function(x) unname(quantile(conf[[c(x, "inside")]], 0.5, type = 2))), conf$params))

    # ---------------------------------------------------------------
    # Testing all valid combinations; expecting silent execution and
    # a valid (non-NA) numeric return.
    # ---------------------------------------------------------------
    for (n in c(1L, 2L, 7L)) {
        formals(random)[names(args)] <- args

        rinfo <- sprintf("r%s(n = %d, %s)", family, n, paste(sprintf("%s = %s", names(args), fmt(args)), collapse = ", "))

        expect_silent(tmp <- random(n = n), info = sprintf("'%s' expected to run silent.", rinfo))
        expect_inherits(tmp, "numeric",     info = sprintf("Return of '%s' should be numeric.", rinfo))
        expect_identical(length(tmp), n,    info = sprintf("Length of return of '%s' expected to be %d.", rinfo, n))
        expect_true(all(!is.na(tmp)),       info = sprintf("Expected return of '%s' to only contain non-missing (NA) values.", rinfo))
    }
    rm(random)

}
