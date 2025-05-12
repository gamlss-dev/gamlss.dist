# -------------------------------------------------------------------
# Testing distribution functions in the valid range (w/ valid parameters
# within support).
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

    # Getting p<FAM> function (cdf)
    cdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(cdf)
    expect_identical(as.list(f), as.list(conf$arguments$p),
        info = sprintf("Testing that default arguments of 'p%s()' have not changed!", family))

    # ---------------------------------------------------------------
    # Get grid of valid parameter values
    # ---------------------------------------------------------------
    grd_valid <- get_testgrid_valid(conf, TRUE, "q")

    # ---------------------------------------------------------------
    # Testing all valid combinations; expecting silent execution and
    # a valid (non-NA) numeric return.
    # ---------------------------------------------------------------
    for (i in seq_len(nrow(grd_valid))) {
        formals(cdf)[names(grd_valid)] <- grd_valid[i, ]
        pinfo <- sprintf("p%s%s", family, gsub("^pairlist", "", deparse(formals(cdf))))

        expect_silent(tmp <- cdf(),     info = sprintf("'%s' expected to run silent.", pinfo))
        expect_inherits(tmp, "numeric", info = sprintf("Return of '%s' should be numeric.", pinfo))
        expect_inherits(tmp, "numeric", info = sprintf("Length of return of '%s' should be 1L.", pinfo))
    }

}
