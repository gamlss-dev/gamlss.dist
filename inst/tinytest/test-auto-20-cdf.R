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
        info = sprintf("Names of arguments or order of arguments changed in 'p%s()'!", family))

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
        expect_silent(tmp <- cdf(),
            info = sprintf("'p%s%s' did not run silent.", family, gsub("^pairlist", "", deparse(formals(cdf)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'p%s%s' did not return numeric.", family, gsub("^pairlist", "", deparse(formals(cdf)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'p%s%s' did not return result of length 1.", family, gsub("^pairlist", "", deparse(formals(cdf)))))
    }

}
