# -------------------------------------------------------------------
# Testing distribution functions in the valid range (w/ valid parameters
# within support).
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("config/get_testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL)

# If 'outside' is  a numeric vector, return as is. If 'outside' is invalid, we
# take the 50% quantile (no interpolation; closest value defined) of the
# 'inside' vector to have a valid element for testing.
get_vals <- function(inside, outside)
    if (is.null(outside)) unname(quantile(inside, p = 0.5, type = 1)) else outside

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting p<FAM> function (cdf)
    cdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # Missing argument 'x' should throw an error
    expect_error(cdf(),
        info = sprintf("'p%s()' without argument 'x' did not throw an error.", family))

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(cdf)
    expect_identical(as.list(f), as.list(conf$arguments$p),
        info = sprintf("Names of arguments or order of arguments changed in 'p%s()'!", family))
    # Expecting log = FALSE per default
    expect_false(f$log, info = "'p%s()': Expected default argument 'log = FALSE'.")

    # ---------------------------------------------------------------
    # Getting valid value set (inside defined range for dpqr)
    # ---------------------------------------------------------------
    valid <- list(q = conf$y$inside)
    for (p in conf$params) valid[[p]] <- conf[[c(p, "dpqr", "inside")]]

    # ---------------------------------------------------------------
    # Testing all valid combinations; expecting silent execution and
    # a valid (non-NA) numeric return.
    # ---------------------------------------------------------------
    grd_valid <- expand.grid(valid)

    # ---------------------------------------------------------------
    # Testing all valid combinations; expecting silent execution and
    # a valid (non-NA) numeric return.
    # ---------------------------------------------------------------
    tmpfun <- cdf
    for (i in seq_len(nrow(grd_valid))) {
        formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
        expect_silent(tmp <- tmpfun(),
            info = sprintf("'p%s%s' did not run silent.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'p%s%s' did not return numeric.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'p%s%s' did not return result of length 1.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
    }
    rm(tmpfun)

}
