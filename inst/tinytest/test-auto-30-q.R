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

    # Getting q<FAM> function (quantile function)
    qfun <- get(sprintf("q%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(qfun)
    expect_identical(as.list(f), as.list(conf$arguments$q),
        info = sprintf("Testing that default arguments of 'q%s()' have not changed!", family))

    # ---------------------------------------------------------------
    # Get grid of valid parameter values
    # ---------------------------------------------------------------
    p <- seq(0, 1, by = 0.25) # 25% interval
    grd_valid <- get_testgrid_valid(conf, TRUE, "p", main_values = p)

    # ---------------------------------------------------------------
    # Testing all valid combinations; expecting silent execution and
    # a valid (non-NA) numeric return.
    # ---------------------------------------------------------------
    for (i in seq_len(nrow(grd_valid))) {
        formals(qfun)[names(grd_valid)] <- grd_valid[i, ]
        qinfo <- sprintf("q%s%s", family, gsub("^pairlist", "", deparse(formals(qfun))))

        expect_silent(tmp <- qfun(),      info = sprintf("'%s' expected to run silent.", qinfo))
        expect_inherits(tmp, "numeric",   info = sprintf("Return of '%s' should be numeric.", qinfo))
        expect_identical(length(tmp), 1L, info = sprintf("Length of return of '%s' should be 1L.", qinfo))
    }
    rm(qfun)

}
