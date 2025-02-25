# -------------------------------------------------------------------
# Testing distribution functions; check behavior when the density is
# calculated outside the support of the distribution.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("config/get_testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL, verbose = FALSE)

# Epsilon for testing
eps <- sqrt(.Machine$double.eps)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting d<FAM> function (pdf)
    pdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Setting up combinations of valid where 'x' is always outside support.
    # ---------------------------------------------------------------
    dead_end <- match.arg(conf$type, c("Continuous", "Discrete")) # Ensure we have this captured
    if (conf$type == "Continuous") {
        valid <- list(q = conf$support + c(-eps, eps))
    } else {
        valid <- list(q = conf$support + c(-1, 1))
    }
    for (p in conf$params) valid[[p]] <- conf$dpqr[[p]]$valid

    # Make grid of unique combinations
    grd <- expand.grid(valid)

    for (i in seq_len(nrow(grd))) {
        tmpfun <- pdf
        formals(tmpfun)[names(grd)] <- grd[i, ]
        expect_equal(tmpfun(), 0,
            info = sprintf("Expected 'p%s(%s)' (outside support) to return 0.", family,
                           paste(sprintf("%s = %s", names(grd), fmt(grd[i, ])))))
    }

}
