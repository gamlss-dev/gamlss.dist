# -------------------------------------------------------------------
# Testing density functions; check behavior when the density is
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
    cdf <- get(sprintf("d%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Setting up combinations of valid where 'x' is always outside support,
    # such that we always expect '0' as result (no density).
    # ---------------------------------------------------------------
    dead_end <- match.arg(conf$type, c("Continuous", "Discrete")) # Ensure we have this captured
    if (conf$type == "Continuous") {
        valid <- list(x = conf$support + c(-eps, eps))
    } else {
        valid <- list(x = conf$support + c(-1, 1))
    }
    for (p in conf$params) valid[[p]] <- conf[[c(p, "inside")]]

    # Make grid of unique combinations
    grd <- expand.grid(valid)

    for (i in seq_len(nrow(grd))) {
        tmpfun <- cdf
        formals(tmpfun)[names(grd)] <- grd[i, ]
        expect_equal(tmpfun(), 0,
            info = sprintf("Expected 'd%s(%s)' (outside support) to return 0.", family,
                           paste(sprintf("%s = %s", names(grd), fmt(grd[i, ])), collapse = ", ")))
    }

}
