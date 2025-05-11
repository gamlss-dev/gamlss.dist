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

    # Getting p<FAM> function (cdf)
    cdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Setting up combinations of valid parameters; q is always below the lower
    # end of the support to check if the CDF returns 0.
    # ---------------------------------------------------------------
    dead_end <- match.arg(conf$type, c("Continuous", "Discrete")) # Ensure we have this captured
    valid <- list(q = conf$support[1L] - if (conf$type == "Continuous") 1 else eps)
    for (p in conf$params) valid[[p]] <- conf[[c(p, "inside")]]

    # Make grid of unique combinations
    grd <- expand.grid(valid)
    for (i in seq_len(nrow(grd))) {
        formals(cdf)[names(grd)] <- grd[i, ]
        expect_equal(cdf(), 0,
            info = sprintf("Expected 'p%s(%s)' (outside lower support) to return 0.", family,
                           paste(sprintf("%s = %s", names(grd), fmt(grd[i, ])), collapse = ", ")))
    }
    rm(valid, grd)

    # ---------------------------------------------------------------
    # Doing the very same, but setting q above upper support.
    # ---------------------------------------------------------------
    dead_end <- match.arg(conf$type, c("Continuous", "Discrete")) # Ensure we have this captured
    valid <- list(q = conf$support[2L] + if (conf$type == "Continuous") 1 else eps)
    for (p in conf$params) valid[[p]] <- conf[[c(p, "inside")]]

    # Make grid of unique combinations
    grd <- expand.grid(valid)
    for (i in seq_len(nrow(grd))) {
        formals(cdf)[names(grd)] <- grd[i, ]
        expect_equal(cdf(), 1,
            info = sprintf("Expected 'p%s(%s)' (outside lower support) to return 1.", family,
                           paste(sprintf("%s = %s", names(grd), fmt(grd[i, ])), collapse = ", ")))
    }
    rm(valid, grd)

}
