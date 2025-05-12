# -------------------------------------------------------------------
# Testing density functions; check behavior when the density is
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
    main_val <- if (conf$type == "Continuous") conf$support + c(-eps, eps) else conf$support + c(-1, 1)

    # Get grid of unique combinations
    grd_valid <- get_testgrid_valid(conf, TRUE, "x", main_values = main_val)
    for (i in seq_len(nrow(grd_valid))) {
        formals(cdf)[names(grd_valid)] <- grd_valid[i, ]
        dinfo <- sprintf("d%s(%s)", family, gsub("^pairlist", "", deparse(formals(cdf))))

        expect_equal(cdf(), 0, info = sprintf("'%s' (outside support) should return a density of 0.", dinfo))
    }

}
