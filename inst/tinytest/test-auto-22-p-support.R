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

# Epsilon for testing
eps <- sqrt(.Machine$double.eps)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting p<FAM> function (cdf)
    cdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Get grid of valid parameter combinations, but setting the
    # main parameter 'q' below the lower end of the support.
    # ---------------------------------------------------------------
    dead_end <- match.arg(conf$type, c("Continuous", "Discrete")) # Ensure we have this captured
    main_val <- if (conf$type == "Continuous") conf$support[1L] - eps else conf$support - 1
    grd_valid <- get_testgrid_valid(conf, TRUE, "q", main_values = main_val)

    for (i in seq_len(nrow(grd_valid))) {
        tmpfun <- cdf
        formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
        expect_equal(tmpfun(), 0,
            info = sprintf("Expected 'p%s(%s)' (outside lower support) to return 0.", family,
                           paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))
    }

    # ---------------------------------------------------------------
    # Doing the very same, but setting '1' above upper support.
    # ---------------------------------------------------------------
    main_val <- if (conf$type == "Continuous") conf$support[2L] + eps else conf$support + 1
    grd_valid <- get_testgrid_valid(conf, TRUE, "q", main_values = main_val)

    for (i in seq_len(nrow(grd_valid))) {
        tmpfun <- cdf
        formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
        expect_equal(tmpfun(), 1,
            info = sprintf("Expected 'p%s(%s)' (outside lower support) to return 1.", family,
                           paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))
    }

    # ---------------------------------------------------------------
    # When using in valid range, the CDF must return a value in [0, 1]
    # ---------------------------------------------------------------
    # This 'grid' excludes boundaries in the parameters
    tmp <- list(q = conf$y$inside)
    for (p in conf$params) tmp[[p]] <- conf[[c(p, "inside")]]
    grd <- expand.grid(tmp)
    for (i in seq_len(nrow(grd))) {
        tmpfun <- cdf
        formals(tmpfun)[names(grd)] <- grd[i, ]
        tmp <- tmpfun()
        expect_true(length(tmp) == 1L && tmp >= 0 && tmp <= 1,
            info = sprintf("Expected 'p%s(%s)' to return single numeric in [0.0, 1.0].", family,
                           paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))

    }

}
