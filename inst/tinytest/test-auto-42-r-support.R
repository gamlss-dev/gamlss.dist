# -------------------------------------------------------------------
# Drawing random numbers, checking support. Random numbers drawn
# with valid parameters should always return numbers inside the
# support of the distribution. Although these tests are - by definition -
# random, we are still testing the returns.
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

    # Getting r<FAM> function (random number generator)
    random <- get(sprintf("r%s", family), envir = getNamespace("gamlss.dist"))

    # Get grid of unique combinations
    grd_valid <- get_testgrid_valid(conf, TRUE, "n", main_values = 20L)
    for (i in seq_len(nrow(grd_valid))) {
        formals(random)[names(grd_valid)] <- grd_valid[i, ]
        rinfo <- sprintf("r%s%s", family, gsub("^pairlist", "", deparse(formals(random))))

        expect_silent(tmp <- random(), info = sprintf("Expected '%s' to run silent.", rinfo))
        expect_true(all(tmp >= conf$support[1L]),
                info = sprintf("Expected all elemts of the return of '%s' to be >= %s", rinfo, fmt(conf$support[1L])))
        expect_true(all(tmp <= conf$support[2L]),
                info = sprintf("Expected all elemts of the return of '%s' to be <= %s", rinfo, fmt(conf$support[2L])))
    }

}
