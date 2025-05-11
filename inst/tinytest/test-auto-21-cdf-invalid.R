# -------------------------------------------------------------------
# Testing distribution function with invalid parameters.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("helperfunctions.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL, verbose = FALSE)

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
    # Get grid of invalid combination of parameters.
    # ---------------------------------------------------------------
    grd_invalid <- get_testgrid_invalid(conf, TRUE, "q")

    # Testing CDF call
    for (i in seq_len(nrow(grd_invalid))) {
        formals(cdf)[names(grd_invalid)] <- grd_invalid[i, ]
        expect_error(tmp <- cdf(),
            info = sprintf("'d%s%s' did not throw an error.", family, gsub("^pairlist", "", deparse(formals(cdf)))))
    }

}
