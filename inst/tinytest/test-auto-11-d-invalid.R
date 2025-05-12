# -------------------------------------------------------------------
# Testing density function with invalid parameters.
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

    # Getting d<FAM> function (pdf)
    pdf <- get(sprintf("d%s", family), envir = getNamespace("gamlss.dist"))

    # Missing argument 'x' should throw an error
    expect_error(pdf(), info = sprintf("Calling 'd%s()' without argument 'x' should throw an error.", family))

    # ---------------------------------------------------------------
    # Testing all invalid combinations; expecting warnings and
    # a numeric missing value as return (NA_real_).
    # ---------------------------------------------------------------
    grd_invalid <- get_testgrid_invalid(conf, TRUE, "x")
    for (i in seq_len(nrow(grd_invalid))) {
        formals(pdf)[names(grd_invalid)] <- grd_invalid[i, ]
        dinfo <- paste0("d", family, gsub("^pairlist", "", deparse(formals(pdf))))

        expect_warning(tmp <- pdf(), info = sprintf("'%s' should throw a warning.", dinfo))
        expect_equal(tmp, NA_real_,  info = sprintf("'%s' should return a numeric missing value (NA_real_).", dinfo))
    }
    rm(pdf)

}
