# -------------------------------------------------------------------
# Getting random values with invalid parameters.
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

    # Getting r<FAM> function (random number generator)
    random <- get(sprintf("r%s", family), envir = getNamespace("gamlss.dist"))

    # Missing argument 'n' should throw an error
    expect_error(random(), info = sprintf("Calling 'r%s()' without argument 'n' should throw an error.", family))

    # ---------------------------------------------------------------
    # Testing all invalid combinations; expecting warnings and
    # a numeric missing value as return (NA_real_).
    # ---------------------------------------------------------------
    grd_invalid <- get_testgrid_invalid(conf, TRUE, "n", main_values = c(1L, 7L))
    for (i in seq_len(nrow(grd_invalid))) {
        formals(random)[names(grd_invalid)] <- grd_invalid[i, ]
        rinfo <- paste0("r", family, gsub("^pairlist", "", deparse(formals(random))))

        expect_error(tmp <- random(), info = sprintf("'%s' expected to throw an error.", rinfo))
    }
    rm(random)

}
