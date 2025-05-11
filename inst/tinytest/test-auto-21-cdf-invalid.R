# -------------------------------------------------------------------
# Testing distribution function with invalid parameters.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("config/get_testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL, verbose = FALSE)

# If 'outside' is  a numeric vector, return as is. If 'outside' is invalid, we
# take the 50% quantile (no interpolation; closest value defined) of the
# 'inside' vector to have a valid element for testing.
get_vals <- function(inside, outside)
    if (is.null(outside)) unname(quantile(inside, p = 0.5, type = 1)) else outside

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting p<FAM> function (cdf)
    cdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Getting invalid value set (outside defined range for dpqr)
    # ---------------------------------------------------------------
    invalid <- list(q = with(conf$y, get_vals(inside, outside)))
    for (p in conf$params)
        invalid[[p]] <- with(conf[[c(p, "dpqr")]], get_vals(inside, outside))

    # ---------------------------------------------------------------
    # Testing all invalid combinations; expecting warnings and
    # a numeric missing value as return (NA_real_).
    # ---------------------------------------------------------------
    grd_invalid <- expand.grid(invalid)

    for (i in seq_len(nrow(grd_invalid))) {
        formals(cdf)[names(grd_invalid)] <- grd_invalid[i, ]
        expect_error(tmp <- cdf(),
            info = sprintf("'d%s%s' did not throw an error.", family, gsub("^pairlist", "", deparse(formals(cdf)))))
    }
    rm(cdf)

}
