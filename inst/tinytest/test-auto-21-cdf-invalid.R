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

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting d<FAM> function (pdf)
    pdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Setting up combinations of valid values
    # ---------------------------------------------------------------
    valid <- list(x = conf$y$valid)
    for (p in conf$params) valid[[p]] <- conf[[c("dpqr", p, "valid")]]
    invalid <- setNames(lapply(conf$params, function(p) conf$dpqr[[p]]$invalid), conf$params)

    # Testing invalid parameters (should aus an error)
    for (p in names(invalid)) {
        tmpfun <- pdf
        for (v in invalid[[p]]) {
            formals(tmpfun)[c("x", p)] <- c(valid$x[1], v)
            # TODO(R): Here deparse(formals(tmpfun)) works different than in test-auto-10-pdf.R;
            #          Try to streamline it (maybe via a function sourced above) to provide
            #          easy-to-read infos.
            expect_error(tmpfun(),
                info = sprintf("'p%s(x = %s, %s = %s)' (invalid %s) did not throw a warning.",
                               family, fmt(valid$x[1]), p, fmt(v), p))
        }
    }

}
