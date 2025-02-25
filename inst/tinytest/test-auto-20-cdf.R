# -------------------------------------------------------------------
# Testing distribution functions in the valid range (w/ valid parameters
# within support).
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("config/get_testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting d<FAM> function (pdf)
    pdf <- get(sprintf("p%s", family), envir = getNamespace("gamlss.dist"))

    # Missing argument 'x' should throw an error
    expect_error(pdf(),
        info = sprintf("'p%s()' without argument 'x' did not throw an error.", family))

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(pdf)
    expect_identical(as.list(f), as.list(conf$arguments$p),
        info = sprintf("Names of arguments or order of arguments changed in 'p%s()'!", family))
    # Expecting log = FALSE per default
    expect_false(f$log, info = "'p%s()': Expected default argument 'log = FALSE'.")

    # ---------------------------------------------------------------
    # Getting valid parameter set
    # ---------------------------------------------------------------
    valid <- list(x = conf$y$valid)
    for (p in conf$params) valid[[p]] <- conf[[c("dpqr", p, "valid")]]

    # ---------------------------------------------------------------
    # Testing all valid combinations; expecting silent execution and
    # a valid (non-NA) numeric return.
    # ---------------------------------------------------------------
    grd <- expand.grid(valid[!names(valid) == "x"])

    tmpfun <- pdf
    formals(tmpfun)["q"] <- valid$x[[1]]
    for (i in seq_len(nrow(grd))) {
        formals(tmpfun)[names(grd)] <- grd[i, ]
        expect_silent(tmp <- tmpfun(),
            info = sprintf("'p%s%s' did not run silent.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'p%s%s' did not return numeric.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'p%s%s' did not return result of length 1.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
    }
    rm(tmpfun)

}
