# -------------------------------------------------------------------
# Testing density functions in the valid range (w/ valid parameters
# within support).
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

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(pdf)
    expect_identical(as.list(f), as.list(conf$arguments$d),
        info = sprintf("Testing that default arguments of 'd%s()' have not changed!", family))

    # ---------------------------------------------------------------
    # Getting valid value set (inside defined range for dpqr)
    # ---------------------------------------------------------------
    grd_valid <- get_testgrid_valid(conf, FALSE, main = "x")

    tmpfun <- pdf
    for (i in seq_len(nrow(grd_valid))) {
        formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
        dinfo <- sprintf("d%s%s", family, gsub("^pairlist", "", deparse(formals(tmpfun))))

        expect_silent(tmp <- tmpfun(),    info = sprintf("'%s' expected to run silent.", dinfo))
        expect_inherits(tmp, "numeric",   info = sprintf("Return of '%s' should be numeric.", dinfo))
        expect_identical(length(tmp), 1L, info = sprintf("Length of return of '%s' should be 1L.", dinfo))
    }
    rm(tmpfun)

    # ---------------------------------------------------------------
    # Testing integral, should sum up to 1
    # ---------------------------------------------------------------
    # If continuous: Try numeric integration for all combinations
    if (conf$type == "Continuous") {
        for (i in seq_len(nrow(grd_valid))) {
            # Dynamically create pdf function with different default arguments
            tmpfun <- pdf
            formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
            dinfo <- sprintf("d%s%s", family, gsub("^pairlist", "", deparse(formals(tmpfun))))

            # Integrate ...
            integral <- integrate(pdf, lower = conf$support[1], upper = conf$support[2], subdivisions = 1000L)
            expect_equal(integral$value, 1, info = sprintf("Integral of (continuous) '%s' should evaluate to 1.0", dinfo))
        }
    # Else build sum
    } else {
        # TODO(R): Using 0:5000 here not conf$support as e.g., the Poisson
        #          distribution supports -Inf,Inf.
        for (i in seq_len(nrow(grd_valid))) {
            # Dynamically create pdf function with different default arguments
            args   <- grd_valid[i, , drop = FALSE]
            tmpfun <- pdf
            formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
            dinfo <- sprintf("d%s%s", family, gsub("^pairlist", "", deparse(formals(tmpfun))))

            # Integrate
            integral <- sum(pdf(seq.int(0, 5000)))
            expect_equal(integral, 1, info = sprintf("Integral of (discrete) '%s' should evaluate to 1.0", dinfo))
        }
    }

}
