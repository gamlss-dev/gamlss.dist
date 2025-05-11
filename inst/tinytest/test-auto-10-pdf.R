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
        info = sprintf("Names of arguments or order of arguments changed in 'd%s()'!", family))

    # ---------------------------------------------------------------
    # Getting valid value set (inside defined range for dpqr)
    # ---------------------------------------------------------------
    grd_valid <- get_testgrid_valid(conf, FALSE, main = "x")

    tmpfun <- pdf
    for (i in seq_len(nrow(grd_valid))) {
        formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
        expect_silent(tmp <- tmpfun(),
            info = sprintf("'d%s%s' did not run silent.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'d%s%s' did not return numeric.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
        expect_inherits(tmp, "numeric",
            info = sprintf("'d%s%s' did not return result of length 1.", family, gsub("^pairlist", "", deparse(formals(tmpfun)))))
    }
    rm(tmpfun)

    # ---------------------------------------------------------------
    # Testing integral, should sum up to 1
    # ---------------------------------------------------------------
    # If continuous: Try numeric integration for all combinations
    if (conf$type == "Continuous") {
        for (i in seq_len(nrow(grd_valid))) {
            # Dynamically create pdf function with different default arguments
            args   <- grd_valid[i, , drop = FALSE]
            tmpfun <- pdf
            formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]

            # Integrate
            tmp <- integrate(pdf, lower = conf$support[1], upper = conf$support[2],
                             subdivisions = 1000L)
            expect_equal(tmp$value, 1,
                info = sprintf("Integral of 'd%s(x, %s)' does not result in 1.", family,
                     paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))
        }
        rm(args, tmpfun, tmp)
    # Else build sum
    } else {
        # TODO(R): Using 0:5000 here not conf$support as e.g., the Poisson
        #          distribution supports -Inf,Inf.
        for (i in seq_len(nrow(grd_valid))) {
            # Dynamically create pdf function with different default arguments
            args   <- grd_valid[i, , drop = FALSE]
            tmpfun <- pdf
            formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]

            # Integrate
            expect_equal(sum(pdf(seq.int(0, 5000))), 1,
                info = sprintf("Sum of 'd%s(x, %s)' does not result in 1.", family,
                     paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))
        }
        rm(args, tmpfun)
    }

}
