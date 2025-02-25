# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist") }

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

    # Setting up family with default arguments used for testing
    obj <- get(family, envir = getNamespace("gamlss.dist"))()

    # TODO(R): Currently the config does not specify anything for 'link = "own"',
    #          so this feature is currently not tested.
    links  <- expand.grid(lapply(conf[conf$params], names), stringsAsFactors = FALSE)

    # Getting d<FAM> function (pdf)
    cdf <- get(sprintf("d%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing arguments, order of arguments, and default values
    # ---------------------------------------------------------------
    f <- formals(cdf)
    expect_identical(as.list(f), as.list(conf$arguments$d),
        info = sprintf("Names of arguments or order of arguments changed in 'd%s()'!", family))
    # Expecting log = FALSE per default
    expect_false(f$log, info = "'d%s()': Expected default argument 'log = FALSE'.")

    # ---------------------------------------------------------------
    # Setting up combinations of valid values
    # ---------------------------------------------------------------
    # Extracting default links from the family for testing
    links <- unlist(obj[sprintf("%s.link", conf$param)])
    names(links) <- gsub("\\.link$", "", names(links))


    valid <- list(y = conf$y$valid)
    for (p in conf$param) valid[[p]] <- conf[[c(p, links[p], "valid")]]
    valid
    sapply(valid,length)

    expect_silent(tmp <- cdf(valid$y, valid$mu, valid$sigma),
            info = sprintf("'d%s(...)' expected to be silent when tested with all-valid input values.", family))
    # TODO(R): I just call it agian as I do not get object 'tmp' if the call
    #          above is not silent (we currently have this with dGT)
    tmp <- cdf(valid$y, valid$mu, valid$sigma)
    expect_silent(is.numeric(tmp),
        info = sprintf("'d%s(...)' expected to return numeric result.", family))
    expect_silent(is.numeric(tmp),
        info = sprintf("'d%s(...)' returned NA when used with all-valid input values.", family))
    expect_identical(length(tmp), max(sapply(valid, length)),
        info = sprintf("'d%s(...)' returned result of unexpected length.", family))

    # Testing invalid parameters (should aus an error)
    for (p in conf$param) {
        for (v in conf[[c(p, links[p], "invalid")]]) {
            tmpfun <- cdf
            formals(tmpfun)[c("x", p)] <- c(valid$y[1], v)
            expect_error(tmpfun(),
                info = sprintf("'d%s(x = %s, %s = %s)' (invalid %s) did not throw a warning.",
                               family, fmt(valid$y[1]), p, fmt(v), p))
        }
    }


    # ---------------------------------------------------------------
    # Testing if the integral of the density sums up to one
    # ---------------------------------------------------------------
    grd <- expand.grid(valid[!names(valid) == "y"])

    # If continuous: Try numeric integration for all combinations
    if (conf$type == "Continuous") {
        for (i in seq_along(nrow(grd))) {
            # Dynamically create cdf function with different default arguments
            args   <- grd[i, , drop = FALSE]
            tmpfun <- cdf
            formals(tmpfun)[names(grd)] <- grd[i, ]

            # Integrate
            tmp <- integrate(cdf, lower = conf$support[1], upper = conf$support[2],
                             subdivisions = 1000L)
            expect_equal(tmp$value, 1,
                info = sprintf("Integral of 'd%s(x, %s)' does not result in 1.", family,
                     paste(sprintf("%s = %s", names(grd), fmt(grd[i, ])), collapse = ", ")))
        }
        rm(args, tmpfun, tmp)
    # Else build sum
    } else {
        # TODO(R): Using 0:5000 here not conf$support as e.g., the Poisson
        #          distribution supports -Inf,Inf.
        for (i in seq_along(nrow(grd))) {
            # Dynamically create cdf function with different default arguments
            args   <- grd[i, , drop = FALSE]
            tmpfun <- cdf
            formals(tmpfun)[names(grd)] <- grd[i, ]

            # Integrate
            expect_equal(sum(cdf(seq.int(0, 5000))), 1,
                info = sprintf("Sum of 'd%s(x, %s)' does not result in 1.", family,
                     paste(sprintf("%s = %s", names(grd), fmt(grd[i, ])), collapse = ", ")))
        }
        rm(args, tmpfun)
    }





}
