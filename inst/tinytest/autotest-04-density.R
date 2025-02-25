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
    f <- formals(cdf)

    # Checking arguments, expecting 'x', <params>, 'log'.
    expect_identical(names(f), (expected <- c("x", conf$params, "log")),
        info = sprintf("Missing arguments for 'd%s()', expected: %s",
                       family, paste(expected, collapse = ", ")))

    # Expecting log = FALSE per default
    expect_false(f$log, info = "'d%s()': Expected default argument 'log = FALSE'.")

    # Using default links from obj
    links <- unlist(obj[sprintf("%s.link", conf$param)])
    names(links) <- gsub("\\.link$", "", names(links))

    # ---------------------------------------------------------------
    # Setting up combinations of valid values
    # ---------------------------------------------------------------
    valid <- list(y = conf$y$valid)
    for (p in conf$param) valid[[p]] <- conf[[c(p, links[p], "valid")]]

    expect_silent(tmp <- cdf(valid$y, valid$mu, valid$sigma),
        info = sprintf("'d%s(...)' expected to be silent when tested with all-valid input values.", family))
    expect_silent(is.numeric(tmp),
        info = sprintf("'d%s(...)' expected to return numeric result.", family))
    expect_silent(is.numeric(tmp),
        info = sprintf("'d%s(...)' returned NA when used with all-valid input values.", family))
    expect_identical(length(tmp), max(sapply(valid, length)),
        info = sprintf("'d%s(...)' returned result of unexpected length.", family))


}
