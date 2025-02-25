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

    # For convenience: character vector with distribution parameters as expected
    # TODO(R): Currently the config does not specify anything for 'link = "own"',
    #          so this feature is currently not tested.
    links  <- expand.grid(lapply(conf[conf$params], names), stringsAsFactors = FALSE)

    # ---------------------------------------------------------------
    # Testing all available link combinations. Based on the test run
    # before these we assume that the constructor function works and
    # returns properly defined family objects.
    #
    # Here we are testing all possible link combinations, and test
    # that they work as expected.
    # ---------------------------------------------------------------
    for (i in seq_len(nrow(links))) {
        # Call constructor function (default args)
        cmd <- sprintf("obj <- %s(%s)", family,
            paste(sprintf("%s.link = \"%s\"", names(links[i, ]), links[i, ]), collapse = ", "))

        # Call constructor to create family object
        expect_silent(eval(parse(text = cmd)),
            info = sprintf("'%s' was not silent.", cmd))

        # From the previous tests we know that <param>.valid is properly defined, testing
        # if it properly returns TRUE/FALSE given the configured valies (conf).
        for (p in conf$params) {
            # Check if <param>.valid looks as expected
            tmp <- formals(obj[[paste0(p, ".valid")]])
            expect_identical(length(tmp), 1L,      info = sprintf("%s(...)$%s.valid expected to have one argument.", family, p))
            expect_true(is.symbol(tmp[[1]]),       info = sprintf("%s(...)$%s.valid arguments must not have defaults.", family, p))

            for (v in conf[[c(p, "valid")]])
                expect_true(obj[[paste0(p, ".valid")]](v),
                    info = sprintf("%s: %s.valid(%s) should evaluate to TRUE, got FALSE.", cmd, p, fmt(v)))
            for (v in conf[[c(p, "invalid")]])
                expect_false(obj[[paste0(p, ".valid")]](v),
                    info = sprintf("%s: %s.valid(%s) should evaluate to FALSE, got TRUE.", cmd, p, fmt(v)))
        }
        rm(p, v, tmp)

        # Checking xx.link entries, ensure the xx.linkfun and xx.invlink are available and
        # are both functions. 'p': Name of parameter to test.
        for (p in conf$params) {
            expect_identical(obj[[paste0(p, ".link")]], links[i, p],
                info = sprintf("%s(...)$%s.link not '%s' as expected.", family, p, links[i, p]))

            # Check if xx.linkfun exists and is a function
            linkfun <- obj[[paste0(p, ".linkfun")]]
            expect_true(!is.null(linkfun),         info = sprintf("%s(...)$%s.linkfun not found.", family, p))
            expect_inherits(linkfun, "function",   info = sprintf("%s(...)$%s.linkfun is not a function.", family, p))

            # Check if xx.linkinv exists and is a function
            linkinv <- obj[[paste0(p, ".linkinv")]]
            expect_true(!is.null(linkinv),         info = sprintf("%s(...)$%s.linkinv not found.", family, p))
            expect_inherits(linkinv, "function",   info = sprintf("%s(...)$%s.linkinv is not a function.", family, p))

            # Check that linkfun and linkinv are functions expecting one parameter
            tmp <- formals(linkfun)
            expect_identical(length(tmp), 1L,      info = sprintf("%s(...)$%s.linkfun expected to have one argument.", family, p))
            expect_true(is.symbol(tmp[[1]]),       info = sprintf("%s(...)$%s.linkfun arguments must not have defaults.", family, p))
            rm(tmp, linkfun)
            tmp <- formals(linkinv)
            expect_identical(length(tmp), 1L,      info = sprintf("%s(...)$%s.linkinv expected to have one argument.", family, p))
            expect_true(is.symbol(tmp[[1]]),       info = sprintf("%s(...)$%s.linkinv arguments must not have defaults.", family, p))
            rm(tmp, linkinv)
        }

        # Now we are sure the link functions are defined properly, let us test if
        # linkinv(linkfun(x)) is equal to x for valid parameters.
        for (p in conf$params) {
            linkfun  <- obj[[paste0(p, ".linkfun")]]
            linkinv  <- obj[[paste0(p, ".linkinv")]]
            vals     <- conf[[c(p, links[i, p], "valid")]]
            vals_str <- sprintf("c(%s)", paste(fmt(vals), collapse = ","))
            expect_equal(linkinv(linkfun(vals)), vals,
                info = sprintf("%s: %s.linkinv(%s.linkfun(%s)) not equal to itself.", cmd, p, p, vals_str))
            rm(linkfun, linkinv, vals, vals_str)
        }

        # Cleaning up
        rm(cmd, obj, p)
    }
}

