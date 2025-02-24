# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist") }

# Helper functions
source("../distributions-testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # For convenience: character vector with distribution parameters as expected
    params <- names(conf$param)

    # Create grid of possible link combinations
    links <- expand.grid(lapply(conf$param, function(x) x$links), stringsAsFactors = FALSE)

    # Create grid valid values for testing (for density, dist)
    testvals <- expand.grid(conf$values)

    for (i in seq_len(nrow(links))) {
        # Call constructor function (default args)
        cmd <- sprintf("obj <- %s(%s)", family,
            paste(sprintf("%s.link = \"%s\"", names(links[i, ]), links[i, ]), collapse = ", "))

        # Call constructor to create family object
        expect_silent(eval(parse(text = cmd)),
            info = sprintf("'%s' was not silent.", cmd))

        # Checking basic properties
        expect_inherits(obj, "gamlss.family",
            info = sprintf("object returned by %s(...) not of class 'gamlss.family'.", family))
        expect_inherits(obj, "family",
            info = sprintf("object returned by %s(...) not of class 'family'.", family))

        # Test if $type is specified correctly
        expect_identical(obj$type, conf$type,
            info = sprintf("%s(...)$type incorrect, expected '%s'.", family, conf$type))

        # Check if family was set correctly
        expect_identical(obj$family[[1]], family,
            info = sprintf("'%1$s()$family[[1]]' does not contain \"%1$s\" as expected.", family))

        # Test if $parameters is set correctly
        expect_identical(obj$parameters,
            as.list(setNames(rep(TRUE, length(params)), params)),
            info = sprintf("%s(...)$parameters does not include all expected parameters.", family))

        # Test if $nopar is correct
        expect_equal(obj$nopar, length(params),
            info = sprintf("%s(...)$nopar contains incorrect value.", family))

        # Checking xx.link entries, ensure the xx.linkfun and xx.invlink are available and
        # are both functions. 'p': Name of parameter to test.
        for (p in params) {
            expect_identical(obj[[paste0(p, ".link")]], links[i, p],
                info = sprintf("%s(...)$%s.link not '%s' as expected.", family, p, links[i, p]))
            # Check if xx.linkfun exists and is a function
            expect_true(paste0(p, ".linkfun") %in% names(obj),
                info = sprintf("%s(...)$%s.linkfun not found.", family, p))
            expect_inherits(obj[[paste0(p, ".linkfun")]], "function",
                info = sprintf("%s(...)$%s.linkfun is not a function.", family, p))
            # Check if xx.linkinv exists and is a function
            expect_true(paste0(p, ".linkinv") %in% names(obj),
                info = sprintf("%s(...)$%s.linkinv not found.", family, p))
            expect_inherits(obj[[paste0(p, ".linkinv")]], "function",
                info = sprintf("%s(...)$%s.linkinv is not a function.", family, p))
        }

        # Test that xx.valid exists and is a function
        for (p in c("y", params)) {
            expect_true(paste0(p, ".valid") %in% names(obj),
                info = sprintf("%s(...)$%s.valid not found.", family, p))
            expect_inherits(obj[[paste0(p, ".valid")]], "function",
                info = sprintf("%s(...)$%s.valid is not a function.", family, p))
        }

        # TODO(R): Not all families have initial values
        # Test that xx.initial is available and an expression
        for (p in params) {
            expect_true(paste0(p, ".initial") %in% names(obj),
                info = sprintf("%s(...)$%s.initial not found.", family, p))
            expect_inherits(obj[[paste0(p, ".initial")]], "expression",
                info = sprintf("%s(...)$%s.initial is not an expression.", family, p))
        }

        # Cleaning up
        rm(obj, cmd, j)
    }
}

