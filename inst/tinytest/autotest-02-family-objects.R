# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist") }

# Helper functions
source("config/get_testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Create default family object
    cmd <- sprintf("obj <- %s()", family)

    # Call constructor to create family object
    expect_silent(eval(parse(text = cmd)),
        info = sprintf("'%s' was not silent.", cmd))

    # Checking xx.link entries, ensure the xx.linkfun and xx.invlink are available and
    # are both functions. 'p': Name of parameter to test.
    for (p in conf$params) {
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
    # Cleaning up
    rm(cmd, p)

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
        as.list(setNames(rep(TRUE, length(conf$params)), conf$params)),
        info = sprintf("%s(...)$parameters does not include all expected parameters.", family))

    # Test if $nopar is correct
    expect_equal(obj$nopar, length(conf$params),
        info = sprintf("%s(...)$nopar contains incorrect value.", family))

    # Test that xx.valid exists and is a function
    for (p in c("y", conf$params)) {
        expect_true(paste0(p, ".valid") %in% names(obj),
            info = sprintf("%s(...)$%s.valid not found.", family, p))
        expect_inherits(obj[[paste0(p, ".valid")]], "function",
            info = sprintf("%s(...)$%s.valid is not a function.", family, p))
    }

    # Find all elements '^d.*' which should be the derivatives, and check
    # that they are functions.
    derivs <- names(obj)[grepl("^d.*", names(obj))]
    for (d in derivs) {
        expect_inherits(obj[[d]], "function",
            info = sprintf("%s(...)$%s (derivative) is not a function.", family, d))
    }

    # Function G.dev.incr
    expect_inherits(obj[["G.dev.incr"]], "function",
        info = sprintf("%s(...)$G.devv.incr is not a function.", family))

    # Function G.dev.incr
    expect_inherits(obj[["rqres"]], "expression",
        info = sprintf("%s(...)$rqres is not an expression.", family))

    # TODO(R): Not all families have initial values
    # Test that xx.initial is available and an expression
    for (p in conf$params) {
        expect_true(paste0(p, ".initial") %in% names(obj),
            info = sprintf("%s(...)$%s.initial not found.", family, p))
        expect_inherits(obj[[paste0(p, ".initial")]], "expression",
            info = sprintf("%s(...)$%s.initial is not an expression.", family, p))
    }

    # Mean and variance
    for (d in c("mean", "variance")) {
        expect_true(d %in% names(obj),
            info = sprintf("%s(...)$%s not found.", family, d))
        expect_inherits(obj[[d]], "function",
            info = sprintf("%s(...)$%s is not a function.", family, d))
    }

    # Cleaning up
    rm(obj)
}

