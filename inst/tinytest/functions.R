# -------------------------------------------------------------------
# Helper functions for testing the different families and functions.
# -------------------------------------------------------------------

my_expect_silent <- function(expr, info = NULL, family = NULL, env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to run silent.", deparse(expr))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_silent(expr, info = info)), envir = env)
}

###my_expect_warning <- function(expr, info = NULL, family = "foo", env = parent.frame()) {
###  expr <- substitute(expr)  # Capture the expression BEFORE evaluation
###
###  merged_env <- new.env(parent = parent.env(env))
###  parent.env(merged_env) <- env
###
###  # If expr is a string, convert it into an actual expression
###  if (inherits(expr, "name")) expr <- parse(text = eval(expr, env))[[1]]
###  # Resolve `expr` in `merged_env`, but evaluate in `parent.frame()`
###  resolved_expr <- eval(bquote(.(expr)), envir = merged_env)
###  ##print(resolved_expr)
###
###  # Ensure `tinytest` registers the test by evaluating in `parent.frame()`
###  eval(bquote(expect_warning(.(resolved_expr), info = .(info))), envir = env)
###}
my_expect_warning <- function(expr, info = NULL, family = NULL, env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to run silent.", deparse(expr))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_warning(expr, info = info)), envir = env)
}


my_expect_error <- function(expr, info = NULL, family = NULL, ..., env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to fail with an error.", deparse(expr))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_error(expr, info = info, ...)), envir = env)
}

my_expect_true <- function(expr, info = NULL, family = NULL, env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to evaluate to TRUE.", deparse(expr))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_true(expr, info = info)), envir = env)
}

my_expect_false <- function(expr, info = NULL, family = NULL, env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to evaluate to FALSE.", deparse(expr))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_false(expr, info = info)), envir = env)
}

my_expect_inherits <- function(expr, class, info = NULL, family = NULL, env = NULL) {
    stopifnot(is.null(class) || is.character(class))
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to inherit from '%s'.", deparse(expr), class)
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_inherits(expr, class, info = info)), envir = env)
}

my_expect_identical <- function(expr, target, info = NULL, family = NULL, env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to be identical to %s.", deparse(expr), deparse(target))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_identical(expr, target, info = info)), envir = env)
}

my_expect_equal <- function(expr, target, info = NULL, family = NULL, ..., env = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))
    stopifnot(is.null(env) || inherits(env, "environment"))
    if (is.null(env)) env <- parent.frame()

    expr <- substitute(expr)
    if (is.null(info))    info <- sprintf("Expected '%s' to be equal to %s.", deparse(expr), deparse(target))
    if (!is.null(family)) info <- sprintf("Family \"%s\": %s", family, info)
    eval(substitute(expect_equal(expr, target, info = info, ...)), envir = env)
}


# -------------------------------------------------------------------
# Limits for discrete and continuous families (for automated tests)
# -------------------------------------------------------------------
get_limits <- function(type = c("discrete", "continuous")) {
    type <- match.arg(type)

    if (type == "discrete") {
        res <- list(
               y.range  = c(0,Inf),
               p.range  = c(0, 1),
               mu.range = c(0,Inf),
            sigma.range = c(0,Inf),
               nu.range = c(0,Inf),
              tau.range = c(0,Inf),
                 mu.val = c(.5,5,10,30),
              sigma.val = c(.5,5,10,30),
                 nu.val = c(.5,5,10,30),
                tau.val = c(.5,5,10,30),
                      N = 100,
                     bd = NULL,
                   save = TRUE,
                  trace = TRUE,
                   crit = 0.001
        )
    } else {
        res <- list(
                y.range = c(-Inf,Inf), # the range of the resposnse..
               p.range  = c(0, 1),
               mu.range = c(-Inf,Inf), #.
            sigma.range = c(0,Inf),
               nu.range = c(0,Inf),
              tau.range = c(0,Inf),
                 mu.val = c(0,1,10,30), # which values to test
              sigma.val = c(1,5,10,30),
                 nu.val = c(1,5,10,30),
                tau.val = c(1,5,10,30),
                      N = 100,
                   save = TRUE,
                  trace = TRUE,
                   crit = 0.001 # what difference we are looking
        )
    }
    return(res)
}


# -------------------------------------------------------------------
# Limits for discrete and continuous families (for automated tests)
# -------------------------------------------------------------------
get_test_config <- function(family) {
    eps <- sqrt(.Machine$double.eps)
    if (family %in% c("NO", "NO2")) {
        type    <- "Continuous"
        support <- c(-Inf, Inf)
        # Distribution parameters with support (range) and all available links
        params <- list("mu"    = list(range = c(-Inf, Inf),
                                      links = c("inverse", "log", "identity", "own")),
                       "sigma" = list(range = c(0, Inf),
                                      links = c("inverse", "log", "identity", "own"))
                       )
        # Valid values used for testing dpqr
        values  <- list("y"     = c(-30, -5 -1, 0, 1, 5, 30),
                        "mu"    = c(-30, -5 -1, 0, 1, 5, 30),
                        "sigma" = c(eps, 1, 5, 30))
        # Illegal values, any combination of these must fail
        illegal <- list("y"     = 0,
                        "mu"    = 0,
                        "sigma" = c(0, -0.0001, -100))
    } else if (family == "LO") {
        type    <- "Continuous"
        support <- c(-Inf, Inf)
        # Distribution parameters with support (range) and all available links
        params <- list("mu"    = list(range = c(-Inf, Inf),
                                      links = c("inverse", "log", "identity", "own")),
                       "sigma" = list(range = c(0, Inf),
                                      links = c("inverse", "log", "identity", "own"))
                       )
        # Valid values used for testing dpqr
        values <- list("y"     = c(-30, -5 -1, 0, 1, 5, 30),
                       "mu"    = c(-30, -5 -1, 0, 1, 5, 30),
                       "sigma" = c(eps, 1, 5, 30))
        # Illegal values, any combination of these must fail
        illegal <- list("y"     = 0,
                        "mu"    = 0,
                        "sigma" = c(0, -0.0001, -100))
    } else if (family == "BE") {
        type   <- "Continuous"
        support <- c(0, 1)
        # Distribution parameters with support (range) and all available links
        params <- list("mu"    = list(range = c(0, 1),
                                      links = c("logit", "probit", "cloglog", "cauchit", "log", "own")),
                       "sigma" = list(range = c(0, 1),
                                      links = c("logit", "probit", "cloglog", "cauchit", "log", "own"))
                       )
        # Valid values used for testing dpqr
        values <- list("y"     = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps),
                       "mu"    = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps),
                       "sigma" = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps))
        # Illegal values, any combination of these must fail
        illegal <- list("y"     = c(-10, -0.00001, 0, 1, 1.00001, 10),
                        "mu"    = c(-10, -0.00001, 0, 1, 1.00001, 10),
                        "sigma" = c(-10, -0.00001, 0, 1, 1.00001, 10))
    } else {
        stop("Test configuration for family \"", family, "\" not defined!")
    }
    return(list(type = type, support = support, params = params,
                values = values, illegal = illegal))
}








