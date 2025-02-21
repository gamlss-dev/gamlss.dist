# -------------------------------------------------------------------
# Helper functions for testing the different families and functions.
# -------------------------------------------------------------------

my_expect_silent <- function(expr, info = NULL, family = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to run silent.", deparse(expr))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_silent(expr, info = info)), envir = parent.frame())
}

my_expect_warning <- function(expr, info = NULL, family = NULL, ...) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to throw a warning.", deparse(expr))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_warning(expr, info = info, ...)), envir = parent.frame())
}

my_expect_error <- function(expr, info = NULL, family = NULL, ...) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to fail with an error.", deparse(expr))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_error(expr, info = info, ...)), envir = parent.frame())
}

my_expect_true <- function(expr, info = NULL, family = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to evaluate to TRUE.", deparse(expr))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_true(expr, info = info)), envir = parent.frame())
}

my_expect_false <- function(expr, info = NULL, family = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to evaluate to FALSE.", deparse(expr))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_false(expr, info = info)), envir = parent.frame())
}

my_expect_inherits <- function(expr, class, info = NULL, family = NULL) {
    stopifnot(is.null(class) || is.character(class))
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to inherit from '%s'.", deparse(expr), class)
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_inherits(expr, class, info = info)), envir = parent.frame())
}

my_expect_identical <- function(expr, target, info = NULL, family = NULL) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to be identical to %s.", deparse(expr), deparse(target))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_identical(expr, target, info = info)), envir = parent.frame())
}

my_expect_equal <- function(expr, target, info = NULL, family = NULL, ...) {
    stopifnot(is.null(info) || is.character(info))
    stopifnot(is.null(family) || is.character(family))

    expr <- substitute(expr)
    if (is.null(info))   info <- sprintf("Expected '%s' to be equal to %s.", deparse(expr), deparse(target))
    if (is.null(family)) info <- sprintf("Family \"%s\": %s", info)
    eval(substitute(expect_equal(expr, target, info = info, ...)), envir = parent.frame())
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

