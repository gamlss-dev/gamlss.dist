# -------------------------------------------------------------------
# Helper functions for testing the different families and functions.
# -------------------------------------------------------------------

my_expect_silent <- function(f, expr) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": Expected '%s' to run silent.", f[[1]], deparse(expr))
    eval(substitute(expect_silent(expr, info = info)), envir = parent.frame())
}
my_expect_warning <- function(f, expr) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to throw a warning.", f[[1]], deparse(expr))
    eval(substitute(expect_warning(expr, info = info)), envir = parent.frame())
}
my_expect_error <- function(f, expr) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to fail (error).", f[[1]], deparse(expr))
    eval(substitute(expect_error(expr, info = info)), envir = parent.frame())
}
my_expect_true <- function(f, expr, class) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to evaluate to TRUE.",
                    f[[1]], deparse(expr))
    eval(substitute(expect_true(expr, info = info)), envir = parent.frame())
}
my_expect_false <- function(f, expr, class) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to evaluate to TRUE.",
                    f[[1]], deparse(expr))
    eval(substitute(expect_false(expr, info = info)), envir = parent.frame())
}
my_expect_inherits <- function(f, expr, class) {
    stopifnot(is.character(f))
    stopifnot(is.character(class))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to inherit from %s.",
                    f[[1]], deparse(expr), class[[1]])
    eval(substitute(expect_inherits(expr, class[[1]], info = info)), envir = parent.frame())
}
my_expect_identical <- function(f, expr, target) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to return %s.", f[[1]], deparse(expr), deparse(target))
    eval(substitute(expect_identical(expr, target, info = info)), envir = parent.frame())
}
my_expect_equal <- function(f, expr, target, ...) {
    stopifnot(is.character(f))
    expr <- substitute(expr)
    info <- sprintf("Family \"%s\": expected '%s' to return %s.", f[[1]], deparse(expr), deparse(target))
    eval(substitute(expect_equal(expr, target, info = info, ...)), envir = parent.frame())
}

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

