# -------------------------------------------------------------------
# Testing the Beta family (BE)
#
# TODO(R):
#   -dBE(0, mu = numeric()): Returns 0
#   -qBE(0, mu = numeric()): Returns 0
#   -pBE(0, mu = numeric()): Returns 0
#   -qBE(..., log.p = TRUE) ist in meinen Augen kaputt!
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) {
    library("tinytest")
    library("gamlss.dist")
}



# -------------------------------------------------------------------
# Check if functions exist.
# -------------------------------------------------------------------
check_and_load <- c("BE", "dBE", "pBE", "qBE", "rBE")
for (f in check_and_load) {
    expect_silent(get(f, envir = getNamespace("gamlss.dist")),
                  info = sprintf("Could not load function '%s' from gamlss.dist", f))
    expect_inherits(get(f, envir = getNamespace("gamlss.dist")), "function",
                  info = sprintf("'%s' is not a function", f))
}
rm(f)


# -------------------------------------------------------------------
# Testing distribution function (pBE)
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(pBE(),
    info = "Expected error when 'q' is missing.")
expect_error(pBE("foo"),
    info = "Expected error when 'q' is not numeric.")
expect_error(pBE(mu = list()),
    info = "Expected error when 'q' is not numeric.")

# Testing sanity checks
expect_error(pBE(0, mu = "foo"),     info = "Expected error when 'mu' is not numeric.")
expect_error(pBE(0, mu = list()),    info = "Expected error when 'mu' is not numeric.")
expect_error(pBE(0, mu = -1),        info = "Expected error when 'mu = 0'.")
expect_error(pBE(0, mu =  0),        info = "Expected error when 'mu < 0'.")
expect_error(pBE(0, mu =  1),        info = "Expected error when 'mu = 1'.")
expect_error(pBE(0, mu =  2),        info = "Expected error when 'mu > 1'.")
expect_error(pBE(0, mu = c(0.3, 0.2, -0.001, 1.00001)),
    info = "Expected error when 'mu' contains at least one value outside (0, 1).")

expect_error(pBE(0, sigma = "foo"),     info = "Expected error when 'sigma' is not numeric.")
expect_error(pBE(0, sigma = list()),    info = "Expected error when 'sigma' is not numeric.")
expect_error(pBE(0, sigma = -1),        info = "Expected error when 'sigma = 0'.")
expect_error(pBE(0, sigma =  0),        info = "Expected error when 'sigma < 0'.")
expect_error(pBE(0, sigma =  1),        info = "Expected error when 'sigma = 1'.")
expect_error(pBE(0, sigma =  2),        info = "Expected error when 'sigma > 1'.")
expect_error(pBE(0, sigma = c(0.3, 0.2, -0.001, 1.00001)),
    info = "Expected error when 'sigma' contains at least one value outside (0, 1).")

# Testing default values
expect_identical(pBE(0.3), pBE(q = 0.3, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE),
    info = "Return differs from expected result when using default arguments.")
expect_identical(pBE(0.3), pBE(0.3, 0.5, 0.2, TRUE, FALSE),
    info = "Order of input arguments may have changed; got incorrect return.")

# Testing return values
expect_inherits(pBE(0), "numeric",
    info = "Expected numeric return.")
expect_inherits(pBE(0, mu = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(pBE(0, mu = numeric()), NA_real_,
    info = "Expected NA_real_ as return if mu is empty.")
expect_inherits(pBE(0, sigma = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(pBE(0, sigma = numeric()), NA_real_,
    info = "Expected NA_real_ as return if mu is empty.")

# Testing 'expansion' of q/mu/sigma if not of same length.
expect_identical(length(pBE(0.3, 0.5, 0.2)), 1L,
    info = "Expected return of length 1L if length of 'q' = 'mu' = 'sigma' = 1.")
expect_identical(length(pBE(seq(0.1, 0.9, length.out = 10), 0.3, 0.2)), 10L,
    info = "Expected return of length 10 (length of 'q' = 10).")
expect_identical(length(pBE(0.3, seq(0.1, 0.9, length.out = 10), 0.2)), 10L,
    info = "Expected return of length 10 (length of 'mu' = 10).")
expect_identical(length(pBE(0.3, 0.5, seq(0.1, 0.9, length.out = 10))), 10L,
    info = "Expected return of length 10 (length of 'sigma' = 10).")



my_pbeta <- function(q, mu, sigma) {
    pbeta(q, shape1 = (a <- mu * (1 - sigma^2) / (sigma^2)),
             shape2 = a * (1 - mu) / mu)
}
my_dbeta <- function(q, mu, sigma) {
    dbeta(q, shape1 = (a <- mu * (1 - sigma^2) / (sigma^2)),
             shape2 = a * (1 - mu) / mu)
}
my_qbeta <- function(q, mu, sigma) {
    qbeta(q, shape1 = (a <- mu * (1 - sigma^2) / (sigma^2)),
             shape2 = a * (1 - mu) / mu, ncp = 0)
}

# Testing correct answer(s)

# - CDF = 0 if q outside (0,1)
expect_equal(pBE(-0.0001), 0,       info = "Expected pBE(-0.0001) to evaluate to 0.")
expect_equal(pBE( 0.0000), 0,       info = "Expected pBE( 0.0000) to evaluate to 0.")
expect_equal(pBE(+1.0000), 1,       info = "Expected pBE(+1.0000) to evaluate to 1.")
expect_equal(pBE(+1.0001), 1,       info = "Expected pBE(+1.0001) to evaluate to 1.")

# - Testing against base R
q <- 0.1; mu <- 0.3; sigma <- 0.5
expect_equal(pBE(q, mu, sigma), my_pbeta(q, mu, sigma))
q <- seq(0.1, 0.9, by = 0.1)
expect_equal(pBE(q, mu, sigma), my_pbeta(q, mu, sigma))
rm(q, mu, sigma)


# - Testing 'lower tail' and 'log.p'
q     <- runif(10, 0.01, 0.99)
mu    <- runif(10, 0.01, 0.99)
sigma <- runif(10, 0.01, 0.99)
expect_equal(pBE(q, mu, sigma, lower.tail = FALSE), 1 - pBE(q, mu, sigma, lower.tail = TRUE),
    info = "Expected pBE(..., lower.tail = FALSE) to be equal to 1 - pBE(..., lower.tail = TRUE).")
expect_equal(pBE(q, mu, sigma, log.p = TRUE), log(pBE(q, mu, sigma, log.p = FALSE)),
    info = "Expected pBE(..., log.p = TRUE) to be equal to log(pBE(..., log.p = FALSE).")
expect_equal(pBE(q, mu, sigma, lower.tail = FALSE, log.p = TRUE),
             log(pBE(q, mu, sigma, lower.tail = FALSE, log.p = FALSE)),
    info = "Expected pBE(..., lower.tail = FALSE, log.p = TRUE) to be equal to
            log(pBE(..., lower.tail = FALSE, log.p = FALSE)).")


# -------------------------------------------------------------------
# Testing density function (dBE)
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(dBE(),
    info = "Expected error when 'x' is missing.")
expect_error(dBE("foo"),
    info = "Expected error when 'x' is not numeric.")
expect_error(dBE(mu = list()),
    info = "Expected error when 'x' is not numeric.")

# Testing sanity checks
expect_error(dBE(0, mu = "foo"),     info = "Expected error when 'mu' is not numeric.")
expect_error(dBE(0, mu = list()),    info = "Expected error when 'mu' is not numeric.")
expect_error(dBE(0, mu = -1),        info = "Expected error when 'mu = 0'.")
expect_error(dBE(0, mu =  0),        info = "Expected error when 'mu < 0'.")
expect_error(dBE(0, mu =  1),        info = "Expected error when 'mu = 1'.")
expect_error(dBE(0, mu =  2),        info = "Expected error when 'mu > 1'.")
expect_error(dBE(0, mu = c(0.3, 0.2, -0.001, 1.00001)),
    info = "Expected error when 'mu' contains at least one value outside (0, 1).")

expect_error(dBE(0, sigma = "foo"),     info = "Expected error when 'sigma' is not numeric.")
expect_error(dBE(0, sigma = list()),    info = "Expected error when 'sigma' is not numeric.")
expect_error(dBE(0, sigma = -1),        info = "Expected error when 'sigma = 0'.")
expect_error(dBE(0, sigma =  0),        info = "Expected error when 'sigma < 0'.")
expect_error(dBE(0, sigma =  1),        info = "Expected error when 'sigma = 1'.")
expect_error(dBE(0, sigma =  2),        info = "Expected error when 'sigma > 1'.")
expect_error(dBE(0, sigma = c(0.3, 0.2, -0.001, 1.00001)),
    info = "Expected error when 'sigma' contains at least one value outside (0, 1).")

# Testing default values
expect_identical(dBE(0.9), dBE(x = 0.9, mu = 0.5, sigma = 0.2, log = FALSE),
    info = "Return differs from expected result when using default arguments.")
expect_identical(dBE(0.9), dBE(0.9, 0.5, 0.2, FALSE),
    info = "Order of input arguments may have changed; got incorrect return.")

# Testing return values
expect_inherits(dBE(0), "numeric",
    info = "Expected numeric return.")
expect_inherits(dBE(0, mu = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(dBE(0, mu = numeric()), 0, # TODO(R): Good or bad?
    info = "Expected 0 as return if mu is empty.")
expect_inherits(dBE(0, sigma = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(dBE(0, sigma = numeric()), 0, # TODO(R): Good or bad?
    info = "Expected 0 as return if mu is empty.")

# Testing 'expansion' of q/mu/sigma if not of same length.
expect_identical(length(dBE(0.3, 0.5, 0.2)), 1L,
    info = "Expected return of length 1L if length of 'q' = 'mu' = 'sigma' = 1.")
expect_identical(length(dBE(seq(0.1, 0.9, length.out = 10), 0.3, 0.2)), 10L,
    info = "Expected return of length 10 (length of 'q' = 10).")
expect_identical(length(dBE(0.3, seq(0.1, 0.9, length.out = 10), 0.2)), 10L,
    info = "Expected return of length 10 (length of 'mu' = 10).")
expect_identical(length(dBE(0.3, 0.5, seq(0.1, 0.9, length.out = 10))), 10L,
    info = "Expected return of length 10 (length of 'sigma' = 10).")


# - CDF = 0 if q outside (0,1)
expect_equal(dBE(-0.0001), 0,       info = "Expected dBE(-0.0001) to evaluate to 0.")
expect_equal(dBE( 0.0000), 0,       info = "Expected dBE( 0.0000) to evaluate to 0.")
expect_equal(dBE(+1.0000), 0,       info = "Expected dBE(+1.0000) to evaluate to 0.")
expect_equal(dBE(+1.0001), 0,       info = "Expected dBE(+1.0001) to evaluate to 0.")

# - Testing against base R
q <- 0.1; mu <- 0.3; sigma <- 0.5
expect_equal(dBE(q, mu, sigma), my_dbeta(q, mu, sigma))
q <- seq(0.1, 0.9, by = 0.1)
expect_equal(dBE(q, mu, sigma), my_dbeta(q, mu, sigma))
rm(q, mu, sigma)

# - Testing 'lower tail' and 'log.p'
tmp <- seq(0.1, 0.9, by = 0.1)
expect_equal(qPE(tmp, 3, lower.tail = FALSE), rev(qPE(tmp, 3, lower.tail = TRUE)),
    info = "Expected qPE(..., lower.tail = FALSE) to be equal to rev(qPE(..., lower.tail = TRUE)).")
expect_true(all(is.nan(qPE(tmp, 3, log.p = TRUE))),
    info = "Expected NaNs as result when log.p = TRUE.")
expect_true(all(is.nan(qPE(tmp, 3, lower.tail = FALSE, log.p = TRUE))),
    info = "Expected NaNs as result when log.p = TRUE.")
rm(tmp)


# - Testing 'log'

# TODO(R): This can fail as dBE(..., log = TRUE)
#          returns very negative numbers -> resulting in log(...)
#          to get -Inf.
# set.seed(43)
# q     <- runif(10, 0.01, 0.99)
# mu    <- runif(10, 0.01, 0.99)
# sigma <- runif(10, 0.01, 0.99)
# expect_equal(dBE(q, mu, sigma, log = TRUE), log(dBE(q, mu, sigma, log = FALSE)),
#     info = "Expected dBE(..., log = TRUE) to be equal to log(dBE(..., log = FALSE)).")
# rm(q, mu, sigma)



# -------------------------------------------------------------------
# Testing quantile function (qPE)
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(qBE(),
    info = "Expected error when 'x' is missing.")
expect_error(qBE("foo"),
    info = "Expected error when 'x' is not numeric.")
expect_error(qBE(mu = list()),
    info = "Expected error when 'x' is not numeric.")

# Testing sanity checks
expect_error(qBE(0, mu = "foo"),     info = "Expected error when 'mu' is not numeric.")
expect_error(qBE(0, mu = list()),    info = "Expected error when 'mu' is not numeric.")
expect_error(qBE(0, mu = -1),        info = "Expected error when 'mu = 0'.")
expect_error(qBE(0, mu =  0),        info = "Expected error when 'mu < 0'.")
expect_error(qBE(0, mu =  1),        info = "Expected error when 'mu = 1'.")
expect_error(qBE(0, mu =  2),        info = "Expected error when 'mu > 1'.")
expect_error(qBE(0, mu = c(0.3, 0.2, -0.001, 1.00001)),
    info = "Expected error when 'mu' contains at least one value outside (0, 1).")

expect_error(qBE(0, sigma = "foo"),     info = "Expected error when 'sigma' is not numeric.")
expect_error(qBE(0, sigma = list()),    info = "Expected error when 'sigma' is not numeric.")
expect_error(qBE(0, sigma = -1),        info = "Expected error when 'sigma = 0'.")
expect_error(qBE(0, sigma =  0),        info = "Expected error when 'sigma < 0'.")
expect_error(qBE(0, sigma =  1),        info = "Expected error when 'sigma = 1'.")
expect_error(qBE(0, sigma =  2),        info = "Expected error when 'sigma > 1'.")
expect_error(qBE(0, sigma = c(0.3, 0.2, -0.001, 1.00001)),
    info = "Expected error when 'sigma' contains at least one value outside (0, 1).")

# Testing default values
expect_identical(qBE(0.25), qBE(p = 0.25, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE),
    info = "Return differs from expected result when using default arguments.")
expect_identical(qBE(0.25), qBE(0.25, 0.5, 0.2, TRUE, FALSE),
    info = "Order of input arguments may have changed; got incorrect return.")

# Testing range
expect_identical(qBE(-0.0000001), NaN,   info = "Expected NaN if p < 0.")
expect_identical(qBE( 0.0000000), 0,     info = "Expected 0 if p = 0.")
expect_identical(qBE(+1.0000000), 1,     info = "Expected 0 if p = 1.")
expect_identical(qBE(+1.0000001), NaN,   info = "Expected NaN if p > 1.")

# Testing return values
expect_inherits(qBE(0), "numeric",
    info = "Expected numeric return.")
expect_inherits(qBE(0, mu = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(qBE(0, mu = numeric()), 0, # TODO(R): good or bad?
    info = "Expected 0 as return if mu is empty.")

# Testing 'expansion' of q/mu/sigma if not of same length.
expect_identical(length(qBE(0.3, 0.5, 0.2)), 1L,
    info = "Expected return of length 1L if length of 'q' = 'mu' = 'sigma' = 1.")
expect_identical(length(qBE(seq(0.1, 0.9, length.out = 10), 0.3, 0.2)), 10L,
    info = "Expected return of length 10 (length of 'q' = 10).")
expect_identical(length(qBE(0.3, seq(0.1, 0.9, length.out = 10), 0.2)), 10L,
    info = "Expected return of length 10 (length of 'mu' = 10).")
expect_identical(length(qBE(0.3, 0.5, seq(0.1, 0.9, length.out = 10))), 10L,
    info = "Expected return of length 10 (length of 'sigma' = 10).")

# - Testing against base R
q <- 0.1; mu <- 0.3; sigma <- 0.5
expect_equal(qBE(q, mu, sigma), my_qbeta(q, mu, sigma))
q <- seq(0.1, 0.9, by = 0.1)
expect_equal(qBE(q, mu, sigma), my_qbeta(q, mu, sigma))
rm(q, mu, sigma)

# - Testing 'lower tail' and 'log.p'
tmp <- seq(0.1, 0.9, by = 0.1)
expect_equal(qPE(tmp, 3, lower.tail = FALSE), rev(qPE(tmp, 3, lower.tail = TRUE)),
    info = "Expected qPE(..., lower.tail = FALSE) to be equal to rev(qPE(..., lower.tail = TRUE)).")
expect_true(all(is.nan(qPE(tmp, 3, log.p = TRUE))),
    info = "Expected NaNs as result when log.p = TRUE.")
expect_true(all(is.nan(qPE(tmp, 3, lower.tail = FALSE, log.p = TRUE))),
    info = "Expected NaNs as result when log.p = TRUE.")
rm(tmp)


# TODO(R): I think something is broken here with log.p = T,
#          my best guess is we take p <- exp(p) and then forward
#          it to qbeta with log.p = log.p -> My guess is it is
#          transformed twice.
# # - Testing 'log'
# set.seed(333)
# p     <- runif(10, 0.01, 0.99)
# mu    <- runif(10, 0.01, 0.99)
# sigma <- runif(10, 0.01, 0.99)
# expect_equal(qBE(p, mu, sigma, log.p = TRUE), log(qBE(p, mu, sigma, log = FALSE)),
#     info = "Expected qBE(..., log = TRUE) to be equal to log(qBE(..., log = FALSE)).")
# rm(p, mu, sigma)



# -------------------------------------------------------------------
# Testing random function
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(rBE(),
    info = "Expected error when 'n' is missing.")
expect_error(rBE("foo"),
    info = "Expected error when 'n' is not numeric.")
expect_error(rBE(-1),
    info = "Expected error when 'n' is <= 0.")
expect_error(rBE(c(10, 3, -5)),
    info = "Expected error when any 'n' is <= 0.")

# Testing sanity checks
expect_error(rBE(5, mu = "foo"),               info = "Expected error when 'mu' is not numeric.")
expect_error(rBE(5, mu = list()),              info = "Expected error when 'mu' is not numeric.")
expect_error(rBE(5, mu = 0 - eps),             info = "Expected error when 'mu < 0'.")
expect_error(rBE(5, mu = 0),                   info = "Expected error when 'mu = 0'.")
expect_error(rBE(5, mu = 1),                   info = "Expected error when 'mu = 1'.")
expect_error(rBE(5, mu = 1 + eps),             info = "Expected error when 'mu > 0'.")
expect_error(rBE(5, mu = c(0.1, -0.1, 1.3)),   info = "Expected error when 'mu' contains negative values.")

expect_error(rBE(5, sigma = "foo"),            info = "Expected error when 'mu' is not numeric.")
expect_error(rBE(5, sigma = list()),           info = "Expected error when 'mu' is not numeric.")
expect_error(rBE(5, sigma =  0),               info = "Expected error when 'mu = 0'.")
expect_error(rBE(5, sigma = -1),               info = "Expected error when 'mu' contains negative values.")
expect_error(rBE(5, sigma = c(10, 3, -0.001)), info = "Expected error when 'mu' contains negative values.")

# Testing return
expect_inherits(length(rBE(5)), "integer",
    info = "Expected integer return from rBE.")
expect_identical(length(rBE(5)), 5L,
    info = "Expected return of length 5L for rBE(5).")

# Testing against base R
set.seed(1);                 a <- rBE(20, mu = 0.3, sigma = 0.5)
set.seed(1); p <- runif(20); b <- my_qbeta(p, mu = 0.3, sigma = 0.5)
expect_equal(a, b, info = "Behavior of rBE differs from base R.")
set.seed(1);                 a <- rBE(20, mu = 0.9, sigma = 0.1)
set.seed(1); p <- runif(20); b <- my_qbeta(p, mu = 0.9, sigma = 0.1)
expect_equal(a, b, info = "Behavior of rBE differs from base R.")
rm(a, b, p)
set.seed(Sys.time())


## -------------------------------------------------------------------
## Testing PO family
## -------------------------------------------------------------------

# Misuse
expect_error(BE(foo = "bar"),
    info = "Expected error when using non-defined input arguments.")

# Incorrect specification of link
expect_error(BE("foo"),
    info = "Expected error when using invalid link.")
expect_error(BE(mu.link = "foo"),
    info = "Expected error when using invalid link.")

# Testing default family object
expect_silent(x <- BE(),
    info = "Expected BE() to be silent (using default mu.link = \"log\").")
expect_inherits(x, "gamlss.family",
    info = "Expected BE() to return an object of class 'gamlss.family'.")
expect_identical(x$family, c("BE", "Beta"),
    info = "Unexpected content in $family.")
expect_identical(x$parameters, list(mu = TRUE, sigma = TRUE),
    info = "Unexpected content in $parameters.")
expect_identical(x$nopar, 2.0,
    info = "Unexpected content in $nopar.")
expect_identical(x$type, "Continuous",
    info = "Unexpected content in $type.")
expect_identical(x$mu.link, "logit",
    info = "Unexpected content in $mu.link (expected \"logit\", default).")
expect_identical(x$sigma.link, "logit",
    info = "Unexpected content in $sigma.link (expected \"logit\", default).")

# Testing link functions and first/second derivs
expect_inherits(x$mu.linkfun, "function",
    info = "Content of $mu.linkfun expected to be a function.")
expect_identical(deparse(x$mu.linkfun)[[1L]], "function (mu) ",
    info = "Unexpected function call for $mu.linkfun.")
expect_inherits(x$mu.linkinv, "function",
    info = "Content of $mu.linkinv expected to be a function.")
expect_identical(deparse(x$mu.linkinv)[[1L]], "function (eta) ",
    info = "Unexpected function call for $mu.linkinv.")
expect_inherits(x$mu.dr, "function",
    info = "Content of $mu.dr expected to be a function.")
expect_identical(deparse(x$mu.dr)[[1L]], "function (eta) ",
    info = "Unexpected function call for $mu.dr.")
expect_inherits(x$dldm, "function",
    info = "Content of $dldm expected to be a function.")
expect_identical(deparse(x$dldm)[[1L]], "function (y, mu, sigma) ",
    info = "Unexpected function call for $dldm.")
expect_inherits(x$d2ldm2, "function",
    info = "Content of $d2ldm2 expected to be a function.")
expect_identical(deparse(x$d2ldm2)[[1L]], "function (mu, sigma) ",
    info = "Unexpected function call for $d2ldm2.")
expect_inherits(x$G.dev.incr, "function",
    info = "Content of $G.dev.incr expected to be a function.")
expect_identical(deparse(x$G.dev.incr)[[1L]], "function (y, mu, sigma, w, ...) ",
    info = "Unexpected function call for $G.dev.incr.")

requireNamespace("gamlss")
expect_inherits(x$rqres, "expression",
    info = "Unexpected content in $rqres.")
expect_equal(with(list(y = 0.53, mu = 0.2, sigma = 0.1, rqres = gamlss:::rqres), eval(x$rqres)),
             6.785943893799, tol = 0.5,
             info = "Unexpected result from $rqres.")
expect_equal(with(list(y = c(0.1, 0.8), mu = c(0.13, 0.64), sigma = 0.5, rqres = gamlss:::rqres), eval(x$rqres)),
             c(0.2762378219, 0.4772980872), tol = 0.5,
             info = "Unexpected result from $rqres.")

expect_inherits(x$mu.initial, "expression",
    info = "Unexpected content in $mu.initial.")
expect_identical(with(list(y = 3), eval(x$mu.initial)), 3,
    info = "Unexpected result of $mu.initial.")
expect_identical(with(list(y = c(1, 8, 3)), eval(x$mu.initial)),
                 (c(1, 8, 3) + mean(c(1, 8, 3))) / 2,
    info = "Unexpected result of $mu.initial.")

expect_identical(deparse(x$mu.valid)[[1L]], "function (mu) ",
    info = "Unexpected function call for $mu.valid.")
expect_identical(deparse(x$y.valid)[[1L]], "function (y) ",
    info = "Unexpected function call for $y.valid.")
expect_identical(deparse(x$mean)[[1L]], "function (mu, sigma) ",
    info = "Unexpected function call for $mean.")
expect_identical(deparse(x$variance)[[1L]], "function (mu, sigma) ",
    info = "Unexpected function call for $variance.")

# Testing functions
expect_equal(x$mu.linkinv(x$mu.linkfun(0.6)), 0.6,
    info = "Unexpected result when testing x > linkfun > inverse link fun (mu).")
expect_equal(x$mu.linkinv(x$sigma.linkfun(0.3)), 0.3,
    info = "Unexpected result when testing x > linkfun > inverse link fun (mu).")
expect_equal(x$mu.linkinv(x$mu.linkfun(c(0.943, 0.10, 0.3))), c(0.943, 0.10, 0.3),
    info = "Unexpected result when testing x > linkfun > inverse link fun (sigma).")
expect_equal(x$mu.linkinv(x$sigma.linkfun(c(0.943, 0.10, 0.3))), c(0.943, 0.10, 0.3),
    info = "Unexpected result when testing x > linkfun > inverse link fun (sigma).")


expect_true(x$y.valid(0.0000001))
expect_true(x$y.valid(0.9999999))
expect_true(x$mu.valid(0.0000001))
expect_true(x$mu.valid(0.9999999))
expect_true(x$sigma.valid(0.0000001))
expect_true(x$sigma.valid(0.9999999))

expect_false(x$y.valid(0))
expect_false(x$y.valid(1))
expect_false(x$mu.valid(0))
expect_false(x$mu.valid(1))
expect_false(x$sigma.valid(0))
expect_false(x$sigma.valid(1))



expect_identical(x$mean(0.5, 0.3), 0.5)
expect_identical(x$mean(seq(0.1, 0.9, by = 0.1), rnorm(8)), seq(0.1, 0.9, by = 0.1))
expect_identical(x$variance(0.3, 0.6), 0.6^2 * 0.3 * (1 - 0.3))
rm(x)
#
#
## Testing alternative link functions (above we only tested mainly
## mu.link = "log") which is the default.
#
## Testing mu.link = "inverse"
#expect_silent(x <- PO(mu.link = "inverse"),
#    info = "Expected function PO() to be silent when using 'mu.link = \"inverse\".")
#expect_identical(x$mu.link, "inverse")
#
#expect_identical(x$mu.linkfun(3), 1 / 3,
#    info = "Unexpected result from mu.linkfun (using mu.link = \"inverse\").")
#expect_identical(x$mu.linkinv(1 / 3), 3,
#    info = "Unexpected result from mu.linkinv (using mu.link = \"inverse\").")
#expect_equal(x$mu.linkinv(x$mu.linkfun(3)), 3,
#    info = "Unexpected result when testing x > linkfun > inverse link fun.")
#expect_equal(x$mu.linkinv(x$mu.linkfun(c(10, 3, 17.3))), c(10, 3, 17.3),
#    info = "Unexpected result when testing x > linkfun > inverse link fun.")
#expect_identical(x$mu.dr(3), -1 / 3^2,
#    info = "Unexpected result when calling $mu.dr.")
#rm(x)
#
## Testing mu.link = "sqrt"
#expect_silent(x <- PO(mu.link = "sqrt"),
#    info = "Expected function PO() to be silent when using 'mu.link = \"sqrt\".")
#expect_identical(x$mu.link, "sqrt")
#
#expect_identical(x$mu.linkfun(3), sqrt(3),
#    info = "Unexpected result from mu.linkfun (using mu.link = \"inverse\").")
#expect_identical(x$mu.linkinv(3), 9,
#    info = "Unexpected result from mu.linkinv (using mu.link = \"inverse\").")
#expect_equal(x$mu.linkinv(x$mu.linkfun(3)), 3,
#    info = "Unexpected result when testing x > linkfun > inverse link fun.")
#expect_equal(x$mu.linkinv(x$mu.linkfun(c(10, 3, 17.3))), c(10, 3, 17.3),
#    info = "Unexpected result when testing x > linkfun > inverse link fun.")
#expect_identical(x$mu.dr(3), 2 * 3,
#    info = "Unexpected result when calling $mu.dr.")
#rm(x)
#
## Testing mu.link = "identity"
#expect_silent(x <- PO(mu.link = "identity"),
#    info = "Expected function PO() to be silent when using 'mu.link = \"identity\".")
#expect_identical(x$mu.link, "identity")
#
#expect_identical(x$mu.linkfun(3), 3,
#    info = "Unexpected result from mu.linkfun (using mu.link = \"inverse\").")
#expect_identical(x$mu.linkinv(1 / 3), 1 / 3,
#    info = "Unexpected result from mu.linkinv (using mu.link = \"inverse\").")
#expect_equal(x$mu.linkinv(x$mu.linkfun(3)), 3,
#    info = "Unexpected result when testing x > linkfun > inverse link fun.")
#expect_equal(x$mu.linkinv(x$mu.linkfun(c(10, 3, 17.3))), c(10, 3, 17.3),
#    info = "Unexpected result when testing x > linkfun > inverse link fun.")
#expect_identical(x$mu.dr(3), 1,
#    info = "Unexpected result when calling $mu.dr.")
#rm(x)
#
#
#
