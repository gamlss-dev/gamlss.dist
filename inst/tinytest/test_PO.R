# -------------------------------------------------------------------
# Testing the PO distribution.
#
# TODO(R):
# - ppois(0, 0) Returns, pPO(0, 0) throws an error.
# - pdf(p, numeric()) returns NA_real_, ppois(0, numeric()) returns empty vector.
# - PO$mean and PO$variance allow for negative inputs (just the identify function).
# - Currently have no tests for the evaluation of:
#     - x$rqres = expression(rqres(pfun="pPO", type="Discrete", ymin=0, y=y, mu=mu))
#     - x$mu.initial = expression({mu <- (y +mean(y))/2 })
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) {
    library("tinytest")
    library("gamlss.dist")
}



# -------------------------------------------------------------------
# Check if functions exist.
# -------------------------------------------------------------------
check_and_load <- c("PO", "dPO", "pPO", "qPO", "rPO")
for (f in check_and_load) {
    expect_silent(get(f, envir = getNamespace("gamlss.dist")),
                  info = sprintf("Could not load function '%s' from gamlss.dist", f))
    expect_inherits(get(f, envir = getNamespace("gamlss.dist")), "function",
                  info = sprintf("'%s' is not a function", f))
}
rm(f)


# -------------------------------------------------------------------
# Testing distribution function (pPO)
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(pPO(),
    info = "Expected error when 'q' is missing.")
expect_error(pPO("foo"),
    info = "Expected error when 'q' is not numeric.")
expect_error(pPO(mu = list()),
    info = "Expected error when 'q' is not numeric.")

# Testing sanity checks
expect_error(pPO(0, mu = "foo"),
    info = "Expected error when 'mu' is not numeric.")
expect_error(pPO(0, mu = list()),
    info = "Expected error when 'mu' is not numeric.")
expect_error(pPO(0, mu =  0),
    info = "Expected error when 'mu = 0'.")
expect_error(pPO(0, mu = -1),
    info = "Expected error when 'mu' contains negative values.")
expect_error(pPO(0, mu = c(10, 3, -0.001)),
    info = "Expected error when 'mu' contains negative values.")

# Testing default values
expect_identical(pPO(0), pPO(q = 0, mu = 1, lower.tail = TRUE, log.p = FALSE),
    info = "Return differs from expected result when using default arguments.")
expect_identical(pPO(0), pPO(0, 1, TRUE, FALSE),
    info = "Order of input arguments may have changed; got incorrect return.")

# Testing return values
expect_inherits(pPO(0), "numeric",
    info = "Expected numeric return.")
expect_inherits(pPO(0, mu = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(pPO(0, mu = numeric()), NA_real_,
    info = "Expected NA_real_ as return if mu is empty.")

# Testing 'expansion' of q/mu if not of same length.
expect_identical(length(pPO(3, seq_len(1))), 1L,
    info = "Expected return of length 1L when one 'mu' is of length 1L, 'q' of length 1L.")
expect_identical(length(pPO(3, seq_len(10))), 10L,
    info = "Expected return of length 1L when one 'mu' is of length 10L, 'q' of length 1L.")
expect_identical(length(pPO(seq_len(1), 3)), 1L,
    info = "Expected return of length 1L when one 'q' is of length 1L, 'mu' of length 1L.")
expect_identical(length(pPO(seq_len(10), 3)), 10L,
    info = "Expected return of length 1L when one 'q' is of length 10L, 'mu' of length 1L.")

# Testing correct answer(s)
# - 'Manual' tests
expect_equal(pPO(0, 1), 1 / exp(1),
    info = "Expected pPO(0, 1) to evaluate to 1 / exp(1).")
expect_equal(pPO(0, 8.3), 1 / exp(8.3),
    info = "Expected pPO(0, 8.3) to evaluate to 1 / exp(8.3).")

# - CDF = 0 if q < 0
expect_equal(pPO(-0.0001, 1), 0,
    info = "Expected pPO(-0.0001, 1) to evaluate to 0.")
expect_equal(pPO(-3:3, 1), ppois(-3:3, 1),
    info = "Results for pPO(-3:3, 1) differ from ppois(-3:3, 1)!")

# - Testing against base R
expect_equal(pPO(0:10, 3), ppois(0:10, 3),
    info = "Expected pPO() to be equal to ppois() from base R.")
expect_equal(pPO(0:10, 17.3), ppois(0:10, 17.3),
    info = "Expected pPO() to be equal to ppois() from base R when using non-integer 'mu'.")
expect_equal(pPO(100:110, 132), ppois(100:110, 132),
    info = "Expected pPO() to be equal to ppois() from base R when using large 'mu'.")
tmp <- c(0.1, 0.5, 13.2)
expect_equal(pPO(tmp, 3), ppois(tmp, 3),
    info = "Expected pPO() to be equal to ppois() when using non-integer values for 'q'.")
rm(tmp)

# - Testing 'lower tail' and 'log.p'
expect_equal(pPO(0:5, 3, lower.tail = FALSE), 1 - pPO(0:5, 3, lower.tail = TRUE),
    info = "Expected pPO(..., lower.tail = FALSE) to be equal to 1 - pPO(..., lower.tail = TRUE).")
expect_equal(pPO(0:5, 3, log.p = TRUE), log(pPO(0:5, 3, log.p = FALSE)),
    info = "Expected pPO(..., log.p = TRUE) to be equal to log(pPO(..., log.p = FALSE).")
expect_equal(pPO(0:5, 3, log.p = TRUE), log(pPO(0:5, 3, log.p = FALSE)),
    info = "Expected pPO(..., log.p = TRUE) to be equal to log(pPO(..., log.p = FALSE).")
expect_equal(pPO(0:5, 3, lower.tail = FALSE, log.p = TRUE),
             log(pPO(0:5, 3, lower.tail = FALSE, log.p = FALSE)),
    info = "Expected pPO(..., lower.tail = FALSE, log.p = TRUE) to be equal to
            log(pPO(..., lower.tail = FALSE, log.p = FALSE)).")


# -------------------------------------------------------------------
# Testing density function (dPO)
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(dPO(),
    info = "Expected error when 'x' is missing.")
expect_error(dPO("foo"),
    info = "Expected error when 'x' is not numeric.")
expect_error(dPO(mu = list()),
    info = "Expected error when 'x' is not numeric.")

# Testing sanity checks
expect_error(dPO(0, mu = "foo"),
    info = "Expected error when 'mu' is not numeric.")
expect_error(dPO(0, mu = list()),
    info = "Expected error when 'mu' is not numeric.")
expect_error(dPO(0, mu =  0),
    info = "Expected error when 'mu = 0'.")
expect_error(dPO(0, mu = -1),
    info = "Expected error when 'mu' contains negative values.")
expect_error(dPO(0, mu = c(10, 3, -0.001)),
    info = "Expected error when 'mu' contains negative values.")

# Testing default values
expect_identical(dPO(0), dPO(x = 0, mu = 1, log = FALSE),
    info = "Return differs from expected result when using default arguments.")
expect_identical(dPO(0), dPO(0, 1, FALSE),
    info = "Order of input arguments may have changed; got incorrect return.")

# Testing return values
expect_inherits(dPO(0), "numeric",
    info = "Expected numeric return.")
expect_inherits(dPO(0, mu = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(dPO(0, mu = numeric()), NA_real_,
    info = "Expected NA_real_ as return if mu is empty.")

# Testing 'expansion' of q/mu if not of same length.
expect_identical(length(dPO(3, seq_len(1))), 1L,
    info = "Expected return of length 1L when one 'mu' is of length 1L, 'q' of length 1L.")
expect_identical(length(dPO(3, seq_len(10))), 10L,
    info = "Expected return of length 1L when one 'mu' is of length 10L, 'q' of length 1L.")
expect_identical(length(dPO(seq_len(1), 3)), 1L,
    info = "Expected return of length 1L when one 'x' is of length 1L, 'mu' of length 1L.")
expect_identical(length(dPO(seq_len(10), 3)), 10L,
    info = "Expected return of length 1L when one 'x' is of length 10L, 'mu' of length 1L.")

# Testing correct answer(s)
# - 'Manual' tests
expect_equal(dPO(2, 3), 3^2 * exp(-3) / factorial(2),
    info = "Expected dPO(2, 3) to evaluate to 3^2 * exp(-3) / factorial(2)).")
expect_equal(dPO(5, 8.3), 8.3^5 * exp(-8.3) / factorial(5),
    info = "Expected dPO(2, 3) to evaluate to 8.3^5 * exp(-8.5) / factorial(5)).")

# - CDF = 0 if x < 0
expect_equal(dPO(-1, 1), 0,
    info = "Expected pPO(-1, 1) to evaluate to 0.")
expect_warning(dPO(-0.5, 1),
    info = "Expected warning when 'x' is not integer.")
expect_equal(dPO(-3:3, 1), dpois(-3:3, 1),
    info = "Results for dPO(-3:3, 1) differ from dpois(-3:3, 1)!")

# - Testing against base R
expect_equal(dPO(0:10, 3), dpois(0:10, 3),
    info = "Expected dPO() to be equal to dpois() from base R.")
expect_equal(dPO(0:10, 17.3), dpois(0:10, 17.3),
    info = "Expected dPO() to be equal to dpois() from base R when using non-integer 'mu'.")
expect_equal(dPO(100:110, 132), dpois(100:110, 132),
    info = "Expected dPO() to be equal to dpois() from base R when using large 'mu'.")
tmp <- c(0.1, 0.5, 13.2)
expect_warning(dPO(tmp, 3), dpois(tmp, 3),
    info = "Expected warnings if 'x' is not integer.")
expect_equal(dPO(tmp, 3), dpois(tmp, 3),
    info = "Expected dPO() to be equal to dpois() when using non-integer values for 'q'.")
rm(tmp)

# - Testing 'log'
expect_equal(dPO(0:5, 3, log = TRUE), log(dPO(0:5, 3, log = FALSE)),
    info = "Expected dPO(..., log = TRUE) to be equal to log(dPO(..., log = FALSE)).")



# -------------------------------------------------------------------
# Testing quantile function (qPO)
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(qPO(),
    info = "Expected error when 'p' is missing.")
expect_error(qPO("foo"),
    info = "Expected error when 'n' is not numeric.")
expect_error(qPO(0.5, mu = list()),
    info = "Expected error when 'mu' is not numeric.")
expect_error(qPO(0.5, mu = list()),
    info = "Expected error when 'mu' is not numeric.")

# Testing range
expect_warning(qPO(+1.0000001), pattern = "NaNs produced",
    info = "Expected warning when p > 1.0")
expect_identical(qPO(+1.0000001), NaN,
    info = "Expected NaN as result when p > 1.0.")
expect_silent(qPO(+1.0000000),
    info = "Function qPO(+1.0000) not silent!")
expect_warning(qPO(-0.0000001), pattern = "NaNs produced",
    info = "Expected warning when p < 0.0")
expect_identical(qPO(-0.0000001), NaN,
    info = "Expected NaN as result when p < 0.0.")
expect_silent(qPO(-0.0000000),
    info = "Function qPO(-0.0000) not silent!")

# Testing sanity checks
expect_error(qPO(0, mu = "foo"),
    info = "Expected error when 'mu' is not numeric.")
expect_error(qPO(0, mu = list()),
    info = "Expected error when 'mu' is not numeric.")
expect_error(qPO(0, mu =  0),
    info = "Expected error when 'mu = 0'.")
expect_error(qPO(0, mu = -1),
    info = "Expected error when 'mu' contains negative values.")
expect_error(qPO(0, mu = c(10, 3, -0.001)),
    info = "Expected error when 'mu' contains negative values.")

# Testing default values
expect_identical(qPO(0), qPO(p = 0, mu = 1, lower.tail = TRUE, log.p = FALSE),
    info = "Return differs from expected result when using default arguments.")
expect_identical(qPO(0), qPO(0, 1, TRUE, FALSE),
    info = "Order of input arguments may have changed; got incorrect return.")

# Testing return values
expect_inherits(qPO(0), "numeric",
    info = "Expected numeric return.")
expect_inherits(qPO(0, mu = numeric()), "numeric",
    info = "Expected numeric return.")
expect_identical(qPO(0, mu = numeric()), NA_real_,
    info = "Expected NA_real_ as return if mu is empty.")

# Testing 'expansion' of q/mu if not of same length.
expect_identical(length(qPO(0.5, seq_len(1))), 1L,
    info = "Expected return of length 1L when one 'mu' is of length 1L, 'p' of length 1L.")
expect_identical(length(qPO(0.5, seq_len(10))), 10L,
    info = "Expected return of length 1L when one 'mu' is of length 10L, 'p' of length 1L.")
expect_identical(length(qPO(c(0.1, 0.5, 0.9), 3)), 3L,
    info = "Expected return of length 3L when one 'p' is of length 3L, 'mu' of length 1L.")

# Testing correct answer(s)
# - 'Manual' tests
expect_equal(qPO(0.5, 1), 1,
    info = "Expected qPO(0.5, 1) to evaluate to 1.")
expect_equal(qPO(0.5, 8.9), round(8.9),
    info = "Expected qPO(0.5, 8.9) to evaluate to round(8.3).")

# - Testing against base R
tmp <- seq(0.1, 0.9, by = 0.1)
expect_equal(qPO(tmp, 3), qpois(tmp, 3),
    info = "Expected qPO() to be equal to qpois() from base R.")
expect_equal(qPO(tmp, 17.3), qpois(tmp, 17.3),
    info = "Expected qPO() to be equal to qpois() from base R when using non-integer 'mu'.")
expect_equal(qPO(tmp, 132), qpois(tmp, 132),
    info = "Expected qPO() to be equal to qpois() from base R when using large 'mu'.")
rm(tmp)

# - Testing 'lower tail' and 'log.p'
tmp <- seq(0.1, 0.9, by = 0.1)
expect_equal(qPO(tmp, 3, lower.tail = FALSE), rev(qPO(tmp, 3, lower.tail = TRUE)),
    info = "Expected qPO(..., lower.tail = FALSE) to be equal to rev(qPO(..., lower.tail = TRUE)).")
expect_true(all(is.nan(qPO(tmp, 3, log.p = TRUE))),
    info = "Expected NaNs as result when log.p = TRUE.")
expect_true(all(is.nan(qPO(tmp, 3, lower.tail = FALSE, log.p = TRUE))),
    info = "Expected NaNs as result when log.p = TRUE.")
rm(tmp)


# -------------------------------------------------------------------
# Testing random function
# -------------------------------------------------------------------

# Testing wrong usage
expect_error(rPO(),
    info = "Expected error when 'n' is missing.")
expect_error(rPO("foo"),
    info = "Expected error when 'n' is not numeric.")
expect_error(rPO(-1),
    info = "Expected error when 'n' is <= 0.")
expect_error(rPO(c(10, 3, -5)),
    info = "Expected error when any 'n' is <= 0.")
expect_error(rPO(5, mu = list()),
    info = "Expected error when 'n' is not numeric.")

# Testing sanity checks
expect_error(rPO(5, mu = "foo"),
    info = "Expected error when 'mu' is not numeric.")
expect_error(rPO(5, mu = list()),
    info = "Expected error when 'mu' is not numeric.")
expect_error(rPO(5, mu =  0),
    info = "Expected error when 'mu = 0'.")
expect_error(rPO(5, mu = -1),
    info = "Expected error when 'mu' contains negative values.")
expect_error(rPO(5, mu = c(10, 3, -0.001)),
    info = "Expected error when 'mu' contains negative values.")

# Testing return
expect_inherits(length(rPO(5)), "integer",
    info = "Expected integer return from rPO.")
expect_identical(length(rPO(5)), 5L,
    info = "Expected return of length 5L for rPO(5).")

# Testing against base R
set.seed(1); a <- rPO(20, mu = 10)
set.seed(1); b <- rpois(20, lambda = 10)
expect_identical(a, b, info = "Behavior of rPO differs from base R.")
set.seed(1); a <- rPO(20, mu = 153.1)
set.seed(1); b <- rpois(20, lambda = 153.1)
expect_identical(a, b, info = "Behavior of rPO differs from base R.")
rm(a, b)
set.seed(Sys.time())


# -------------------------------------------------------------------
# Testing PO family
# -------------------------------------------------------------------

# Misuse
expect_error(PO(foo = "bar"),
    info = "Expected error when using non-defined input arguments.")

# Incorrect specification of link
expect_error(PO("foo"),
    info = "Expected error when using invalid link.")
expect_error(PO(mu.link = "foo"),
    info = "Expected error when using invalid link.")

# Testing for allowed links (shall be silent)
expect_silent(tmp <- PO(mu.link = "inverse"),
    info = "Expected function PO() to be silent when using 'mu.link = \"inverse\".")
expect_identical(tmp$mu.link, "inverse")
rm(tmp)

expect_silent(tmp <- PO(mu.link = "sqrt"),
    info = "Expected function PO() to be silent when using 'mu.link = \"sqrt\".")
expect_identical(tmp$mu.link, "sqrt")
rm(tmp)

expect_silent(tmp <- PO(mu.link = "log"),
    info = "Expected function PO() to be silent when using 'mu.link = \"log\".")
expect_identical(tmp$mu.link, "log")
rm(tmp)

expect_silent(tmp <- PO(mu.link = "identity"),
    info = "Expected function PO() to be silent when using 'mu.link = \"identity\".")
expect_identical(tmp$mu.link, "identity")
rm(tmp)

# Testing default family object
expect_silent(x <- PO(),
    info = "Expected PO() to be silent.")
expect_inherits(x, "gamlss.family",
    info = "Expected PO() to return an object of class 'gamlss.family'.")
expect_identical(x$family, c("PO", "Poisson"),
    info = "Unexpected content in $family.")
expect_identical(x$parameters, list(mu = TRUE),
    info = "Unexpected content in $parameters.")
expect_identical(x$nopar, 1.0,
    info = "Unexpected content in $nopar.")
expect_identical(x$type, "Discrete",
    info = "Unexpected content in $type.")
expect_identical(x$mu.link, "log",
    info = "Unexpected content in $mu.link (expected \"log\", default).")

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
expect_identical(deparse(x$dldm)[[1L]], "function (y, mu) ",
    info = "Unexpected function call for $dldm.")
expect_inherits(x$d2ldm2, "function",
    info = "Content of $d2ldm2 expected to be a function.")
expect_identical(deparse(x$d2ldm2)[[1L]], "function (mu) ",
    info = "Unexpected function call for $d2ldm2.")
expect_inherits(x$G.dev.incr, "function",
    info = "Content of $G.dev.incr expected to be a function.")
expect_identical(deparse(x$G.dev.incr)[[1L]], "function (y, mu, ...) ",
    info = "Unexpected function call for $G.dev.incr.")

expect_inherits(x$rqres, "expression",
    info = "Unexpected content in $rqres.")
expect_inherits(x$mu.initial, "expression",
    info = "Unexpected content in $mu.initial.")

expect_identical(deparse(x$mu.valid)[[1L]], "function (mu) ",
    info = "Unexpected function call for $mu.valid.")
expect_identical(deparse(x$y.valid)[[1L]], "function (y) ",
    info = "Unexpected function call for $y.valid.")
expect_identical(deparse(x$mean)[[1L]], "function (mu) ",
    info = "Unexpected function call for $mean.")
expect_identical(deparse(x$variance)[[1L]], "function (mu) ",
    info = "Unexpected function call for $variance.")

# Testing functions
expect_identical(x$mu.linkfun(3), log(3),
    info = "Unexpected result from mu.linkfun (using mu.link = \"log\").")
expect_identical(x$mu.linkinv(0.3), exp(0.3),
    info = "Unexpected result from mu.linkinv (using mu.link = \"log\").")
expect_equal(x$mu.linkinv(x$mu.linkfun(3)), 3,
    info = "Unexpected result when testing x > linkfun > inverse link fun.")
expect_equal(x$mu.linkinv(x$mu.linkfun(c(10, 3, 17.3))), c(10, 3, 17.3),
    info = "Unexpected result when testing x > linkfun > inverse link fun.")
expect_identical(x$mu.dr(0.3), exp(0.3),
    info = "Unexpected result when calling $mu.dr.")

y <- 5; mu <- 7.2
expect_identical(x$dldm(y, mu), (y - mu) / mu,
    info = "Unexpected result when calling $dldm (first derivative).")
expect_identical(x$d2ldm2(mu),  -1 / mu,
    info = "Unexpected result when calling $d2ldm2 (second derivative).")
expect_identical(x$G.dev.incr(y, mu, 1, 2, 3),  -2 * dPO(x = y, mu = mu, log = TRUE),
    info = "Unexpected result when calling $G.dev.incr.")
rm(y, mu)



expect_true(x$mu.valid(3))
expect_true(x$mu.valid(0.0000001))
expect_false(x$mu.valid(0))
expect_false(x$mu.valid(-0.0001))

expect_true(x$y.valid(3))
expect_true(x$y.valid(0.0000001))
expect_true(x$y.valid(0))
expect_false(x$y.valid(-0.0001))

expect_identical(x$mean(5.2), 5.2)
expect_identical(x$mean(-1), -1)
expect_identical(x$mean((-4):4), (-4):4)
expect_identical(x$variance(5.2), 5.2)
expect_identical(x$variance(-1), -1)
expect_identical(x$variance((-4):4), (-4):4)

