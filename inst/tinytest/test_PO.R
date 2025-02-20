# -------------------------------------------------------------------
# Testing the PO distribution.
#
# Notes by Reto:
# - ppois(0, 0) Returns, pPO(0, 0) throws an error.
# - pdf(p, numeric()) returns NA_real_, ppois(0, numeric()) returns empty vector.
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

# Testing sanity checks
expect_error(pPO(0, mu = "foo"),
    info = "Expected error when 'q' is not numeric.")
expect_error(pPO(0, mu = list()),
    info = "Expected error when 'q' is not numeric.")
expect_error(pPO(0, mu =  0),
    info = "Expected error when 'mu = 0'.")
expect_error(pPO(0, mu = -1),
    info = "Expected error when 'mu' contains negative values.")
expect_error(pPO(0, mu = c(10, 3, -0.001)),
    info = "Expected error when 'mu' contains negative values.")

# Testing default values
expect_identical(pPO(0), pPO(q = 0, mu = 1, lower.tail = TRUE, log.p = FALSE),
    info = "Return differs from expected return when using default arguments.")
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


