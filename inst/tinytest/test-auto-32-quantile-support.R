# -------------------------------------------------------------------
# Testing distribution functions; check behavior when the density is
# calculated outside the support of the distribution.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist"); family <- "NO" }

# Used for printing numerics
fmt <- function(x) format(x, digits = 10)

# Helper functions
source("helperfunctions.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL, verbose = FALSE)

# Epsilon for testing
eps <- sqrt(.Machine$double.eps)

# Looping over all defined families
for (family in names(configs)) {
    # For convenience
    conf <- configs[[family]]

    # Getting p<FAM> function (qfun)
    qfun <- get(sprintf("q%s", family), envir = getNamespace("gamlss.dist"))

    # ---------------------------------------------------------------
    # Testing that we get NA_real_ if p is outside the probability
    # range (i.e., p < 0 | p > 1).
    # ---------------------------------------------------------------
    dead_end <- match.arg(conf$type, c("Continuous", "Discrete")) # Ensure we have this captured
    grd_valid <- get_testgrid_valid(conf, TRUE, "p", main_values = c(0 - eps, 1 + eps))

    tmpfun <- qfun
    for (i in seq_len(nrow(grd_valid))) {
        formals(tmpfun)[names(grd_valid)] <- grd_valid[i, ]
        expect_equal(tmpfun(), NA_real_,
            info = sprintf("Expected 'q%s(%s)' (p < 0 or p > 1) to return NA_real_.", family,
                           paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))
    }

    # ---------------------------------------------------------------
    # When using in valid range, the quantile function must return
    # a value within the support range of the response.
    # ---------------------------------------------------------------
    grd   <- expand.grid(setNames(lapply(conf$params, function(p) conf[[c(p, "inside")]]), conf$params))
    p     <- c(0.0001, 0.25, 0.5, 0.75, 0.9999)
    dinfo <- sprintf("p = %s", deparse(p))

    # Lower or upper support infinite?
    inf <- list(lower = is.infinite(conf$support[1L]), upper = is.infinite(conf$support[2L]))

    tmpfun <- qfun
    for (i in seq_len(nrow(grd))) {
        formals(tmpfun)[names(grd)] <- grd[i, ]
        tmp <- tmpfun(p = p)

        expect_identical(length(tmp), length(p),
            info = sprintf("Expected 'p%s(%s, %s)' to return numeric of length %d.", family, dinfo,
                           paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", "), length(p)))

        if (inf$lower && inf$upper) {
            expect_true(all(tmp > -Inf & tmp < Inf),
                info = sprintf("Expected 'p%s(%s, %s)' to be in (-Inf, Inf).", family, dinfo,
                               paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", ")))
        } else if (inf$lower) {
            expect_true(all(tmp > -Inf & tmp <= conf$support[2L]),
                info = sprintf("Expected 'p%s(%s, %s)' to be in (-Inf, %s].", family, dinfo,
                               paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", "),
                               fmt(conf$support[2L])))
        } else if (inf$upper) {
            expect_true(all(tmp >= conf$support[1L] & tmp < Inf),
                info = sprintf("Expected 'p%s(%s, %s)' to be in [%s, Inf).", family, dinfo,
                               paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", "),
                               fmt(conf$support[2L])))
        } else {
            expect_true(all(tmp >= conf$support[1L] & tmp <= conf$support[2L]),
                info = sprintf("Expected 'p%s(%s, %s)' to be in [%s, %s).", family, dinfo,
                               paste(sprintf("%s = %s", names(grd_valid), fmt(grd_valid[i, ])), collapse = ", "),
                               fmt(conf$support[1L]), fmt(conf$support[2L])))
        }

    }

}
