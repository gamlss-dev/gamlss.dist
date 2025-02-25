# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Setting up configuration
res <- list(
    type    = "Continuous",
    support = c(0, 1),
    # Distribution parameters with support (range) and all available links
    params  = list("mu"    = list(range = c(0, 1),
                                  links = c("logit", "probit", "cloglog", "cauchit", "log", "own")),
                   "sigma" = list(range = c(0, 1),
                                  links = c("logit", "probit", "cloglog", "cauchit", "log", "own"))
                   ),
    # Valid values used for testing dpqr
    values  = list("y"     = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps),
                   "mu"    = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps),
                   "sigma" = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps)),
    # Illegal values, any combination of these must fail
    illegal = list("y"     = c(-10, -0.00001, 0, 1, 1.00001, 10),
                   "mu"    = c(-10, -0.00001, 0, 1, 1.00001, 10),
                   "sigma" = c(-10, -0.00001, 0, 1, 1.00001, 10))
)
