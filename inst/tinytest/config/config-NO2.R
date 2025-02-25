# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Setting up configuration
res <- list(
    type    = "Continuous",
    support = c(-Inf, Inf),

    # Distribution parameters with support (range) and all available links
    params  = list("mu"    = list(range = c(-Inf, Inf),
                                  links = c("inverse", "log", "identity", "own")),
                   "sigma" = list(range = c(0, Inf),
                                  links = c("inverse", "log", "identity", "own"))
                   ),
    # Valid values used for testing dpqr
    values  = list("y"     = c(-30, -5 -1, 0, 1, 5, 30),
                   "mu"    = c(-30, -5 -1, 0, 1, 5, 30),
                   "sigma" = c(eps, 1, 5, 30)),
    # Illegal values, any combination of these must fail
    illegal = list("y"     = 0,
                   "mu"    = 0,
                   "sigma" = c(0, -0.0001, -100))
)

