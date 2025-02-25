# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Setting up configuration
res <- list(
    # If set TRUE it will not be used for auto-testing
    disabled = FALSE,

    # Type of distribution and response support
    type    = "Continuous",
    support = c(-Inf, Inf),

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "constructor" = expression(mu.link = "identity", sigma.link = "log"),
        "d" = expression(x =, mu = 0, sigma = 1, log = FALSE),
        "p" = expression(q =, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 0, sigma = 1)
    ),

    # Name of the parameters used for testing the constructor functio
    params = c("mu", "sigma"),

    # Valid and invalid response values
    y     = list(valid = c(-30, -1, 0, 1, 30), invalid = NULL),

    mu    = list("identity" = list(valid = c(-30, -1, 0, 1, 30), invalid = NULL),
                 "inverse"  = list(valid = c(-30, -1, 0, 1, 30), invalid = NULL),
                 "log"      = list(valid = c(eps, 1, 30), invalid = c(-eps, 0))),

    sigma = list("identity" = list(valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "inverse"  = list(valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "log"      = list(valid = c(eps, 1, 30), invalid = c(-eps, 0))),

    # Parameters used for testing the dprq methods; they are not aware of the
    # link function and have different valid/invalid ranges.

    # TODO(R): dNO accepts sigma = 0, dNO2 fails.
    dpqr  = list("mu"    = list(valid = c(-30, -5, -1, 0, 1, 5, 30), invalid = NULL),
                 "sigma" = list(valid = c(eps, 1, 30), invalid = c(-eps, 0)))

)

