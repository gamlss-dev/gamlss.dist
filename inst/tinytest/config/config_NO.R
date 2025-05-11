# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Setting up configuration
config_NO <- list(
    # If set TRUE it will not be used for auto-testing
    disabled = FALSE,

    # Type of distribution and response support
    type    = "Continuous",
    support = c(-Inf, Inf),

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "family" = expression(mu.link = "identity", sigma.link = "log"),
        "d" = expression(x =, mu = 0, sigma = 1, log = FALSE),
        "p" = expression(q =, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 0, sigma = 1)
    ),

    # Name of the parameters used for testing the constructor function
    params = c("mu", "sigma"),

    # Names of the supported named links
    links = list(
        "mu"    = c("inverse", "log", "identity"),
        "sigma" = c("inverse", "log", "identity")
    ),

    # Valid and invalid response values
    y = list(inside = c(-30, -1, 0, 1, 30), outside = NULL),

    # Parameters:
    # - valid interval
    # - values inside and outside of interval
    # - whether boundaries are valid for d/p/q/r functions and/or family (none, both, left, right)
    mu = list(
      interval = c(-Inf, Inf),
      inside   = c(-30, -1, 0, 1, 30),
      outside  = NULL,
      dpqr     = "both",
      family   = "none"
    ),

    sigma = list(
      interval = c(0, Inf),
      inside   = c(eps, 1, 30),
      outside  = -eps,
      dpqr     = "none",
      family   = "none"
    )
)

