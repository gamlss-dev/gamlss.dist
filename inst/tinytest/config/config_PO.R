# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Setting up configuration
config_PO <- list(
    # If set TRUE it will not be used for auto-testing
    disabled = TRUE,

    # Type of distribution and response support
    type    = "Discrete",
    support = c(0, Inf),

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "family" = expression(mu.link = "log"),
        "d" = expression(x =, mu = 1, log = FALSE),
        "p" = expression(q =, mu = 1, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 1, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 1)
    ),

    # Name of the parameters used for testing the constructor function
    params = "mu",

    # Names of the supported named links
    links = list(
        "mu" = c("identity", "inverse", "log", "sqrt")
    ),

    # Response values inside and outside of the support
    y = list(inside = c(0, 1, 30), outside = c(-1, -eps, 1.5)),

    # Parameters:
    # - valid interval
    # - values inside and outside of interval
    # - whether boundaries are valid for d/p/q/r functions and/or family (none, both, left, right)
    mu = list(
      interval = c(0, Inf),
      inside   = c(eps, 1, 30),
      outside  = c(-1, -eps),
      dpqr     = "both",
      family   = "none"
    )
)

