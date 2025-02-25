# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Setting up configuration
res <- list(
    type    = "Discrete",
    support = c(-Inf, Inf),

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "constructor" = expression(mu.link = "log"),
        "d" = expression(x =, mu = 1, log = FALSE),
        "p" = expression(q =, mu = 1, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 1, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 1)
    ),

    # Name of the parameters
    params = "mu",

    # Valid and invalid response values
    y     = list(valid = c(0, 1, 30), invalid = c(-1, -eps)),

    # TODO(R): PO currently does not allow for '0' (base R does)
    mu    = list("identity" = list(range = c(eps, Inf), valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "inverse"  = list(range = c(eps, Inf), valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "log"      = list(range = c(eps, Inf), valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "sqrt"     = list(range = c(eps, Inf), valid = c(eps, 1, 30), invalid = c(-eps, 0)))
)

