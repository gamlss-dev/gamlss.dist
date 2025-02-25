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

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "constructor" = expression(mu.link = "identity", sigma.link = "log", nu.link = "log", tau.link = "log"),
        "d" = expression(x =, mu = 0, sigma = 1, nu = 3, tau = 1.5, log = FALSE),
        "p" = expression(q =, mu = 0, sigma = 1, nu = 3, tau = 1.5, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 0, sigma = 1, nu = 3, tau = 1.5, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 0, sigma = 1, nu = 3, tau = 1.5)
    ),

    # Name of the parameters
    params = c("mu", "sigma", "nu", "tau"),

    # Valid and invalid response values
    y     = list(valid = c(-30, -1, 0, 1, 30), invalid = NULL),

    mu    = list("identity" = list(range = c(-Inf, Inf), valid = c(-30, -1, 0, 1, 30), invalid = NULL),
                 "inverse"  = list(range = c(-Inf, Inf), valid = c(-30, -1, 0, 1, 30), invalid = NULL),
                 "log"      = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0))),

    sigma = list("identity" = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "inverse"  = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "log"      = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0))),

    nu    = list("identity" = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "inverse"  = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "log"      = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0))),

    tau   = list("identity" = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "inverse"  = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)),
                 "log"      = list(range = c(eps, Inf),  valid = c(eps, 1, 30), invalid = c(-eps, 0)))

)

