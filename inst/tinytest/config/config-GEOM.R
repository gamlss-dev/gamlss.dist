# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
#
# TODO(R): GEOM() 'mstats' contains 'log' as link definition twice.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Same set of valid/invalid values for all parameters
val_valid   <- c(eps, 0.1, 0.5, 0.9, 1 - eps)
val_invalid <- c(-1, -eps, 0)

# Setting up configuration
res <- list(
    # If set TRUE it will not be used for auto-testing
    disabled = FALSE,

    # Type of distribution and response support
    type    = "Discrete",
    support = c(0, Inf),

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "constructor" = expression(mu.link = "log"),
        "d" = expression(x =, mu = 2, log = FALSE),
        "p" = expression(q =, mu = 2, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 2, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 2)
    ),

    # Name of the parameters
    params = "mu",

    # Valid and invalid response values used for testing the constructor function
    y     = list(valid = c(0, 1, 2, 30), invalid = c(-eps, -1)),

    # TODO(R): These settings are perfectly fine for testing
    #          mu.linkfun/mu.linkinv and mu.valid, however,
    #          they do not throw errors when used with dGEOM as
    #          dGEOM (who does not know the link) only checks for mu > 0.
    mu    = list("log"      = list(valid = c(eps, 1, 5, 30), invalid = c(-1, -eps, 0)),
                 "probit"   = list(valid = val_valid, invalid = val_invalid),
                 "cloglog"  = list(valid = val_valid, invalid = val_invalid),
                 "cauchit"  = list(valid = val_valid, invalid = val_invalid)),

    # Parameters used for testing the dprq methods; they are not aware of the
    # link function and have different valid/invalid ranges.
    dpqr  = list("mu"    = list(valid = c(0, 1, 30), invalid = c(-1, -eps)),
                 "sigma" = list(valid = c(0, 1, 30), invalid = c(-1, -eps)))
)

