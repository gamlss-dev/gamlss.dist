# -------------------------------------------------------------------
# Configuration for testing the family object.
# Used by the tinytest scripts for automated testing.
# -------------------------------------------------------------------

# Used to add some delta to test for 'out of support' values
eps <- sqrt(.Machine$double.eps)

# Same set of valid/invalid values for all parameters
val_valid   <- c(0 + eps, 0.1, 0.5, 0.9, 1 - eps)
val_invalid <- c(-10, -0.00001, 0, 1, 1.00001, 10)

# Setting up configuration
res <- list(
    # If set TRUE it will not be used for auto-testing
    disabled = FALSE,

    # Type of distribution and response support
    type    = "Continuous",
    support = c(0, 1),

    # Default arguments (correct order, correct defaults) for
    # the constructor functions as well as dpqr.
    arguments = list(
        "constructor" = expression(mu.link = "logit", sigma.link = "logit"),
        "d" = expression(x =, mu = 0.5, sigma = 0.2, log = FALSE),
        "p" = expression(q =, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE),
        "q" = expression(p =, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE),
        "r" = expression(n =, mu = 0.5, sigma = 0.2)
    ),

    # Name of the parameters
    params = c("mu", "sigma"),

    # Valid and invalid response values used for testing the constructor function
    y     = list(valid = val_valid, invalid = val_invalid),

    mu    = list("logit"    = list(valid = val_valid, invalid = val_invalid),
                 "probit"   = list(valid = val_valid, invalid = val_invalid),
                 "cloglog"  = list(valid = val_valid, invalid = val_invalid),
                 "cauchit"  = list(valid = val_valid, invalid = val_invalid),
                 "log"      = list(valid = val_valid, invalid = val_invalid)),

    sigma = list("logit"    = list(valid = val_valid, invalid = val_invalid),
                 "probit"   = list(valid = val_valid, invalid = val_invalid),
                 "cloglog"  = list(valid = val_valid, invalid = val_invalid),
                 "cauchit"  = list(valid = val_valid, invalid = val_invalid),
                 "log"      = list(valid = val_valid, invalid = val_invalid)),

    # Parameters used for testing the dprq methods; they are not aware of the
    # link function and have different valid/invalid ranges.
    dpqr  = list("mu"    = list(valid = c(eps, 0.1, 0.5, 0.9, 1 - eps), invalid = c(-eps, 0, 1, 1 + eps)),
                 "sigma" = list(valid = c(eps, 0.1, 0.5, 0.9, 1 - eps), invalid = c(-eps, 0, 1, 1 + eps)))

)
