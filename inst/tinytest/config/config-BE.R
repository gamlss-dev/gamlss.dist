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
    type    = "Continuous",
    support = c(0, 1),

    # Name of the parameters
    params = c("mu", "sigma"),

    # Valid and invalid response values
    y     = list(valid = val_valid, invalid = val_invalid),

    mu    = list("logit"    = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "probit"   = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "cloglog"  = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "cauchit"  = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "log"      = list(range = c(0, 1), valid = val_valid, invalid = val_invalid)),

    sigma = list("logit"    = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "probit"   = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "cloglog"  = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "cauchit"  = list(range = c(0, 1), valid = val_valid, invalid = val_invalid),
                 "log"      = list(range = c(0, 1), valid = val_valid, invalid = val_invalid))
)
