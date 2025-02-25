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

    # Specify arguments for constructor function; correct order and correct default values
    arguments = list(mu.link = "log"),

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

