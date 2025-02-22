# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) {
    library("tinytest")
    library("gamlss.dist")
}

# Helper functions
source("functions.R")
source("auto-functions.R")

# Limits to be tested
eps <- sqrt(.Machine$double.eps)

# TODO(R): A few hand selected distributions which do not fail these basic tests.
families <- c(
    "NO", "LO", "BE"
)

# -------------------------------------------------------------------
# Check if functions exist.
# -------------------------------------------------------------------
for (family in families) {
    # Test if constructor function as well as the dprq functions
    # exist and are functions.
    test_constructor_and_dpqr_exist(family)

    ## Get test config
    conf <- get_test_config(family)

    ## Test family constructor (basic tests) with different
    ## combinations of links (if there are any).
    test_constructor_function(family, conf)
}
