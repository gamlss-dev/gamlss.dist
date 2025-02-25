# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist") }

# Helper functions
source("config/get_testconfig.R")

# Get test config; could also be added here directly
configs <- get_testconfig(NULL, verbose = FALSE)

# Looping over all defined families
for (family in names(configs)) {
    # Testing if the constructor function as well as the dpqr functions
    # exist in gamlss.dist and that they are functions.
    check_and_load <- paste0(c("", "d", "p", "q", "r"), family)
    for (f in check_and_load) {
        # Attach 'f' to the environment in which we will evaluate the expression
        expect_silent(get(f, envir = getNamespace("gamlss.dist")),
            info = sprintf("Cannot find function 'famlss.dist::%s'.", f))
        expect_inherits(get(f, envir = getNamespace("gamlss.dist")), "function",
            info = sprintf("'gamlss.dist::%s' is not a function.", f))

    }
}

