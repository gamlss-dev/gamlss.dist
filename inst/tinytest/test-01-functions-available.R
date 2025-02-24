# -------------------------------------------------------------------
# Testing limits of continuous distributions.
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) { library("tinytest"); library("gamlss.dist") }

# Helper functions
source("../distributions-testconfig.R")

# Defines the family to be tested
family <- "NO"

# Get test config; could also be added here directly
configs <- get_testconfig(NULL)

eps <- sqrt(.Machine$double.eps)

for (family in names(configs)) {
    config <- configs[[family]]

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

## Used to format numeric values for expressions/commands
#fmt <- function(x) format(x, digits = 10)
#
## Create grid of possible link combinations
#links <- expand.grid(lapply(conf$param, function(x) x$links),
#                      stringsAsFactors = FALSE)
#
## Create grid valid values for testing (for density, dist)
#testvals <- expand.grid(conf$values)
#
## Create grid valid values for testing (for density, dist)
#testillegal <- expand.grid(conf$illegal)
#
## Name of the parameters
#params <- names(conf$param)
#
## Test if silent with combinations of links
#for (i in seq_len(nrow(links))) {
#    # Command for constructor function
#    cmd <- sprintf("tmp <- %s(%s)", family,
#        paste(sprintf("%s.link = \"%s\"", names(links[i, ]), links[i, ]), collapse = ", "))
#    # Call constructure, returns object on 'tmp'
#    expect_silent(eval(parse(text = cmd)))
#
#    # Check if family was set correctly
#    expect_identical(tmp$family[[1]], family,
#                     info = "$family[[1]] does not contain expected string.")
#    for (j in seq_len(ncol(links))) {
#        expect_identical(tmp[[paste0(names(links)[j], ".link")]],
#                            links[i, j],
#                     info = sprintf("$%s.link not as expected.", family))
#    }
#}
#
## Testing if the family object is defined properly
#expect_inherits(tmp, "gamlss.family",
#          info = "Expected object to inherit from class 'gamlss.family'.")
#expect_inherits(tmp, "family",
#          info = "Expected object to inherit from class 'family'.")
#
## Checking parameters
#expect_identical(tmp$parameters,
#            as.list(setNames(rep(TRUE, length(params)), params)),
#            info = "$parameters does not include all expected parameters.")
#expect_equal(tmp$nopar, length(params),
#            info = "$nopar contains incorrect value.")
#expect_identical(tmp$type, conf$type,
#            info = sprintf("$type incorrect, expected '%s'.", conf$type))
#
## Testing tha
#for (p in params) {
#   expect_inherits(tmp[[paste0(p, ".linkfun")]], "function",
#           info = sprintf("$%s.valid expected to be a function.", p))
#   expect_inherits(tmp[[paste0(p, ".linkinv")]], "function",
#           info = sprintf("$%s.valid expected to be a function.", p))
#   expect_inherits(tmp[[paste0(p, ".dr")]], "function",
#           info = sprintf("$%s.valid expected to be a function.", p))
#   expect_inherits(tmp[[paste0(p, ".initial")]], "expression",
#           info = sprintf("$%s.valid expected to be an expression.", p))
#}
#
#for (p in c("y", params)) {
#    fun <- tmp[[paste0(p, ".valid")]]
#    expect_inherits(fun, "function",
#            info = sprintf("$%s.valid expected to be a function.", p))
#    for (v in conf$values[[p]]) {
#        expect_true(tmp[[paste0(p, ".valid")]](v),
#                info = sprintf("Expected $%s.valid(%.5f) to evaluate to TRUE.", p, v))
#    }
#    lower <- conf$params[[p]]$range[1L]
#    if (isTRUE(is.finite(lower))) {
#        expect_false(fun(lower - 2 * eps),
#                info = sprintf("Expected $%s.valid(%.5f) to evaluate to FALSE.", p, v))
#    }
#    upper <- conf$params[[p]]$range[2L]
#    if (isTRUE(is.finite(upper))) {
#        expect_false(fun(upper + 2 * eps),
#                info = sprintf("Expected $%s.valid(%.5f) to evaluate to FALSE.", p, v))
#    }
#    rm(fun)
#}
