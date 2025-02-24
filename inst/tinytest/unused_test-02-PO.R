# -------------------------------------------------------------------
# Testing the Poisson (PO) family: Automated tests after we have
# tested the family intensively in test-01-PO.R
# -------------------------------------------------------------------

# Used for development/testing manually
if (interactive()) {
    library("tinytest")
    library("gamlss.dist")
}

source("functions.R")

for (link in c("log", "identity", "inverse", "sqrt")) {
    # Testing mu.link = "identity"
    f <- "PO"
    expr <- parse(text = sprintf("x <- PO(mu.link = \"%s\")", link))
    my_expect_silent(expr, family = f)
    eval(expr)

    my_expect_identical(x$family[[1]], f, family = f)
    my_expect_identical(x$mu.link, link, family = f)

    my_expect_equal(x$mu.linkinv(x$mu.linkfun(3)), 3, family = f)
    my_expect_equal(x$mu.linkinv(x$mu.linkfun(c(10, 3, 17.3))), c(10, 3, 17.3), family = f)
    rm(x)
}

