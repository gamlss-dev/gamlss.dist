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
    my_expect_silent(f, expr)
    eval(expr)

    my_expect_identical(f, x$family[[1]], f)
    my_expect_identical(f, x$mu.link, link)

    my_expect_equal(f, x$mu.linkinv(x$mu.linkfun(3)), 3)
    my_expect_equal(f, x$mu.linkinv(x$mu.linkfun(c(10, 3, 17.3))), c(10, 3, 17.3))
    rm(x)
}

