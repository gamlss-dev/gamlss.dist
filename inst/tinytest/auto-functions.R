# -------------------------------------------------------------------
# Automatic functions for testing the different families. Note: Relies on
# `functions.R` and MUST use the `my_expect_*()` with
# `env = parent.frame()` to work properly.
# -------------------------------------------------------------------


test_constructor_and_dpqr_exist <- function(family) {
    stopifnot(is.character(family), length(family) == 1L)
    env <- parent.frame()

    check_and_load <- paste0(c("", "d", "p", "q", "r"), family)
    for (f in check_and_load) {
        # Attach 'f' to the environment in which we will evaluate the expression
        env$f <- f

        my_expect_silent(get(f, envir = getNamespace("gamlss.dist")),
                      info = sprintf("Could not load function '%s' from gamlss.dist", f),
                      env = env)

        my_expect_inherits(get(f, envir = getNamespace("gamlss.dist")), "function",
                      info = sprintf("'%s' is not a function", f),
                      env = env)
    }
}

test_constructor_function <- function(family, conf) {
    stopifnot(is.character(family), length(family) == 1L)
    stopifnot(is.list(conf))
    env <- parent.frame()

    # Create grid of possible link combinations
    links <- expand.grid(lapply(conf$param, function(x) x$links),
                          stringsAsFactors = FALSE)

    # Test if constructor function runs silent
    my_expect_silent(parse(text = paste0(family, "()")), env = env)

    # Test if silent with combinations of links
    for (i in seq_len(nrow(links))) {
        cmd <- sprintf("%s(%s)", family,
            paste(sprintf("%s.link = \"%s\"", names(links[i, ]), links[i, ]), collapse = ", "))
        env$e <- parse(text = cmd)
        my_expect_silent(e, env = env)
        env$tmp <- eval(env$e)

        # Check if family was set correctly
        my_expect_identical(tmp$family[[1]], family, env = env)
        for (j in seq_len(ncol(links))) {
            env$j <- j
            env$i <- i
            env$links <- links
            my_expect_identical(tmp[[paste0(names(links)[j], ".link")]],
                                links[i, j], env = env)
        }
    }

}













