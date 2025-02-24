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
    eps <- env$eps <- sqrt(.Machine$double.eps)

    # Used to format numeric values for expressions/commands
    fmt <- function(x) format(x, digits = 10)

    # Create grid of possible link combinations
    links <- expand.grid(lapply(conf$param, function(x) x$links),
                          stringsAsFactors = FALSE)
    env$links <- links

    # Create grid valid values for testing (for density, dist)
    testvals <- expand.grid(conf$values)
    env$testvals <- testvals

    # Create grid valid values for testing (for density, dist)
    testillegal <- expand.grid(conf$illegal)
    env$testillegal <- testillegal

    # Create grid valid values for testing (for quantiles)
    qtestvals <- expand.grid(c(list(p = c(0, 0.01, 0.5, 0.99, 1)),
                              conf$values[!names(conf$values) == "y"]))
    env$qtestvals <- qtestvals

    # Create grid valid values for testing (for random values)
    rtestvals <- expand.grid(conf$values[!names(conf$values) == "y"])
    env$rtestvals <- rtestvals

    # Name of the parameters
    params <- names(conf$param)
    env$params <- params

    # Test if constructor function runs silent
    my_expect_silent(parse(text = paste0(family, "()")), env = env)

    # Test if silent with combinations of links
    for (i in seq_len(nrow(links))) {
        cmd <- env$cmd <- sprintf("%s(%s)", family,
            paste(sprintf("%s.link = \"%s\"", names(links[i, ]), links[i, ]), collapse = ", "))
        my_expect_silent(parse(text = cmd), env = env)
        env$tmp <- eval(parse(text = cmd))

        # Check if family was set correctly
        my_expect_identical(tmp$family[[1]], family, env = env)
        for (j in seq_len(ncol(links))) {
            env$j <- j
            env$i <- i
            my_expect_identical(tmp[[paste0(names(links)[j], ".link")]],
                                links[i, j], env = env)
        }

        # Testing if the family object is defined properly
        my_expect_inherits(tmp, "gamlss.family",
                    info = "Expected object to inherit from class 'gamlss.family'.",
                    family = family, env = env)
        my_expect_inherits(tmp, "family",
                    info = "Expected object to inherit from class 'family'.",
                    family = family, env = env)

        my_expect_identical(tmp$parameters,
                    as.list(setNames(rep(TRUE, length(params)), params)),
                    info = paste("$parameters does not include all expected parameters.",
                                 "Expected", paste(params, collapse = ", ")),
                    family = family, env = env)

        my_expect_identical(tmp$nopar, as.numeric(length(params)),
                    info = "$nopar contains incorrect value.",
                    family = family, env = env)
        my_expect_identical(tmp$type, conf$type,
                    info = sprintf("$type incorrect, expected '%s'.", conf$type),
                    family = family, env = env)

        for (p in params) {
            env$p <- p
            my_expect_inherits(tmp[[paste0(p, ".linkfun")]], "function",
                    info = sprintf("$%s.linkfun expected to be a function.", p),
                    family = family, env = env)
            my_expect_inherits(tmp[[paste0(p, ".linkinv")]], "function",
                    info = sprintf("$%s.linkinv expected to be a function.", p),
                    family = family, env = env)
            my_expect_inherits(tmp[[paste0(p, ".dr")]], "function",
                    info = sprintf("$%s.dr expected to be a function.", p),
                    family = family, env = env)
            my_expect_inherits(tmp[[paste0(p, ".initial")]], "expression",
                    info = sprintf("$%s.initial expected to be an expression.", p),
                    family = family, env = env)
        }

        for (p in c("y", params)) {
            env$p <- p
            my_expect_inherits(tmp[[paste0(p, ".valid")]], "function",
                    info = sprintf("$%s.valid expected to be a function.", p),
                    family = family, env = env)
            for (v in conf$values[[p]]) {
                env$v <- v
                my_expect_true(tmp[[paste0(p, ".valid")]](v),
                        info = sprintf("Expected $%s.valid(%.5f) to evaluate to TRUE.", p, v),
                        family = family, env = env)
            }
            env$lower <- conf$params[[p]]$range[1L]
            if (isTRUE(is.finite(env$lower))) {
                my_expect_false(tmp[[paste0(p, ".valid")]](lower - 2 * eps),
                        info = sprintf("Expected $%s.valid(%.5f) to evaluate to FALSE.", p, v),
                        family = family, env = env)
            }
            env$upper <- conf$params[[p]]$range[2L]
            if (isTRUE(is.finite(env$upper))) {
                my_expect_false(tmp[[paste0(p, ".valid")]](upper + 2 * eps),
                        info = sprintf("Expected $%s.valid(%.5f) to evaluate to FALSE.", p, v),
                        family = family, env = env)
            }
        }


    }

    ## I still need to write it into env (i.e., parent.frame())
    ## as else the subtitution/evaluation does not work
    ## RETO RETO RETO RETO RETO
    #cmd <- sprintf("warning(' test warn %s')", family)
    #my_expect_warning(cmd,
    #                  info = sprintf("EXP WARNING WITH %s", cmd),
    #                     family = family)
    #cmd2 <- sprintf("warning(' test warn %s')", family)
    #my_expect_warning(cmd2,
    #                  info = sprintf("EXP WARNING WITH %s", cmd),
    #                     family = family)


    # ---------------------------------------------------------------
    # Testing density
    # ---------------------------------------------------------------
    tmp <- ifelse(names(testvals) == "y", "x", names(testvals))
    for (k in seq_len(nrow(testvals))) {
        cmd <- env$cmd <- sprintf("d%s(%s)", family,
            paste(sprintf("%s = %s", tmp, fmt(testvals[k, ])), collapse = ", "))
        my_expect_silent(parse(text = cmd), info = sprintf("Expected '%s' to run silent.", cmd),
                         family = family, env = env)
        env$ev <- eval(parse(text = cmd))
        my_expect_true(is.numeric(ev) && is.finite(ev) && length(ev) == 1L && ev >= 0,
              info = sprintf("Expected '%s' to return one finite numeric value >= 0.", cmd),
              family = family, env = env)
    }

    tmp <- ifelse(names(testillegal) == "y", "x", names(testillegal))
    for (k in seq_len(nrow(testillegal))) {
        cmd <- env$cmd <- sprintf("d%s(%s)", family,
            paste(sprintf("%s = %s", tmp, fmt(testillegal[k, ])), collapse = ", "))
        my_expect_warning(parse(text = cmd), info = sprintf("Expected '%s' to throw a warning", cmd),
                         family = family, env = env)
        #my_expect_error(parse(text = cmd), info = sprintf("Expected '%s' to throw an error.", cmd),
        #                 family = family, env = env)
    }


    # ---------------------------------------------------------------
    # Testing distribution
    # ---------------------------------------------------------------
    tmp <- ifelse(names(testvals) == "y", "q", names(testvals))
    for (k in seq_len(nrow(testvals))) {
        cmd <- env$cmd <- sprintf("p%s(%s)", family,
            paste(sprintf("%s = %s", tmp, fmt(testvals[k, ])), collapse = ", "))
        my_expect_silent(parse(text = cmd), info = sprintf("Expected '%s' to run silent.", cmd),
                         family = family, env = env)
        env$ev <- eval(parse(text = cmd))
        my_expect_true(is.numeric(ev) && is.finite(ev) && length(ev) == 1L && ev >= 0 && ev <= 1,
              info = sprintf("Expected '%s' to return one finite numeric value in [0., 1.].", cmd),
              family = family, env = env)
    }

    #tmp <- ifelse(names(testillegal) == "y", "q", names(testillegal))
    #for (k in seq_len(nrow(testillegal))) {
    #    cmd <- env$cmd <- sprintf("p%s(%s)", family,
    #        paste(sprintf("%s = %s", tmp, fmt(testillegal[k, ])), collapse = ", "))
    #    my_expect_error(parse(text = cmd), info = sprintf("Expected '%s' to throw an error.", cmd),
    #                     family = family, env = env)
    #}


    # ---------------------------------------------------------------
    # Testing quantile function
    # ---------------------------------------------------------------
    lower <- env$lower <- conf$support[1L]
    upper <- env$upper <- conf$support[2L]
    for (k in seq_len(nrow(qtestvals))) {
        cmd <- env$cmd <- sprintf("q%s(%s)", family,
            paste(sprintf("%s = %s", names(qtestvals), fmt(qtestvals[k, ])), collapse = ", "))
        my_expect_silent(parse(text = cmd), info = sprintf("Expected '%s' to run silent.", cmd),
                         family = family, env = env)
        env$ev <- eval(parse(text = cmd))
        my_expect_true(is.numeric(ev) && length(ev) == 1L && !is.na(ev),
              info = sprintf("Expected '%s' to return one numeric value (not NA).", cmd),
              family = family, env = env)

        # Check if inside support if support is not infinite on one/both ends
        if (isTRUE(is.finite(lower))) {
            my_expect_true(ev >= lower,
                  info = sprintf("Expected '%s' to be >= %.5f (lower support).", cmd, lower),
                  family = family, env = env)
        }
        if (isTRUE(is.finite(upper))) {
            my_expect_true(ev <= upper,
                  info = sprintf("Expected '%s' to be <= %.5f (upper support).", cmd, upper),
                  family = family, env = env)
        }

    }

    # ---------------------------------------------------------------
    # Testing random function
    # ---------------------------------------------------------------
    lower <- env$lower <- conf$support[1L]
    upper <- env$upper <- conf$support[2L]
    for (k in seq_len(nrow(rtestvals))) {
        # nr: Number of random values to be drawn
        for (nr in c(1L, 5L, 20L)) {
            env$nr <- nr

            cmd <- env$cmd <- sprintf("r%s(n = %d, %s)", family, nr,
                paste(sprintf("%s = %s", names(rtestvals), fmt(rtestvals[k, ])), collapse = ", "))
            my_expect_silent(parse(text = cmd), info = sprintf("Expected '%s' to run silent.", cmd),
                             family = family, env = env)
            env$ev <- eval(parse(text = cmd))
            my_expect_true(is.numeric(ev) && length(ev) == nr && all(!is.na(ev)),
                  info = sprintf("Expected '%s' to return %d non-missing numeric values.", cmd, nr),
                  family = family, env = env)

            # Check if inside support if support is not infinite on one/both ends
            if (isTRUE(is.finite(lower))) {
                my_expect_true(all(ev) >= lower,
                      info = sprintf("Expected all values returned by '%s' to be >= %.5f (lower support).", cmd, lower),
                      family = family, env = env)
            }
            if (isTRUE(is.finite(upper))) {
                my_expect_true(all(ev) <= upper,
                      info = sprintf("Expected all values returned by '%s' to be >= %.5f (upper support).", cmd, upper),
                      family = family, env = env)
            }
        }

    }

}



