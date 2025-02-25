


# -------------------------------------------------------------------
# Helper function for loading one or multiple test configs
# -------------------------------------------------------------------
get_testconfig <- function(family = NULL, dir = "config", verbose = FALSE) {
    stopifnot(
        "'family' must be NULL or valid caracter of length 1" =
            is.null(family) || (is.character(family) && length(family) == 1 && nchar(family) > 0L)
    )


    # First let us check what config files we have available.
    files <- list.files(dir, full.names = TRUE)
    files <- files[grepl("^config-.*\\.R$", basename(files))]
    stopifnot("No config files found on disc" = length(files) > 0L)
    fams  <- regmatches(files, regexpr("(?<=(config-)).*(?=(\\.R))", files, perl = TRUE))

    # If 'family' is chracter, check if it is one of the allowed
    # families, i.e., families for which we have a config file.
    # Replace 'fams' with the requested family; used for loading
    # the configuration in the next step.
    if (is.character(family)) {
        fams <- match.arg(family, fams)
    }

    res <- setNames(lapply(fams, load_check_config, dir = dir), fams)

    # Any of these 'disabled' for auto testing?
    i <- sapply(res, function(x) isTRUE(x$disabled))
    if (any(i)) {
        if (verbose)
            cat("\n[!] The following families are currently disabled for auto-testing: ",
                paste(names(res)[i], collapse = ", "), "\n")
        res <- res[!i]
        if (length(res) == 0L)
            stop("No non-disabled families remaining. Stop.")
    }

    # When the user asked for a specific family: Return list.
    # Else return a named list containing the configuration of
    # all distribution configs found.
    return(if (is.character(family)) res[[1]] else res)
}


# -------------------------------------------------------------------
# Loads and tests the configuration. Will throw an error if the config file is
# misspecified or can't be sourced.
# -------------------------------------------------------------------
load_check_config <- function(f, dir) {
    file <- file.path(dir, sprintf("config-%s.R", f))
    res <- with(new.env(), {
        tryCatch(source(file, local = TRUE),
                 warning = function(w) stop("Issues sourcing \"", file, "\" (W): ", w),
                 error   = function(e) stop("Issues sourcing \"", file, "\" (E): ", e))
        # Test if 'res' is defined when sourcing the file
        if (!"res" %in% ls())
            stop("Can't find object 'res' when sourcing \"", file, "\".")
        # Test 'res' is a list
        if (!is.list(res))
            stop("Object 'res' created by \"", file, "\" is not a list.")

        # Check 'disabled' flag
        if (!is.logical(res$disabled) || (!isTRUE(res$disabled) && !isFALSE(res$disabled)))
            stop("'res$disabled' (\"", file, "\") not defined or not TRUE or FALSE.")

        # Check if we have the expected elements
        if (!is.character(res$type) || !length(res$type) == 1L)
            stop("'res$type' (\"", file, "\") not character of length 1L.")
        if (!res$type %in% c("Continuous", "Discrete"))
            stop("'res$type' (\"", file, "\") must be \"Continuous\" or \"Discrete\".")

        # Default arguments (named list)
        if (!is.list(res$arguments) || is.null(names(res$arguments)))
            stop("'res$arguments' (\"", file, "\") not a named list.")
        expected <- c("constructor", "d", "p", "q", "r")
        if (!all(expected %in% names(res$arguments)))
            stop("'res$arguments' (\"", file, "\") not containing all required elements: ", paste(expected, collapse = ", "))
        for (e in expected)
            if (!is.expression(res$arguments[[e]]))
                stop("'res$arguments$", e, "' is not an expression.")

        # Support
        if (!is.numeric(res$support) || !length(res$support) == 2L)
            stop("'res$support' (\"", file, "\") not numeric of length 2L.")
        if (res$support[1] >= res$support[2])
            stop("'res$support' (\"", file, "\") not properly defined.")

        # Names of the parameters
        if (!is.character(res$params) || !length(res$params) > 0L || !all(nchar(res$params) > 0L))
            stop("'res$params' (\"", file, "\") misspecified, must be valid character vector with length > 0.")

        # Checking res[[parameter]][[link]] content
        test_param <- function(p, n) {
            x <- res[[p]][[n]]
            # Must exist
            if (is.null(x))
                stop("'res$", p, "$", n, "' (\"", file, "\") not defined (got NULL).")
            # Must be a named list
            if (!is.list(x) || is.null(names(x)) || length(x) == 0L)
                stop("'res$", p, "' (\"", file, "\") must be a named list of length > 0L.")

            # Checking list entries
            expected <- c("valid", "invalid")
            if (!all(expected %in% names(x)))
                stop("'res$", p, "' (\"", file, "\") does not contain all expected elements: ",
                     paste(expected, collapse = ", "))
            if (!is.numeric(x$valid) || length(x$valid) < 3L || any(is.na(x$valid)))
                stop("'res$", p, "$", n, "$valid' (\"", file, "\") must be numeric vector of lenth >2 without missing values.")
            if (!is.null(x$invalid) && (!is.numeric(x$invalid) || length(x$invalid) == 0L || any(is.na(x$invalid))))
                stop("'res$", p, "$", n, "$invalid' (\"", file, "\") must be NULL or numeric vector of lenth >0 without missing values.")
        }
        for (p in res$params) {
            if (!p %in% names(res)) stop("Element $", p, " not found (\"", file, "\").")
            for (n in names(res[[p]])) {
                test_param(p, n)
            }
        }

        # Now we do the same for 'dpqr' parameters
        # Checking res$dpqr[[parameter]] content
        test_dpqr_param <- function(p) {
            x <- res$dpqr[[p]]
            # Must exist
            if (is.null(x))
                stop("'res$dpqr$", p, "' (\"", file, "\") not defined (got NULL).")
            # Must be a named list
            if (!is.list(x) || is.null(names(x)) || length(x) == 0L)
                stop("'res$dpqr$", p, "' (\"", file, "\") must be a named list of length > 0L.")

            # Checking list entries
            expected <- c("valid", "invalid")
            if (!all(expected %in% names(x)))
                stop("'res$dpqr$", p, "' (\"", file, "\") does not contain all expected elements: ",
                     paste(expected, collapse = ", "))
            if (!is.numeric(x$valid) || length(x$valid) < 3L || any(is.na(x$valid)))
                stop("'res$", p, "$", n, "$valid' (\"", file, "\") must be numeric vector of lenth >2 without missing values.")
            if (!is.null(x$invalid) && (!is.numeric(x$invalid) || length(x$invalid) == 0L || any(is.na(x$invalid))))
                stop("'res$", p, "$", n, "$invalid' (\"", file, "\") must be NULL or numeric vector of lenth >0 without missing values.")
        }
        if (!"dpqr" %in% names(res))
            stop("Missing $dpqr configuration (\"", file, "\").")
        if (!is.list(res$dpqr) || is.null(names(res$dpqr)))
            stop("'dpqr' (\"", file, "\") is not a named list.")
        if (!all(res$params %in% names(res$dpqr)))
            stop("Not all parameters found in $dpqr (\"", file, "\"). Expected config for: ", paste(res$params, collapse = ", "))
        for (p in res$params) {
            test_dpqr_param(p)
        }

        return(res)
    })
}

