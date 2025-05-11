


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
    files <- files[grepl("^config_.*\\.R$", basename(files))]
    stopifnot("No config files found on disc" = length(files) > 0L)
    fams  <- regmatches(files, regexpr("(?<=(config_)).*(?=(\\.R))", files, perl = TRUE))

    # If 'family' is chracter, check if it is one of the allowed
    # families, i.e., families for which we have a config file.
    # Replace 'fams' with the requested family; used for loading
    # the configuration in the next step.
    if (is.character(family)) fams <- match.arg(family, fams)

    res <- setNames(lapply(fams, load_check_config, dir = dir), fams)

    # Any of these 'disabled' for auto testing?
    i <- sapply(res, function(x) isTRUE(x$disabled))
    if (any(i)) {
        if (verbose)
            cat("\n[!] The following families are currently disabled for auto-testing:",
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
# Helper function which creates the list with inside/outside values
# for the distribution parameters.
# -------------------------------------------------------------------
get_param_values_list <- function(interval, inside, outside, boundaries) {
    x <- sort(unique(c(interval, inside)))

    # Remove left/right boundary if needed
    if (boundaries %in% c("right", "none")) x <- x[-1L]
    if (boundaries %in% c("left",  "none")) x <- x[-length(x)]

    return(list(boundaries = boundaries, inside = x, outside = outside))
}

# -------------------------------------------------------------------
# Loads and tests the configuration. Will throw an error if the config file is
# misspecified or can't be sourced.
# -------------------------------------------------------------------
load_check_config <- function(f, dir) {
    file <- file.path(dir, sprintf("config_%s.R", f))
    res <- with(new.env(), {
        tryCatch(source(file, local = TRUE),
                 warning = function(w) stop("Issues sourcing \"", file, "\" (W): ", w),
                 error   = function(e) stop("Issues sourcing \"", file, "\" (E): ", e))

        # Check newly added variables to find the config list defined.
        # Only consider list objects; if we find != 1L - throw an error.
        # Else we assume that one list object is our config, checked below.
        config_obj <- sapply(ls(), function(o) is.list(get(o)))
        if (sum(config_obj) == 0L) {
            stop("Can't find list object with configuration when sourcing \"", file, "\".")
        } else if (sum(config_obj) > 1L) {
            stop("Found multiple list object when sourcing \"", file, "\"; not sure which one is the configuration list element.")
        }
        config_obj <- names(config_obj)[config_obj]
        config     <- get(config_obj)

        # Helper function for output (errors).
        # Scopes 'config_obj' and 'file'.
        cvar <- function(x) sprintf("'%s$%s' (in \"%s\") ", config_obj, x, file)

        # Check 'disabled' flag
        if (!is.logical(config$disabled) || (!isTRUE(config$disabled) && !isFALSE(config$disabled)))
            stop(cvar("disabled"), "not defined or not TRUE or FALSE.")

        # Check if we have the expected elements
        if (!is.character(config$type) || !length(config$type) == 1L)
            stop(cvar("type"), "not character of length 1L.")
        if (!config$type %in% c("Continuous", "Discrete"))
            stop(cvar("type"), "must be \"Continuous\" or \"Discrete\".")

        # Support
        if (!is.numeric(config$support) || !length(config$support) == 2L)
            stop(cvar("support"), "not numeric of length 2L.")
        if (config$support[1] >= config$support[2])
            stop(cvar("support"), "not properly defined.")

        # Default arguments (named list)
        if (!is.list(config$arguments) || is.null(names(config$arguments)))
            stop(cvar("arguments"), "not a named list.")
        expected <- c("family", "d", "p", "q", "r")
        if (!all(expected %in% names(config$arguments)))
            stop(cvar("arguments"), "not containing all required elements: ", paste(expected, collapse = ", "))
        for (e in expected)
            if (!is.expression(config$arguments[[e]]))
                stop(cvar(paste0("arguments$", e)), "' is not an expression.")

        # Names of the parameters
        if (!is.character(config$params) || !length(config$params) > 0L || !all(nchar(config$params) > 0L))
            stop(cvar("params"), "misspecified, must be valid character vector with length > 0.")


        # Testing 'links' config
        if (is.null(config$links) || !is.list(config$links) || is.null(names(config$links)))
            stop(cvar("links"), "must be a named list.")
        # Testing config$links[[parameter]] specification
        test_links <- function(p) {
            # Not defined?
            if (!p %in% names(config$links)) stop("Element ", cvar(paste0("links$", p)), "not found.")
            # Else check content
            x <- config$links[[p]]
            if (!is.character(x) || length(x) == 0L)
                stop(cvar(paste0("links$", p)), "must be character vector length > 0L.")
            if (any(is.na(x)))
                stop(cvar(paste0("links$", p)), "contains missing values (not allowed).")
        }
        for (p in config$params) test_links(p)

        # Checking specificaction for response y
        if (is.null(config$y) || !is.list(config$y) || is.null(names(config$y)))
            stop(cvar("y"), "must be a named list.")

        # Checking config$y$inside: Must be numeric vector, monotonically
        # increasing (no missing values, no duplicates)
        if (!is.numeric(config$y$inside) || length(config$y$inside) == 0L || any(is.na(config$y$inside)))
            stop(cvar("y$inside"), "must be numeric vector of length >0 without missing values.")
        if (!all(diff(config$y$inside) > 0))
            stop(cvar("y$inside"), "must be monotonically increasing.")


        if (!is.null(config$y$outside) && !(is.numeric(config$y$outside) || length(config$y$outside) > 0L || all(!is.na(config$y$outside))))
            stop(cvar("y$outside"), "must be NULL or a numeric vector of length >0 without missing values.")
        if (!is.null(config$y$outside) && !all(diff(confi$y$outside) > 0))
            stop(cvar("y$outside"), "must be monotonically increasing if not NULL.")

        # Checking config[[parameter]][[link]] content; scopes 'config'
        test_param <- function(p, n) {
            # Not defined?
            if (!p %in% names(config)) stop("Element ", cvar(p), "not found.")
            # Else check content
            x <- config[[p]]
            # Must exist
            if (is.null(x) || !is.list(x) || is.null(names(x)))
                stop(cvar(p), "must be a named list.")

            # Checking list entries
            expected <- c("interval", "inside", "outside", "dpqr", "family")
            if (!all(expected %in% names(x)))
                stop(cvar(p), "does not contain all expected elements: ", paste(expected, collapse = ", "))

            # x$interval expected to be a numeric vector of length 2, no missing values allowed
            if (!is.numeric(x$interval) || !length(x$interval) == 2L || any(is.na(x$interval)))
                stop(cvar(paste0(p, "$interval")), "must be numeric vector of length 2 without missing values.")

            # x$inside must be a numeric vector, monotonically increasing, no missing values.
            if (!is.numeric(x$inside) || length(x$inside) == 0L || any(is.na(x$inside)))
                stop(cvar(paste0(p, "$inside")), "must be numeric vector of length >0 without missing values.")
            if (!all(diff(x$inside) > 0))
                stop(cvar(paste0(p, "$inside")), "must be monotonically increasing.")

            # x$outside mut be either NULL or a monotonically increasing numeric vector.
            if (!is.null(x$outside) && !(is.numeric(x$outside) || length(x$outside) > 0L || all(!is.na(x$outside))))
                stop(cvar(paste0(p, "$outside")), "must be NULL or a numeric vector of length >0 without missing values.")
            if (!is.null(x$outside) && !all(diff(x$outside) > 0))
                stop(cvar(paste0(p, "$outside")), "must be monotonically increasing if not NULL.")

            # x$dpqr and x$family must be one of: "left", "right", "both", or "none"
            expected <- c("left", "right", "both", "none")
            expected_str <- paste(sprintf("\"%s\"", expected), collapse = ", ")
            if (!is.character(x$dpqr) || length(x$dpqr) != 1L || !x$dpqr %in% expected)
                stop(cvar(paste0(p, "$dpqr")), "must be one of: ", expected_str, ".")
            if (!is.character(x$family) || length(x$family) != 1L || !x$family %in% expected)
                stop(cvar(paste0(p, "$family")), "must be one of: ", expected_str, ".")
        }

        for (p in config$params) {
            # Testing configuration for parameter p
            test_param(p, n)

            # Now we are sure x$inside, x$outside, x$dpqr and x$family are all properly defined,
            # we can calculate the 'inside/outside' values for x$family and x$dpqr.
            config[[c(p, "family")]] <- with(config[[p]], get_param_values_list(interval, inside, outside, family))
            config[[c(p, "dpqr")]]   <- with(config[[p]], get_param_values_list(interval, inside, outside, dpqr))
        }

        # Seems the configuration list contains what we expect, return
        return(config)
    })

    return(res)
}

