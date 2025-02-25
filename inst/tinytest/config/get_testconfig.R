


# -------------------------------------------------------------------
# Helper function for loading one or multiple test configs
# -------------------------------------------------------------------
get_testconfig <- function(family = NULL, dir = "config") {
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

        # Check if we have the expected elements
        if (!is.character(res$type) || !length(res$type) == 1L)
            stop("'res$type' (\"", file, "\") not character of length 1L.")
        if (!res$type %in% c("Continuous", "Discrete"))
            stop("'res$type' (\"", file, "\") must be \"Continuous\" or \"Discrete\".")

        # Support
        if (!is.numeric(res$support) || !length(res$support) == 2L)
            stop("'res$support' (\"", file, "\") not numeric of length 2L.")
        if (res$support[1] >= res$support[2])
            stop("'res$support' (\"", file, "\") not properly defined.")

        # 'Good' Values
        if (!is.list(res$values) || is.null(names(res$values)))
            stop("'res$values' (\"", file, "\") must be a named list.")
        lapply(res$values, function(x) {
            if (!is.numeric(x))
                stop("entries in 'res$values' (\"", file, "\") must be numeric.")
        })

        # 'Illegal' Values
        if (!is.list(res$illegal) || is.null(names(res$illegal)))
            stop("'res$illegal' (\"", file, "\") must be a named list.")
        lapply(res$illegal, function(x) {
            if (!is.numeric(x))
                stop("entries in 'res$illegal' (\"", file, "\") must be numeric.")
        })


        return(res)
    })
}

