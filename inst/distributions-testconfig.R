## # -------------------------------------------------------------------
## # Limits for discrete and continuous families (for automated tests)
## # -------------------------------------------------------------------
## get_limits <- function(type = c("discrete", "continuous")) {
##     type <- match.arg(type)
## 
##     if (type == "discrete") {
##         res <- list(
##                y.range  = c(0,Inf),
##                p.range  = c(0, 1),
##                mu.range = c(0,Inf),
##             sigma.range = c(0,Inf),
##                nu.range = c(0,Inf),
##               tau.range = c(0,Inf),
##                  mu.val = c(.5,5,10,30),
##               sigma.val = c(.5,5,10,30),
##                  nu.val = c(.5,5,10,30),
##                 tau.val = c(.5,5,10,30),
##                       N = 100,
##                      bd = NULL,
##                    save = TRUE,
##                   trace = TRUE,
##                    crit = 0.001
##         )
##     } else {
##         res <- list(
##                 y.range = c(-Inf,Inf), # the range of the resposnse..
##                p.range  = c(0, 1),
##                mu.range = c(-Inf,Inf), #.
##             sigma.range = c(0,Inf),
##                nu.range = c(0,Inf),
##               tau.range = c(0,Inf),
##                  mu.val = c(0,1,10,30), # which values to test
##               sigma.val = c(1,5,10,30),
##                  nu.val = c(1,5,10,30),
##                 tau.val = c(1,5,10,30),
##                       N = 100,
##                    save = TRUE,
##                   trace = TRUE,
##                    crit = 0.001 # what difference we are looking
##         )
##     }
##     return(res)
## }


# -------------------------------------------------------------------
# Limits for discrete and continuous families (for automated tests)
# -------------------------------------------------------------------
get_testconfig <- function(family = NULL) {

    # Used to add some delta to test for 'out of support' values
    eps <- sqrt(.Machine$double.eps)

    # Setting up results list with config for all distributions
    res <- list()

    # ---------------------------------------------------------------
    # Configuration for NO and NO2 (Normal)
    # ---------------------------------------------------------------
    res$NO <- res$NO2 <- list(
        type    <- "Continuous",
        support <- c(-Inf, Inf),
        # Distribution parameters with support (range) and all available links
        params <- list("mu"    = list(range = c(-Inf, Inf),
                                      links = c("inverse", "log", "identity", "own")),
                       "sigma" = list(range = c(0, Inf),
                                      links = c("inverse", "log", "identity", "own"))
                       ),
        # Valid values used for testing dpqr
        values  <- list("y"     = c(-30, -5 -1, 0, 1, 5, 30),
                        "mu"    = c(-30, -5 -1, 0, 1, 5, 30),
                        "sigma" = c(eps, 1, 5, 30)),
        # Illegal values, any combination of these must fail
        illegal <- list("y"     = 0,
                        "mu"    = 0,
                        "sigma" = c(0, -0.0001, -100))
    )

    # ---------------------------------------------------------------
    # Configuration for LO (Logistic)
    # ---------------------------------------------------------------
    res$LO <- list(
        type    <- "Continuous",
        support <- c(-Inf, Inf),
        # Distribution parameters with support (range) and all available links
        params <- list("mu"    = list(range = c(-Inf, Inf),
                                      links = c("inverse", "log", "identity", "own")),
                       "sigma" = list(range = c(0, Inf),
                                      links = c("inverse", "log", "identity", "own"))
                       ),
        # Valid values used for testing dpqr
        values <- list("y"     = c(-30, -5 -1, 0, 1, 5, 30),
                       "mu"    = c(-30, -5 -1, 0, 1, 5, 30),
                       "sigma" = c(eps, 1, 5, 30)),
        # Illegal values, any combination of these must fail
        illegal <- list("y"     = 0,
                        "mu"    = 0,
                        "sigma" = c(0, -0.0001, -100))
    )

    # ---------------------------------------------------------------
    # Configuration for BE (Beta)
    # ---------------------------------------------------------------
    res$BE <- list(
        type   <- "Continuous",
        support <- c(0, 1),
        # Distribution parameters with support (range) and all available links
        params <- list("mu"    = list(range = c(0, 1),
                                      links = c("logit", "probit", "cloglog", "cauchit", "log", "own")),
                       "sigma" = list(range = c(0, 1),
                                      links = c("logit", "probit", "cloglog", "cauchit", "log", "own"))
                       ),
        # Valid values used for testing dpqr
        values <- list("y"     = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps),
                       "mu"    = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps),
                       "sigma" = c(0 + eps, 0.1, 0.5, 0.9, 1 - eps)),
        # Illegal values, any combination of these must fail
        illegal <- list("y"     = c(-10, -0.00001, 0, 1, 1.00001, 10),
                        "mu"    = c(-10, -0.00001, 0, 1, 1.00001, 10),
                        "sigma" = c(-10, -0.00001, 0, 1, 1.00001, 10))
    )

    # ---------------------------------------------------------------
    # If 'is.null(family)' we return the entire list.
    # Else we check if the family requested by the user is valid.
    # ---------------------------------------------------------------
    if (!is.null(family)) {
        family <- match.arg(family, names(res))
        res <- res[[family]]
    }
    return(res)
}








