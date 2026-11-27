## create GAMLSS distributions3 objects: a single class and corresponding methods
## encompassing all distributions from the gamlss.dist package using the workflow
## from the distributions3 package
GAMLSS <- function(family, mu, sigma, tau, nu) {
  ## get family object
  f <- .gamlss_family(family)

  ## get parameters
  par <- names(f$parameters)
  if(!("mu" %in% par)    && !missing(mu)    && (!is.null(mu)    || !is.na(mu)))    warning(sprintf("'mu' is not a parameter of the '%s' family", family))
  if(!("sigma" %in% par) && !missing(sigma) && (!is.null(sigma) || !is.na(sigma))) warning(sprintf("'sigma' is not a parameter of the '%s' family", family))
  if(!("tau" %in% par)   && !missing(tau)   && (!is.null(tau)   || !is.na(tau)))   warning(sprintf("'tau' is not a parameter of the '%s' family", family))
  if(!("nu" %in% par)    && !missing(nu)    && (!is.null(nu)    || !is.na(nu)))    warning(sprintf("'nu' is not a parameter of the '%s' family", family))

  ## assure defaults
  if(missing(mu))    mu <- NULL
  if(missing(sigma)) sigma <- NULL
  if(missing(tau))   tau <- NULL
  if(missing(nu))    nu <- NULL
  
  ## set up distribution
  d <- list(mu = mu, sigma = sigma, tau = tau, nu = nu)[par]
  for (i in par) {
    if (!is.numeric(d[[i]])) d[[i]] <- as.numeric(d[[i]])
  }
  d <- do.call(data.frame, d)
  class(d) <- c("GAMLSS", "distribution")
  attr(d, "family") <- f$family
  return(d)
}

## auxiliary functions for getting family and d/p/q/r function
## from gamlss.dist namespace (FIXME: where to get() for user-defined family?)
.gamlss_family <- function(x) get(if(is.character(x)) x else attr(x, "family")[1L], asNamespace("gamlss.dist"))()
.gamlss_dpqr <- function(x, type) get(paste0(type, attr(x, "family")[1L]), asNamespace("gamlss.dist"))

## S3 methods
format.GAMLSS <- function(x, digits = pmax(3L, getOption("digits") - 3L), ...) {
  class(x) <- c(paste("GAMLSS", attr(x, "family")[1L]), "distribution")
  NextMethod()
}

print.GAMLSS <- function(x, digits = pmax(3L, getOption("digits") - 3L), ...) {
  class(x) <- c(paste("GAMLSS", attr(x, "family")[1L]), "distribution")
  NextMethod()
}

mean.GAMLSS <- function(x, ...) {
  f <- .gamlss_family(x)
  if (is.null(f$mean)) return(NextMethod())
  m <- do.call(f$mean, as.list(x))
  setNames(m, names(x))
}

variance.GAMLSS <- function(x, ...) {
  f <- .gamlss_family(x)
  if (is.null(f$variance)) return(NextMethod())
  m <- do.call(f$variance, as.list(x))
  setNames(m, names(x))
}

pdf.GAMLSS <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  stopifnot(requireNamespace("distributions3"))
  FUN <- function(at, d) do.call(.gamlss_dpqr(d, "d"), c(list(x = at), as.list(d), ...))
  distributions3::apply_dpqr(d = d, FUN = FUN, at = x, type = "density", drop = drop, elementwise = elementwise)
}

log_pdf.GAMLSS <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  stopifnot(requireNamespace("distributions3"))
  FUN <- function(at, d) do.call(.gamlss_dpqr(d, "d"), c(list(x = at), as.list(d), list(log = TRUE)))
  distributions3::apply_dpqr(d = d, FUN = FUN, at = x, type = "logLik", drop = drop, elementwise = elementwise)
}

cdf.GAMLSS <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  stopifnot(requireNamespace("distributions3"))
  FUN <- function(at, d) do.call(.gamlss_dpqr(d, "p"), c(list(q = at), as.list(d), ...))
  distributions3::apply_dpqr(d = d, FUN = FUN, at = x, type = "probability", drop = drop, elementwise = elementwise)
}

quantile.GAMLSS <- function(x, probs, drop = TRUE, elementwise = NULL, ...) {
  stopifnot(requireNamespace("distributions3"))
  FUN <- function(at, d) do.call(.gamlss_dpqr(d, "q"), c(list(p = at), as.list(d), ...))
  distributions3::apply_dpqr(d = x, FUN = FUN, at = probs, type = "quantile", drop = drop, elementwise = elementwise)
}

random.GAMLSS <- function(x, n = 1L, drop = TRUE, ...) {
  stopifnot(requireNamespace("distributions3"))
  n <- distributions3::make_positive_integer(n)
  if (n == 0L) return(numeric(0L))
  FUN <- function(at, d) do.call(.gamlss_dpqr(d, "r"), c(list(n = at), as.list(d)))
  distributions3::apply_dpqr(d = x, FUN = FUN, at = n, type = "random", drop = drop)
}

support.GAMLSS <- function(d, drop = TRUE, ...) {
  stopifnot(requireNamespace("distributions3"))
  s <- quantile(d, probs = c(0, 1), elementwise = FALSE, drop = FALSE)
  distributions3::make_support(s[, 1L], s[, 2L], d, drop = drop)
}

is_discrete.GAMLSS <- function(d, ...) {
  f <- .gamlss_family(d)
  if (is.null(f$type)) stop(sprintf("the type is not implemented for the %s family", attr(d, "family")[1L]))
  setNames(rep.int(f$type == "Discrete", length(d)), names(d))
}

is_continuous.GAMLSS <- function(d, ...) {
  f <- .gamlss_family(d)
  if (is.null(f$type)) stop(sprintf("the type is not implemented for the %s family", attr(d, "family")[1L]))
  setNames(rep.int(f$type == "Continuous", length(d)), names(d))
}

score.GAMLSS <- function(d, x, which = NULL, drop = TRUE, ...) {
  f <- .gamlss_family(d)

  ## Calculate max length 'n' (plus input sanity check), get parameter names of
  ## the distribution 'd', and evaluate available/check requested derivative names
  n      <- distributions3::max_length(d, x)
  params <- names(unclass(d))
  which  <- distributions3::get_deriv_names(params, which = which, expand = FALSE, check = FALSE)

  ## always use all arguments
  deriv_nam <- c(mu = "dldm", sigma = "dldd", nu = "dldv", tau = "dldt")
  deriv_req <- deriv_nam[unique(names(which))]
  if (!all(deriv_req %in% names(f))) stop(sprintf("not all score functions available for '%s' family", f$family[1L])) ## return(NextMethod())
  args <- as.pairlist(alist(y =, mu =, sigma =, tau =, nu =))
  for (deriv in deriv_req) formals(f[[deriv]]) <- args

  ## compute scores
  scr <- function(par, d, x) f[[deriv_nam[par]]](y = x, mu = d$mu, sigma = d$sigma, nu = d$nu, tau = d$tau)

  ## Calculate derivatives, prepare return object
  return(distributions3::apply_deriv(d, x, FUN = scr, which = which, drop = drop, check = FALSE))
}

hessian.GAMLSS <- function(d, x, which = NULL, drop = TRUE, expected = FALSE, ...) {
  f <- .gamlss_family(d)

  stopifnot("argument 'expected' must be TRUE or FALSE" = isTRUE(expected) || isFALSE(expected))
  if (expected && (missing(x) || is.null(x))) x <- 0 # dummy

  ## usually gamlss.dist only provide the expected hessian
  ## exceptions can be handled here
  if (expected) {
    unavail <- c("")
    if (f$family[1L] %in% unavail) return(NextMethod())
  } else {
    avail <- c("")
    if (!(f$family[1L] %in% avail)) return(NextMethod())
  }

  ## Calculate max length 'n' (plus input sanity check), get parameter names of
  ## the distribution 'd', and evaluate available/check requested derivative names
  n      <- distributions3::max_length(d, x)
  params <- names(unclass(d))
  which  <- distributions3::get_deriv_names(params, which = which, expand = TRUE)
  if (length(x) < n) x <- rep.int(x, n)

  ## always use all arguments
  deriv_nam <- c("mu" = "d2ldm2", "mu:sigma" = "d2ldmdd", "mu:nu" = "d2ldmdv",
    "sigma:nu" = "d2ldddv", "sigma" = "d2ldd2", "mu:tau" = "d2ldmdt", "sigma:tau" = "d2ldddt",
    "nu" = "d2ldv2", "nu:tau" = "d2ldvdt", "tau" = "d2ldt2")
  deriv_req <- deriv_nam[unique(names(which))]
  if (!all(deriv_req %in% names(f))) stop(sprintf("not all hessian functions available for '%s' family", f$family[1L])) ## return(NextMethod())
  args <- as.pairlist(alist(y =, mu =, sigma =, tau =, nu =))
  for (deriv in deriv_req) formals(f[[deriv]]) <- args

  ## compute hessian
  hess <- function(par, d, x) f[[deriv_nam[par]]](y = x, mu = d$mu, sigma = d$sigma, nu = d$nu, tau = d$tau)

  ## Calculate derivatives, prepare return object
  return(distributions3::apply_deriv(d, x, FUN = hess, which = which, drop = drop, check = FALSE))
}
