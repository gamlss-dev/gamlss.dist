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
  
  ## set up distribution
  d <- eval(parse(text = sprintf("data.frame(%s)", paste(par, "=", par, collapse = ", "))))
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
  if (is.null(f$mean)) stop(sprintf("the mean is not implemented for the %s family", attr(x, "family")[1L]))
  m <- do.call(f$mean, as.list(x))
  setNames(m, names(x))
}

variance.GAMLSS <- function(x, ...) {
  f <- .gamlss_family(x)
  if (is.null(f$variance)) stop(sprintf("the variance is not implemented for the %s family", attr(x, "family")[1L]))
  m <- do.call(f$variance, as.list(x))
  setNames(m, names(x))
}

skewness.GAMLSS <- function(x, ...) {
  stop("not yet implemented")
}

kurtosis.GAMLSS <- function(x, ...) {
  stop("not yet implemented")
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
  s <- quantile(d, probs = c(0, 1), elementwise = FALSE)
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
