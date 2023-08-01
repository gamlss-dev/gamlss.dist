\name{GAMLSS}

\alias{GAMLSS}
\alias{format.GAMLSS}
\alias{print.GAMLSS}
\alias{mean.GAMLSS}
\alias{variance.GAMLSS}
\alias{skewness.GAMLSS}
\alias{kurtosis.GAMLSS}
\alias{pdf.GAMLSS}
\alias{log_pdf.GAMLSS}
\alias{cdf.GAMLSS}
\alias{quantile.GAMLSS}
\alias{random.GAMLSS}
\alias{support.GAMLSS}
\alias{is_discrete.GAMLSS}
\alias{is_continuous.GAMLSS}

\title{Create a GAMLSS Distribution}

\description{
A single class and corresponding methods encompassing all distributions
from the \pkg{gamlss.dist} package using the workflow from the
\pkg{distributions3} package.
}

\usage{
GAMLSS(family, mu, sigma, tau, nu)
}

\arguments{
  \item{family}{character. Name of a GAMLSS family provided by \pkg{gamlss.dist}, e.g.,
    \code{\link{NO}} or \code{\link{BI}} for the normal or binomial distribution, respectively.}
  \item{mu}{numeric. GAMLSS \code{mu} parameter. Can be a scalar or a vector
    or missing if not part of the \code{family}.}
  \item{sigma}{numeric. GAMLSS \code{sigma} parameter. Can be a scalar or a vector
    or missing if not part of the \code{family}.}
  \item{tau}{numeric. GAMLSS \code{tau} parameter. Can be a scalar or a vector
    or missing if not part of the \code{family}.}
  \item{nu}{numeric. GAMLSS \code{nu} parameter. Can be a scalar or a vector
    or missing if not part of the \code{family}.}
}

\details{
The S3 class \code{GAMLSS} provides a unified workflow based on the
\pkg{distributions3} package (see Zeileis et al. 2022) for all distributions
from the \pkg{gamlss.dist} package. The idea is that from fitted
\code{\link[gamlss]{gamlss}} model objects predicted probability distributions
can be obtained for which moments (mean, variance, etc.), probabilities,
quantiles, etc. can be obtained with corresponding generic functions.

The constructor function \code{GAMLSS} sets up a distribution
object, representing a distribution from the GAMLSS (generalized additive
model of location, scale, and shape) framework by the corresponding parameters
plus a \code{family} attribute, e.g., \code{\link{NO}} for the
normal distribution or \code{\link{BI}} for the binomial
distribution. There can be up to four parameters, called \code{mu} (often some
sort of location parameter), \code{sigma} (often some sort of scale parameter),
\code{tau} and \code{nu} (other parameters, e.g., capturing shape, etc.).

All parameters can also be vectors, so that it is possible to define a vector
of GAMLSS distributions from the same family with potentially different parameters.
All parameters need to have the same length or must be scalars (i.e.,
of length 1) which are then recycled to the length of the other parameters.

Note that not all distributions use all four parameters, i.e., some use just a
subset. In that case, the corresponding arguments in \code{GAMLSS} should be
unspecified, \code{NULL}, or \code{NA}.

For the \code{GAMLSS} distribution objects there is a wide range
of standard methods available to the generics provided in the \pkg{distributions3}
package: \code{\link[distributions3]{pdf}} and \code{\link[distributions3]{log_pdf}}
for the (log-)density (PDF), \code{\link[distributions3]{cdf}} for the probability
from the cumulative distribution function (CDF), \code{quantile} for quantiles,
\code{\link[distributions3]{random}} for simulating random variables,
and \code{\link[distributions3]{support}} for the support interval
(minimum and maximum). Internally, these methods rely on the usual d/p/q/r
functions provided in \pkg{gamlss.dist}, see the manual pages of the individual
families. The methods \code{\link[distributions3]{is_discrete}} and
\code{\link[distributions3]{is_continuous}} can be used to query whether the
distributions are discrete on the entire support or continuous on the entire
support, respectively.

See the examples below for an illustration of the workflow for the class and methods.
}

\value{
A \code{GAMLSS} object, inheriting from \code{distribution}.
}

\references{
Zeileis A, Lang MN, Hayes A (2022).
\dQuote{distributions3: From Basic Probability to Probabilistic Regression.}
Presented at \emph{useR! 2022 - The R User Conference}.
Slides, video, vignette, code at \url{https://www.zeileis.org/news/user2022/}.
}

\seealso{
\code{\link{gamlss.family}}
}

\examples{
\dontshow{ if(!requireNamespace("distributions3")) {
  if(interactive() || is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", NA))) {
    stop("not all packages required for the example are installed")
  } else q() }
}
## package and random seed
library("distributions3")
set.seed(6020)

## three Weibull distributions
X <- GAMLSS("WEI", mu = c(1, 1, 2), sigma = c(1, 2, 2))
X

## moments
mean(X)
variance(X)

## support interval (minimum and maximum)
support(X)
is_discrete(X)
is_continuous(X)

## simulate random variables
random(X, 5)

## histograms of 1,000 simulated observations
x <- random(X, 1000)
hist(x[1, ], main = "WEI(1,1)")
hist(x[2, ], main = "WEI(1,2)")
hist(x[3, ], main = "WEI(2,2)")

## probability density function (PDF) and log-density (or log-likelihood)
x <- c(2, 2, 1)
pdf(X, x)
pdf(X, x, log = TRUE)
log_pdf(X, x)

## cumulative distribution function (CDF)
cdf(X, x)

## quantiles
quantile(X, 0.5)

## cdf() and quantile() are inverses
cdf(X, quantile(X, 0.5))
quantile(X, cdf(X, 1))

## all methods above can either be applied elementwise or for
## all combinations of X and x, if length(X) = length(x),
## also the result can be assured to be a matrix via drop = FALSE
p <- c(0.05, 0.5, 0.95)
quantile(X, p, elementwise = FALSE)
quantile(X, p, elementwise = TRUE)
quantile(X, p, elementwise = TRUE, drop = FALSE)

## compare theoretical and empirical mean from 1,000 simulated observations
cbind(
  "theoretical" = mean(X),
  "empirical" = rowMeans(random(X, 1000))
)
}
