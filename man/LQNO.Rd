\name{LQNO}
\alias{LQNO}
\alias{dLQNO}
\alias{pLQNO}
\alias{qLQNO}
\alias{rLQNO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normal distribution with a specific mean and variance relationship for fitting a GAMLSS
model}
\description{
The function \code{LQNO()} defines a normal distribution family, which has a specific  mean and variance relationship. The distribution can be used in a GAMLSS fitting using the function gamlss(). The mean of LQNO is equal to \code{mu}. The variance is equal to \code{mu*(1+sigma*mu)}  so the standard deviation is \code{sqrt(mu*(1+sigma*mu))}. The function is found useful in modelling  small RNA sequencing experiments see Argyropoulos \emph{et al.} (2017. The functions \code{dLQNO}, \code{pLQNO}, \code{qLQNO} and \code{rLQNO} define the density, distribution function, quantile function (inverse cdf) and random generation for the \code{LQNO()} parametrization of the normal distribution.
}
\usage{
LQNO(mu.link = "log", sigma.link = "log")
dLQNO(x, mu = 1, sigma = 1, log = FALSE)
pLQNO(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qLQNO(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rLQNO(n, mu = 1, sigma = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{\code{mu} link function with \code{"log"} as default}
  \item{sigma.link}{\code{mu} link function with \code{"log"} as default}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{
logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}}
  \item{lower.tail}{if \code{TRUE} (default), probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required
}
}
\details{\code{LQNO} stands for Linear Quadratic Normal Family, in which the variance is a linear quadratic function of the mean: \code{Var(Y) = mu*(1+sigma*mu)}. This is created to facilitate the analysis of data coming from small RNA sequencing experiments, basically counts of short RNAs that one isolates from cells or biofluids such as urine, plasma or cerebrospinal fluid. Argyropoulos \emph{et al.} (2017) showing that the \code{LQNO} distribution (and the Negative Binomial which implements the same mean- variance relationship) are highly accurate approximations to the generative models of the signals in these experiments}
\value{
The function \code{LQNO} returns a \code{gamlss.family} object which can be used to fit this specific form of the  normal distribution family in the gamlss() function.
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Argyropoulos C, Etheridge A, Sakhanenko N, Galas D. (2017) Modeling bias and
variation in the stochastic processes of small RNA sequencing. \emph{Nucleic Acids Res.}
2017 Mar 27. doi: 10.1093/nar/gkx199. [Epub ahead of print] PubMed PMID:
28369495.
}

\author{Christos Argyropoulos}

\note{The \code{mu} parameters must be positive so for the relationship \code{Var(Y) = mu*(1+sigma*mu)} to be valid.}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{NO}},\code{\link{NO2}}, \code{\link{NOF}}}
\examples{
LQNO()# gives information about the default links for the normal distribution
# a comparison of different Normal models
#m1 <- gamlss(y~pb(x), sigma.fo=~pb(x), data=abdom,  family=NO(mu.link="log"))
#m2 <- gamlss(y~pb(x), sigma.fo=~pb(x), data=abdom, family=LQNO)
#m3 <- gamlss(y~pb(x), sigma.fo=~pb(x), data=abdom, family=NOF(mu.link="log"))
#AIC(m1,m2,m3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
