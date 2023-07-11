\name{IGAMMA}
\alias{IGAMMA}
\alias{dIGAMMA}
\alias{pIGAMMA}
\alias{qIGAMMA}
\alias{rIGAMMA}
\title{
Inverse Gamma distribution for fitting a GAMLSS
}
\description{
The function \code{IGAMMA()} defines the Inverse Gamma distribution, a two parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}, with parameters \code{mu} (the mode) and \code{sigma}. The functions \code{dIGAMMA}, \code{pIGAMMA}, \code{qIGAMMA} and \code{rIGAMMA} define the density, distribution function, quantile function and random generation for the \code{IGAMMA} parameterization of the Inverse Gamma distribution. 
}
\usage{

IGAMMA(mu.link = "log", sigma.link="log")
dIGAMMA(x, mu = 1, sigma = .5, log = FALSE)
pIGAMMA(q, mu = 1, sigma = .5, lower.tail = TRUE, log.p = FALSE)
qIGAMMA(p, mu = 1, sigma = .5, lower.tail = TRUE, log.p = FALSE)
rIGAMMA(n, mu = 1, sigma = .5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with \code{log} link as the default for the mu parameter}
  \item{sigma.link}{Defines the \code{sigma.link}, with \code{log} as the default for the sigma parameter }
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
}
\details{
The parameterization of the Inverse Gamma distribution in the function \code{IGAMMA} is 
\deqn{f(y|\mu, \sigma) = \frac{\left[\mu\,(\alpha+1)\right]^{\alpha}}{\Gamma(\alpha)} \,y^{-(\alpha+1)}\, \exp{\left[-\frac{\mu\,(\alpha+1)}{y}\right]}}{f(y|mu, sigma) = ([mu (alpha+1)]^alpha)/Gamma(alpha) y^(-(alpha+1)) exp(-(mu (alpha+1))/y)}
where \eqn{\alpha = 1/(\sigma^2)}
for \eqn{y>0}, \eqn{\mu>0} and \eqn{\sigma>0} see pp. 424-426 of Rigby et al. (2019).
}
\value{
returns a gamlss.family object which can be used to fit an Inverse Gamma distribution in the \code{gamlss()} function.}
\author{
Fiona McElduff, Bob Rigby and Mikis Stasinopoulos. }
\references{
Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), \emph{Appl. Statist.}, 54, part 3, pp 507-554. 

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R. \emph{Journal of Statistical Software}, Vol. 23, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\note{
For the function \code{IGAMMA()}, \emph{mu} is the mode of the Inverse Gamma distribution.
}

\seealso{
\code{\link{gamlss.family}}, \code{\link{GA}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(0.2,20,0.2)
plot(y, dIGAMMA(y), type="l")
q <- seq(0.2, 20, 0.2)
plot(q, pIGAMMA(q), type="l")
p<-seq(0.0001,0.999,0.05)
plot(p , qIGAMMA(p), type="l")
dat <- rIGAMMA(50)
hist(dat)
#summary(gamlss(dat~1, family="IGAMMA"))
}
\keyword{distribution}
\keyword{regression}

