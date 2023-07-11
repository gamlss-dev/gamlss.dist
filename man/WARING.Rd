\name{WARING}
\alias{WARING}
\alias{dWARING}
\alias{pWARING}
\alias{qWARING}
\alias{rWARING}
\title{
Waring distribution for fitting a GAMLSS model
}
\description{
The function \code{WARING()} defines the Waring distribution, a two parameter 
distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}, with mean equal to the parameter \code{mu} and scale parameter \code{sigma}. The functions \code{dWARING}, \code{pWARING}, \code{qWARING} and \code{rWARING} define the density, distribution function, quantile function and random generation for the \code{WARING} parameterization of the Waring distribution. 
}
\usage{
WARING(mu.link = "log", sigma.link = "log")
dWARING(x, mu = 2, sigma = 2, log = FALSE)
pWARING(q, mu = 2, sigma = 2, lower.tail = TRUE, log.p = FALSE) 
qWARING(p, mu = 2, sigma = 2, lower.tail = TRUE, log.p = FALSE, 
    max.value = 10000)
rWARING(n, mu = 2, sigma = 2)
}
\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{x}{vector of (non-negative integer) quantiles.}
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of random values to return.}
  \item{mu}{vector of positive \code{mu} values.}
  \item{sigma}{vector of positive \code{sigma} values.}
  \item{lower.tail}{logical; if \code{TRUE} (default) probabilities are \eqn{P[Y\leq y]}, otherwise, \eqn{P[Y>y]}.}
  \item{log, log.p}{logical; if \code{TRUE} probabilities p are given as log(p).}
  \item{max.value}{constant; generates a sequence of values for the cdf function.}
}
\details{
The Waring distribution, \code{WARING},  has density,
\deqn{f(y|\mu, \sigma)= \frac{B(y+\mu \sigma^{-1}, \sigma^{-1}+2)}{B(\mu \sigma^{-1}, \sigma^{-1}+1)}}
for \eqn{y=0,1,2,\ldots}, \eqn{\mu>0} and \eqn{\sigma>0} see  pp. 490-492  of Rigby \emph{et al}. (2019).  
}
\value{
Returns a \code{gamlss.family} object which can be used to fit a Waring distribution in the \code{gamlss()} function.
}
\references{ 
Wimmer, G. and Altmann, G. (1999) \emph{Thesaurus of univariate discrete probability distributions.} Stamm.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).
}
\author{
Fiona McElduff, Bob Rigby and Mikis Stasinopoulos.
\email{f.mcelduff@ich.ucl.ac.uk}
}
\seealso{
\code{\link{gamlss.family}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(0,20,1)
plot(y, dWARING(y), type="h")
q <- seq(0, 20, 1)
plot(q, pWARING(q), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p , qWARING(p), type="s")
dat <- rWARING(100)
hist(dat)
#summary(gamlss(dat~1, family=WARING))
}
\keyword{distribution}
\keyword{regression}