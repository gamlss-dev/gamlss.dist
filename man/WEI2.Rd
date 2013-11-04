\name{WEI2}
\alias{WEI2}
\alias{dWEI2}
\alias{pWEI2}
\alias{qWEI2}
\alias{rWEI2}


\title{A specific parameterization of the  Weibull
distribution for fitting a GAMLSS}
\description{
The function \code{WEI2} can be used to define the Weibull distribution, a two parameter distribution, for a
\code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
This is the parameterization of the Weibull distribution usually used in proportional hazard models and is defined in details below.
[Note that the GAMLSS function \code{WEI} uses a 
different parameterization for fitting the Weibull distribution.]
The functions \code{dWEI2}, \code{pWEI2}, \code{qWEI2} and \code{rWEI2} define the density, distribution function, quantile function and random
generation for the specific parameterization of the Weibull distribution. 
}
\usage{
WEI2(mu.link = "log", sigma.link = "log")
dWEI2(x, mu = 1, sigma = 1, log = FALSE)
pWEI2(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qWEI2(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rWEI2(n, mu = 1, sigma = 1)
}

\arguments{
 \item{mu.link}{Defines the \code{mu.link}, with "log" link as the default for the mu parameter, other links are  "inverse" and "identity"}
 \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter, other link is the "inverse" and "identity" }
 \item{x,q}{vector of quantiles}
 \item{mu}{vector of the mu parameter values }
 \item{sigma}{vector of sigma parameter values}
 \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
 \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
 \item{p}{vector of probabilities. }
 \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
 The parameterization of the function \code{WEI2} is given by
 \deqn{f(y|\mu,\sigma)= \sigma\mu y^{\sigma-1}e^{-\mu
y^{\sigma}}}{f(y|mu,sigma)= sigma*mu*y^(sigma-1)exp(-mu*y^sigma)}
 for \eqn{y>0}, \eqn{\mu>0} and \eqn{\sigma>0}.
The GAMLSS functions \code{dWEI2}, \code{pWEI2}, \code{qWEI2}, and \code{rWEI2} can be used to provide the pdf, the cdf, the quantiles and
random generated numbers for the Weibull distribution with argument \code{mu}, and \code{sigma}.
[See the GAMLSS function \code{WEI} for a different parameterization of the Weibull.] 
}
\value{
  \code{WEI2()} returns a \code{gamlss.family} object which can be used to fit a Weibull distribution in the \code{gamlss()} function.
  \code{dWEI2()} gives the density, \code{pWEI2()} gives the distribution
     function, \code{qWEI2()} gives the quantile function, and \code{rWEI2()}
     generates random deviates. The latest functions are based on the equivalent \code{R} functions for Weibull distribution.
}
\references{ 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }

\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Calliope Akantziliotou }

\note{ The mean in \code{WEI2} is given by \eqn{ \mu^{-1/\sigma} \Gamma (
\frac{1}{\sigma}+1 ) }{mu^(-1/sigma)*gamma((1/sigma)+1)} and the variance
\eqn{ \mu^{-2/\sigma} ( \Gamma ( \frac{2}{\sigma}+1 )- \left[ \Gamma (
\frac{1}{\sigma}+1 )\right]^2
)}{mu^{-2/sigma}*(gamma((2/sigma)+1)-gamma((1/sigma)+1)^2)} }

 \section{Warning }{In \code{WEI2} the estimated parameters \code{mu} and \code{sigma} can be highly correlated so it is advisable to use the 
\code{CG()} method for fitting  [as the RS() method can be veru slow in this situation.] }
\seealso{  \code{\link{gamlss.family}}, \code{\link{WEI}},\code{\link{WEI3}}, }

\examples{
WEI2()
dat<-rWEI(100, mu=.1, sigma=2)
hist(dat)
# library(gamlss)
# gamlss(dat~1, family=WEI2, method=CG())

}
\keyword{distribution}
\keyword{regression}%
