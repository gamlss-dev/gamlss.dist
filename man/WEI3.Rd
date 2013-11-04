\name{WEI3}
\alias{WEI3}
\alias{dWEI3}
\alias{pWEI3}
\alias{qWEI3}
\alias{rWEI3}


\title{A specific parameterization of the  Weibull
distribution for fitting a GAMLSS}
\description{
The function \code{WEI3} can be used to define the Weibull distribution, a two parameter distribution, for a
\code{\link{gamlss.family}} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
This is a parameterization of the Weibull distribution where \eqn{\mu}{mu} is the mean of the distribution.
[Note that the GAMLSS functions \code{\link{WEI}} and \code{\link{WEI2}} use  
different parameterizations for fitting the Weibull distribution.]
The functions \code{dWEI3}, \code{pWEI3}, \code{qWEI3} and \code{rWEI3} define the density, distribution function, quantile function and random
generation for the specific parameterization of the Weibull distribution. 
}
\usage{
WEI3(mu.link = "log", sigma.link = "log")
dWEI3(x, mu = 1, sigma = 1, log = FALSE)
pWEI3(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qWEI3(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rWEI3(n, mu = 1, sigma = 1)
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
 The parameterization of the function \code{WEI3} is given by
 \deqn{f(y|\mu,\sigma)= \frac{\sigma}{\beta}  \left(\frac{y}{\beta}\right)^{\sigma-1} e^{-\left(\frac{y}{\beta}\right)^{\sigma}}}{f(y|mu,sigma)= (sigma/beta)*(y/beta)^(sigma-1)exp(-(y/beta)^sigma)}
 where \eqn{\beta=\frac{\mu}{\Gamma((1/\sigma)+1)}}{mu/(Gamma((1/sigma)+1))} for \eqn{y>0}, \eqn{\mu>0} and \eqn{\sigma>0}.
The GAMLSS functions \code{dWEI3}, \code{pWEI3}, \code{qWEI3}, and \code{rWEI3} can be used to provide the pdf, the cdf, the quantiles and
random generated numbers for the Weibull distribution with argument \code{mu}, and \code{sigma}.
[See the GAMLSS function \code{WEI} for a different parameterization of the Weibull.] 
}
\value{
  \code{WEI3()} returns a \code{\link{gamlss.family}} object which can be used to fit a Weibull distribution in the \code{gamlss()} function.
  \code{dWEI3()} gives the density, \code{pWEI3()} gives the distribution
     function, \code{qWEI3()} gives the quantile function, and \code{rWEI3()}
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

\author{ Bob Rigby \email{r.rigby@londonmet.ac.uk} and Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}}

\note{ The mean in \code{WEI3} is given by \eqn{\mu}{mu} and the variance 
\eqn{\mu^2 \left\{\Gamma(2/\sigma +1)/ \left[ \Gamma(1/\sigma +1)\right]^2 -1\right\}}{mu^2(Gamma(1/sigma +1)/Gamma(1/sigma+1)^2 -1)}
}
 \section{Warning }{In \code{WEI3} the estimated parameters \code{mu} and \code{sigma} can be highly correlated so it is advisable to use the 
\code{CG()} method for fitting  [as the RS() method can be very slow in this situation.] }
\seealso{ \code{\link{gamlss.family}}, \code{\link{WEI}},  \code{\link{WEI2}}}

\examples{
WEI3()
dat<-rWEI(100, mu=.1, sigma=2)
# library(gamlss)
# gamlss(dat~1, family=WEI3, method=CG())

}
\keyword{distribution}
\keyword{regression}%
