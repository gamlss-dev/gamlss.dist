\name{NO}

\alias{NO}
\alias{dNO}
\alias{pNO}
\alias{qNO}
\alias{rNO}


\title{Normal distribution for fitting a GAMLSS}
\description{
The function \code{NO()} defines the normal distribution, a two parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}, with mean equal to the parameter \code{mu}
and \code{sigma} equal the standard deviation.  
The functions \code{dNO}, \code{pNO}, \code{qNO} and \code{rNO} define the density, distribution function, quantile function and random
generation for the \code{NO} parameterization of the normal distribution.  
[A alternative parameterization with \code{sigma} equal to the variance is given in the function \code{NO2()}]   
}
\usage{
NO(mu.link = "identity", sigma.link = "log")
dNO(x, mu = 0, sigma = 1, log = FALSE)
pNO(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qNO(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rNO(n, mu = 0, sigma = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
  }
\details{
  The parametrization of the normal distribution given in the function \code{NO()} is 
 \deqn{f(y|\mu,\sigma)=\frac{1}{\sqrt{2 \pi }\sigma}\exp \left[-\frac{1}{2}(\frac{y-\mu}{\sigma})^2\right]}{f(y|mu,sigma)=(1/(sqrt(2*pi)*sigma))* exp(-0.5*((y-mu)/sigma)^2)} 
 
 for \eqn{y=(-\infty,\infty)}{y=(-Inf,+Inf)}, \eqn{\mu=(-\infty,+\infty)}{\mu=(-Inf,+Inf)} and \eqn{\sigma>0}. 
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a normal distribution in the \code{gamlss()} function. 
    
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }

\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}

\note{ For the function \code{NO()}, \eqn{\mu}{mu} is the mean and \eqn{\sigma}{sigma} is the standard deviation (not the variance) of the normal distribution. 
       }


\seealso{  \code{\link{gamlss.family}}, \code{\link{NO2}}}

\examples{
NO()# gives information about the default links for the normal distribution
plot(function(y) dNO(y, mu=10 ,sigma=2), 0, 20)
plot(function(y) pNO(y, mu=10 ,sigma=2), 0, 20)
plot(function(y) qNO(y, mu=10 ,sigma=2), 0, 1)
dat<-rNO(100)
hist(dat)
# library(gamlss)        
# gamlss(dat~1,family=NO) # fits a constant for mu and sigma 
}
\keyword{distribution}
\keyword{regression}
