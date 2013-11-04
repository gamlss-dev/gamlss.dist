\name{GA} 

\alias{GA}
\alias{dGA}
\alias{qGA}
\alias{pGA}
\alias{rGA}

\title{Gamma distribution for fitting a GAMLSS}

\description{ The function \code{GA} defines the gamma distribution, a two parameter distribution, for a
\code{gamlss.family} object to be used in GAMLSS fitting using the
function \code{gamlss()}. The parameterization used has the mean of the distribution equal to \eqn{\mu}{mu} and the variance equal to 
\eqn{\sigma^2 \mu^2}{(sigma^2)*(mu^2)}. 
The functions \code{dGA}, \code{pGA}, \code{qGA} and \code{rGA} define the density, distribution function, quantile function and random
generation for the specific parameterization of the gamma distribution defined by function \code{GA}. 
}


\usage{
GA(mu.link = "log", sigma.link ="log")
dGA(x, mu = 1, sigma = 1, log = FALSE) 
pGA(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qGA(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rGA(n, mu = 1, sigma = 1)
}
\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter, other links are  "inverse", "identity" ans "own"}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter, other link is the "inverse", "identity" and "own"}
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
  The specific parameterization of the gamma distribution used in \code{GA} is 
 \deqn{f(y|\mu,\sigma)=\frac{y^{(1/\sigma^2-1)}\exp[-y/(\sigma^2 \mu)]}{(\sigma^2 \mu)^{(1/\sigma^2)} \Gamma(1/\sigma^2)}}{f(y|mu,sigma)=(y^((1/sigma^2)-1)*exp[-y/((sigma^2)*mu)])/((sigma^2*mu)^(1/sigma^2) Gamma(1/sigma^2)) }
 for \eqn{y>0}, \eqn{\mu>0} and \eqn{\sigma>0}.
}
\value{
  \code{GA()} returns a \code{gamlss.family} object which can be used to fit a gamma distribution in the \code{gamlss()} function.
  \code{dGA()} gives the density, \code{pGA()} gives the distribution
     function, \code{qGA()} gives the quantile function, and \code{rGA()}
     generates random deviates. The latest functions are based on the equivalent \code{R} functions for gamma distribution.
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

\note{ \eqn{\mu}{mu} is the mean of the distribution in  \code{GA}.  In the function \code{GA}, \eqn{\sigma}{sigma} is the square root of the 
usual dispersion parameter for a GLM gamma model. Hence  \eqn{\sigma \mu}{sigma*mu} is the standard deviation of the distribution defined in \code{GA}.  }



\seealso{ \code{\link{gamlss.family}}}

\examples{
GA()# gives information about the default links for the gamma distribution      
# dat<-rgamma(100, shape=1, scale=10) # generates 100 random observations 
# fit a gamlss model
# gamlss(dat~1,family=GA) 
# fits a constant for each parameter mu and sigma of the gamma distribution
newdata<-rGA(1000,mu=1,sigma=1) # generates 1000 random observations
hist(newdata) 
rm(dat,newdata)
}
\keyword{distribution}
\keyword{regression}% 
