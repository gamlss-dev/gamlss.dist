\name{IG}
\alias{IG}
\alias{dIG}
\alias{pIG}
\alias{qIG}
\alias{rIG}

\title{Inverse Gaussian distribution for fitting a GAMLSS}
\description{
The function \code{IG()}, or equivalently \code{Inverse.Gaussian()}, defines the inverse Gaussian distribution, 
a two parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}.
The functions \code{dIG}, \code{pIG}, \code{qIG} and \code{rIG} define the density, distribution function, quantile function and random
generation for the specific parameterization of the Inverse Gaussian distribution defined by function \code{IG}. 
}
\usage{
IG(mu.link = "log", sigma.link = "log")
dIG(x, mu = 1, sigma = 1, log = FALSE)
pIG(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qIG(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rIG(n, mu = 1, sigma = 1, ...)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter }
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter }
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}         
   \item{\dots}{\code{\dots} can be used to pass the uppr.limit argument to \code{qIG}  }         
}
\details{
 Definition file for inverse Gaussian distribution. 
\deqn{f(y|\mu,\sigma)= \frac{1}{\sqrt{2 \pi \sigma^2  y^3}} \hspace{1mm}
\exp\left\{-\frac{1}{2 \mu^2 \sigma^2
y}\hspace{1mm}(y-\mu)^2\right\}}{f(y|mu,sigma)=(1/(sqrt(2*pi*sigma^2*y^3))* exp(-(y-mu)^2/(2*mu^2*sigma^2*y))} 
 for \eqn{y>0}, \eqn{\mu>0} and \eqn{\sigma>0}.
}
\value{
 returns a \code{gamlss.family} object which can be used to fit a inverse Gaussian distribution in the \code{gamlss()} function.  
}
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}) 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}

\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou }

\note{\eqn{\mu}{mu} is the mean and \eqn{\sigma^2 \mu^3}{(sigma^2)*(mu^3)} is the variance of the inverse Gaussian}


\seealso{ \code{\link{gamlss.family}} ,  \code{\link{GA}},  \code{\link{GIG}}}
\examples{
IG()# gives information about the default links for the normal distribution
# library(gamlss)
# data(rent)        
# gamlss(R~cs(Fl),family=IG, data=rent) # 
plot(function(x)dIG(x, mu=1,sigma=.5), 0.01, 6, 
 main = "{Inverse Gaussian  density mu=1,sigma=0.5}")
plot(function(x)pIG(x, mu=1,sigma=.5), 0.01, 6, 
 main = "{Inverse Gaussian  cdf mu=1,sigma=0.5}")
}
\keyword{distribution}
\keyword{regression}% 
