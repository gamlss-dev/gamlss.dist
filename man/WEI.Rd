\name{WEI}
\alias{WEI}
\alias{dWEI}
\alias{pWEI}
\alias{qWEI}
\alias{rWEI}


\title{ Weibull distribution for fitting a GAMLSS }
\description{
The function \code{WEI} can be used to define the Weibull distribution, a two parameter distribution, for a
\code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. [Note that the GAMLSS function \code{WEI2} uses a 
different parameterization for fitting the Weibull distribution.]
The functions \code{dWEI}, \code{pWEI}, \code{qWEI} and \code{rWEI} define the density, distribution function, quantile function and random
generation for the specific parameterization of the Weibul distribution. 
}
\usage{
WEI(mu.link = "log", sigma.link = "log")
dWEI(x, mu = 1, sigma = 1, log = FALSE)
pWEI(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qWEI(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rWEI(n, mu = 1, sigma = 1)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "log" link as the default for the mu parameter, other links are  "inverse", "identity" and "own"}
  \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter, other link is the "inverse", "identity" and "own"}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of the mu parameter }
  \item{sigma}{vector of sigma parameter }
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{

The parameterization of the function \code{WEI} is given by 
 \deqn{f(y|\mu,\sigma)=\frac{\sigma y^{\sigma-1}}{\mu^\sigma}
\hspace{1mm} \exp \left[ -\left(\frac{y }{\mu}\right)^{\sigma}
\right] }{f(y|mu,sigma)= (sigma*y^(sigma-1)/mu^sigma)exp(-(y/mu)^sigma)}
 for \eqn{y>0}, \eqn{\mu>0} and \eqn{\sigma>0}.
The GAMLSS functions \code{dWEI}, \code{pWEI}, \code{qWEI}, and \code{rWEI} can be used to provide the pdf, the cdf, the quantiles and
random generated numbers for the Weibull distribution with argument \code{mu}, and \code{sigma}.
[See the GAMLSS function \code{WEI2} for a different parameterization of the Weibull.] 

}
\value{
  \code{WEI()} returns a \code{gamlss.family} object which can be used to fit a Weibull distribution in the \code{gamlss()} function.
  \code{dWEI()} gives the density, \code{pWEI()} gives the distribution
     function, \code{qWEI()} gives the quantile function, and \code{rWEI()}
     generates random deviates. The latest functions are based on the equivalent \code{R} functions for Weibull distribution.
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}

\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Calliope Akantziliotou }


\note{ The mean in \code{WEI} is given by \eqn{ \mu \Gamma (
\frac{1}{\sigma}+1 ) }{mu*gamma((1/sigma)+1)} and the variance
\eqn{ \mu^{2} \left[\Gamma( \frac{2}{\sigma}+1 )- (\Gamma(
\frac{1}{\sigma}+1))^2\right]}{(mu^2)*(gamma((2/sigma)+1)-gamma((1/sigma)+1)^2)}
}

\seealso{  \code{\link{gamlss.family}}, \code{\link{WEI2}}, \code{\link{WEI3}}}

\examples{
WEI()
dat<-rWEI(100, mu=10, sigma=2)
# library(gamlss)
# gamlss(dat~1, family=WEI)

}
\keyword{distribution}
\keyword{regression}%
