\name{GIG}
\alias{GIG}
\alias{pGIG}
\alias{dGIG}
\alias{qGIG}
\alias{rGIG}

\title{ Generalized Inverse Gaussian distribution for fitting a GAMLSS }
\description{
  The function GIG defines the generalized inverse gaussian distribution, a three parameter distribution, 
for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
The functions \code{DIG}, \code{pGIG}, \code{GIG} and \code{rGIG} define the density, 
distribution function, quantile function and random generation for the specific parameterization 
of the generalized inverse gaussian distribution defined by function GIG.
}
\usage{
GIG(mu.link = "log", sigma.link = "log", 
                       nu.link = "identity")
dGIG(x, mu=1, sigma=1, nu=1,  
                      log = FALSE)
pGIG(q, mu=1, sigma=1, nu=1,  lower.tail = TRUE, 
                     log.p = FALSE)
qGIG(p, mu=1, sigma=1, nu=1,  lower.tail = TRUE, 
                     log.p = FALSE)
rGIG(n, mu=1, sigma=1, nu=1, ...)
}

\arguments{
  \item{mu.link}{Defines the mu.link, with "log" link as the default for the \code{mu} parameter, 
                                  other links are "inverse" and "identity" }
  \item{sigma.link}{Defines the sigma.link, with "log" link as the default for the \code{sigma} parameter, 
                                           other links are "inverse" and "identity" }
  \item{nu.link}{Defines the nu.link, with "identity" link as the default for the \code{nu} parameter, 
                                       other links are "inverse" and "log"}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of shape parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p). }
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x] }
  \item{p}{vector of probabilities }
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required}
 \item{\dots}{for extra arguments }

}
\details{
   The specific parameterization of the generalized inverse gaussian distribution used in GIG is 
\eqn{f(y|\mu,\sigma,\nu)=(\frac{c}{\mu})^\nu(\frac{y^(\nu-1)}{2 K(\frac{1}{\sigma},\nu)})(\exp((\frac{-1}{2\sigma})(\frac{cy}{\mu}+\frac{\mu}{cy})))}{f(y|mu,sigma,nu)=((c/mu)^nu)*(y^(nu-1))/(2*besselK(1/sigma,nu))*exp(-1/(2*sigma)*(c*y/mu+mu/(c*y)))}
where \eqn{c = \frac{K(\frac{1}{\sigma},\nu+1)}{K(\frac{1}{\sigma},\nu)}}{c = besselK(1/sigma,nu+1)/besselK(1/sigma,nu)}, 
for y>0, \eqn{\mu>0}{mu>0}, \eqn{\sigma>0}{sigma>0} and \eqn{-\infty<\nu<+\infty}{-Inf>nu>Inf}. 
}
\value{
  GIG() returns a gamlss.family object which can be used to fit a generalized inverse gaussian distribution in 
the gamlss() function. DIG() gives the density, pGIG() gives the distribution function, GIG() gives the quantile function, 
and rGIG() generates random deviates. 
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Jorgensen B. (1982) Statistical properties of the generalized inverse Gaussian distribution, Series: Lecture notes in statistics; 9, New York : Springer-Verlag. 

}
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby 
        \email{r.rigby@londonmet.ac.uk} and Nicoleta Motpan}

\seealso{ \code{\link{gamlss.family}}, \code{\link{IG}}}

\examples{
y<-rGIG(100,mu=1,sigma=1, nu=-0.5) # generates 1000 random observations 
hist(y)
# library(gamlss)
# histDist(y, family=GIG) 
}
\keyword{distribution}
\keyword{regression}%
