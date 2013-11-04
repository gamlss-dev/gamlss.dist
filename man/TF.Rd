\name{TF}
\alias{TF}
\alias{dTF}
\alias{pTF}
\alias{qTF}
\alias{rTF}
\alias{TF2}
\alias{dTF2}
\alias{pTF2}
\alias{qTF2}
\alias{rTF2}

\title{t family distribution for fitting a GAMLSS}
\description{
The function \code{TF} defines the t-family distribution, a three parameter distribution,
 for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
 The functions \code{dTF}, \code{pTF}, \code{qTF} and \code{rTF} define the density, distribution function, quantile function and random
generation for the specific parameterization of the t distribution given in details below, with mean equal to \eqn{\mu}{mu}
and standard deviation equal to \eqn{\sigma (\frac{\nu}{\nu-2})^{0.5}}{sigma*(nu/(nu-2))^0.5} with the degrees of freedom \eqn{\nu}{nu}  
The function \code{TF2} is a different parametrization where \code{sigma} is the standard deviation. 

}
\usage{
TF(mu.link = "identity", sigma.link = "log", nu.link = "log")
dTF(x, mu = 0, sigma = 1, nu = 10, log = FALSE)
pTF(q, mu = 0, sigma = 1, nu = 10, lower.tail = TRUE, log.p = FALSE)
qTF(p, mu = 0, sigma = 1, nu = 10, lower.tail = TRUE, log.p = FALSE) 
rTF(n, mu = 0, sigma = 1, nu = 10)

TF2(mu.link = "identity", sigma.link = "log", nu.link = "logshiftto2")
dTF2(x, mu = 0, sigma = 1, nu = 10, log = FALSE)
pTF2(q, mu = 0, sigma = 1, nu = 10, lower.tail = TRUE, log.p = FALSE)
qTF2(p, mu = 0, sigma = 1, nu = 10, lower.tail = TRUE, log.p = FALSE)
rTF2(n, mu = 0, sigma = 1, nu = 10)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link}, with "log" link as the default for the nu parameter}
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of the degrees of freedom parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
  Definition file for t family distribution. 
\deqn{f(y|\mu,\sigma, \nu)=\frac{\Gamma((\nu+1)/2)}{\sigma \Gamma(1/2) \Gamma(\nu/2) \nu^{0.5}} \left[1+\frac{(y-\mu)^2}{\nu \sigma^2}\right]^{-(\nu+1)/2}}{f(y|mu,sigma,nu)=((Gamma((nu+1)/2)/(sigma*Gamma(1/2)*Gamma(nu/2))*nu^0.5 )*(1+(y-mu)^2/(nu*sigma^2))^(-(nu+1)/2)} 
 \eqn{y=(-\infty,+\infty)}{y=(-Inf,+Inf)}, \eqn{\mu=(-\infty,+\infty)}{\mu=(-Inf,+Inf)}, \eqn{\sigma>0} and \eqn{\nu>0}. 
Note that \eqn{z=(y-\mu)/\sigma}{z=(y-mu)/sigma} has a standard t distribution with degrees of freedom \eqn{\nu}{nu}.
}
\value{
 \code{TF()} returns a \code{gamlss.family} object which can be used to fit a t distribution in the \code{gamlss()} function.
  \code{dTF()} gives the density, \code{pTF()} gives the distribution
     function, \code{qTF()} gives the quantile function, and \code{rTF()}
     generates random deviates. The latest functions are based on the equivalent \code{R} functions for gamma distribution.
}
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Kalliope Akantziliotou }
\note{\eqn{\mu}{mu} is the mean and \eqn{\sigma [\nu/(\nu-2)]^{0.5}}{sigma*(nu/(nu-2))^0.5} is the standard deviation of the t family distribution.
\eqn{\nu>0}{nu>0} is a positive real valued parameter. 
}

\seealso{  \code{\link{gamlss.family}}}

\examples{
TF()# gives information about the default links for the t-family distribution 
# library(gamlss)
#data(abdom)
#h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=TF, data=abdom) # fits 
#plot(h)
newdata<-rTF(1000,mu=0,sigma=1,nu=5) # generates 1000 random observations
hist(newdata) 
}
\keyword{distribution}
\keyword{regression}% 
