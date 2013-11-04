\name{NOF}

\alias{NOF}
\alias{dNOF}
\alias{pNOF}
\alias{qNOF}
\alias{rNOF}


\title{Normal distribution family for fitting a GAMLSS}
\description{
The function \code{NOF()} defines a normal distribution family, which has three parameters.  The distribution
can be used  in a GAMLSS fitting using the function \code{gamlss()}.
The  mean of \code{NOF} is equal to \code{mu}. The variance is equal to \code{sigma^2*mu^nu} so
the standard deviation is  \code{sigma*mu^(nu/2)}. The function is design for cases where the
variance is proportional to a power of the mean.
The functions \code{dNOF}, \code{pNOF}, \code{qNOF} and \code{rNOF} define the density, distribution function,
quantile function and random
generation for the \code{NOF} parametrization of the normal distribution family.
}
\usage{
NOF(mu.link = "identity", sigma.link = "log", nu.link = "identity")
dNOF(x, mu = 0, sigma = 1, nu = 0, log = FALSE)
pNOF(q, mu = 0, sigma = 1, nu = 0, lower.tail = TRUE, log.p = FALSE)
qNOF(p, mu = 0, sigma = 1, nu = 0, lower.tail = TRUE, log.p = FALSE)
rNOF(n, mu = 0, sigma = 1, nu = 0)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link} with "identity" link as the default for the nu parameter}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of power parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
  }
\details{
  The parametrization of the normal distribution given in the function \code{NOF()} is 
\deqn{f(y|\mu,\sigma, \nu)=\frac{1}{\sqrt{2 \pi }\sigma \mu^{\nu/2}}\exp \left[-\frac{1}{2}\frac{(y-\mu)^2}{\sigma^2 \mu^\nu}\right]}{f(y|mu,sigma,nu)=(1/(sqrt(2*pi)*sigma*mu^(nu/2)))* exp(-0.5*((y-mu)^2/sigma^2*mu^nu))}

 for \eqn{y=(-\infty,\infty)}{y=(-Inf,Inf)}, \eqn{\mu=(-\infty,\infty)}{\mu=(-Inf,Inf)}, \eqn{\sigma>0} and 
 \eqn{\nu=(-\infty,+\infty)}{\nu=(-Inf,+Inf)}. 
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a normal distribution family 
  in the \code{gamlss()} function. 
    
}
\references{


Davidian, M. and  Carroll,  R. J. (1987), Variance Function Estimation, 
\emph{Journal of the American Statistical Association}, Vol. \bold{82}, pp. 1079-1091

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }

\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}

\note{ For the function \code{NOF()}, \eqn{\mu}{mu} is the mean and \eqn{\sigma \mu^{\nu/2}}{sigma*mu^(nu/2)}
is the standard deviation of the normal distribution family.
The NOF is design for fitting regression type models where the variance is proportional to a power ofthe mean.
Models of this type are related to the "pseudo likelihood" models of Carroll and Rubert (1987) but here a proper
likelihood is miximised.

Note that because the high correlation between the \code{sigma} and the \code{nu} parameter the \code{mixed()}
method should be used in the fitting.
       }


\seealso{  \code{\link{gamlss.family}}, \code{\link{NO}}, \code{\link{NO2}}}

\examples{
NOF()# gives information about the default links for the normal distribution family
# library(gamlss)
#data(abdom)        
## the normal distribution fit with constant sigma
#m1<-gamlss(y~poly(x,2), sigma.fo=~1, family=NO, data=abdom)
## the normal family fit with variance proportional to mu
#m2<-gamlss(y~poly(x,2), sigma.fo=~1, family=NOF, data=abdom, method=mixed(1,20))
## a nornal distribution fit with variance as a function of x
#m3 <-gamlss(y~poly(x,2), sigma.fo=~x,   family=NO, data=abdom, method=mixed(1,20)) 
#GAIC(m1,m2,m3)
}
\keyword{distribution}
\keyword{regression}
