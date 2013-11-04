\name{BCT}
\alias{BCT}
\alias{dBCT}
\alias{pBCT}
\alias{qBCT}
\alias{rBCT}
\alias{BCTuntr}
\alias{BCTo}


\title{Box-Cox t distribution for fitting a GAMLSS}
\description{
The function \code{BCT()}  defines the Box-Cox t distribution, a four parameter distribution, 
for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. The functions \code{dBCT}, 
\code{pBCT}, \code{qBCT} and \code{rBCT} define the density, distribution function, quantile function and random
generation for the Box-Cox t distribution.  
[The function \code{BCTuntr()} is the original version of the function suitable only for the untruncated BCT distribution]. 
 See Rigby and Stasinopoulos (2003) for details. 
The function \code{BCT} is identical to \code{BCT} but with log link for mu.
}
\usage{
BCT(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
          tau.link = "log")
BCTo(mu.link = "log", sigma.link = "log", nu.link = "identity", 
          tau.link = "log")
BCTuntr(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
          tau.link = "log")
dBCT(x, mu = 5, sigma = 0.1, nu = 1, tau = 2, log = FALSE)
pBCT(q, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE, log.p = FALSE)
qBCT(p, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE, log.p = FALSE)          
rBCT(n, mu = 5, sigma = 0.1, nu = 1, tau = 2)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "inverse", "log" and "own"}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse","identity", "own"}
  \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. Other links are "inverse", "log", "own"}
  \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter. Other links are "inverse", "identity" and "own"}
  \item{x,q}{vector of quantiles }
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of \code{nu} parameter values}
  \item{tau}{vector of \code{tau} parameter values}
   \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}

\details{
  The probability density function of the untruncated Box-Cox t distribution, \code{BCTuntr}, is given by 
 \deqn{f(y|\mu,\sigma,\nu,\tau)=\frac{y^{\nu-1}}{\mu^{\nu}\sigma} \frac{\Gamma[(\tau+1)/2]}{\Gamma(1/2) \Gamma(\tau/2) \tau^{0.5}} [1+(1/\tau)z^2]^{-(\tau+1)/2}}{f(y|mu,sigma,nu,tau)=(1/(y*sigma))*(\Gamma((tau+1)/2)/(Gamma(1/2)*Gamma(tau/2)*tau^0.5))*(1+z^2/tau)^(-(tau+1)/2)} 
 where if \eqn{\nu \neq 0}{\nu!=0} then  \eqn{z=[(y/\mu)^{\nu}-1]/(\nu \sigma)}{z=[(y/mu)^(nu)-1]/(nu*sigma)} else \eqn{z=\log(y/\mu)/\sigma}{z=log(y/\mu)/\sigma}, 
 for \eqn{y>0}, \eqn{\mu>0}, \eqn{\sigma>0}, \eqn{\nu=(-\infty,+\infty)}{\nu=(-Inf,+Inf)} and \eqn{\tau>0}. 

The Box-Cox \emph{t} distribution,   \code{BCT}, adjusts the above density \eqn{f(y|\mu,\sigma,\nu,\tau)}{f(y|mu,sigma,nu,tau} for the 
truncation resulting from the condition \eqn{y>0}{y>0}. See Rigby and Stasinopoulos (2003) for details.   

}
\value{
 \code{BCT()}  returns a \code{gamlss.family} object which can be used to fit a Box Cox-t distribution in the \code{gamlss()} function. 
  \code{dBCT()} gives the density, \code{pBCT()} gives the distribution
     function, \code{qBCT()} gives the quantile function, and \code{rBCT()}
     generates random deviates. 

}
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.
  
Rigby, R.A. Stasinopoulos, D.M. (2006).   Using the Box-Cox \emph{t} distribution in GAMLSS to mode skewnees and  and kurtosis.
to appear in \emph{Statistical Modelling}.

Stasinopoulos, D. M. Rigby, R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).
 
Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.  
}

\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}
\note{ \eqn{\mu}{mu} is the median of the distribution, \eqn{\sigma(\frac{\tau}{\tau-2})^{0.5}}{sigma*(tau/(tau-2))^0.5} 
is approximate the coefficient of variation (for small \eqn{\sigma}{sigma} and moderate \code{nu>0} and moderate or large \eqn{\tau}{tau}), 
\eqn{\nu}{nu} controls the skewness and \eqn{\tau}{tau} the kurtosis of the distribution 
  }

\section{Warning }{ The use \code{BCTuntr} distribution may be unsuitable for some combinations of the parameters (mainly for large \eqn{\sigma}{sigma}) 
where the integrating constant is less than 0.99. A warning will be given if this is the case.

The \code{BCT} distribution is suitable for all combinations of the parameters within their ranges 
[i.e. \eqn{\mu>0,\sigma>0, \nu=(-\infty,\infty) {\rm and} \tau>0}{mu>0, sigma>0, nu=(-Inf,+Inf) and tau>0} ]

} 

\seealso{  \code{\link{gamlss.family}}, \code{\link{BCPE}}, \code{\link{BCCG}}}

\examples{
BCT()   # gives information about the default links for the Box Cox t distribution
# library(gamlss)
#data(abdom)
#h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=BCT, data=abdom) # 
#plot(h)
plot(function(x)dBCT(x, mu=5,sigma=.5,nu=1, tau=2), 0.0, 20, 
 main = "The BCT  density mu=5,sigma=.5,nu=1, tau=2")
plot(function(x) pBCT(x, mu=5,sigma=.5,nu=1, tau=2), 0.0, 20, 
 main = "The BCT  cdf mu=5, sigma=.5, nu=1, tau=2")
}
\keyword{distribution}
\keyword{regression}% 
