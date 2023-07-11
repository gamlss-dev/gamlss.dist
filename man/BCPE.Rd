\name{BCPE}



\alias{BCPE}
\alias{dBCPE}
\alias{pBCPE}
\alias{qBCPE}
\alias{rBCPE}
\alias{BCPEuntr}
\alias{checkBCPE}
\alias{BCPEo}
\alias{dBCPEo}
\alias{pBCPEo}
\alias{qBCPEo}
\alias{rBCPEo}

\title{Box-Cox Power Exponential distribution for fitting a GAMLSS}
\description{
This function defines the Box-Cox Power Exponential distribution, a four parameter distribution, for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  

The functions \code{dBCPE}, 
\code{pBCPE}, \code{qBCPE} and \code{rBCPE} define the density, distribution function, quantile function and random
generation for the Box-Cox Power Exponential distribution. 

The function \code{checkBCPE} (very old) can be used, typically when a BCPE model is fitted, to check whether there exit a turning point 
of the distribution close to zero. It give the number of values of the response below their minimum turning point and also 
the maximum probability of the lower tail below minimum turning point.  

 [The function \code{Biventer()} is the original version of the function suitable only for the untruncated BCPE distribution.] See Rigby and Stasinopoulos (2003) for details.
 
The function \code{BCPEo} is identical to \code{BCPE} but with log link for mu.    
}
\usage{
BCPE(mu.link = "identity", sigma.link = "log", nu.link = "identity",
          tau.link = "log")
BCPEo(mu.link = "log", sigma.link = "log", nu.link = "identity", 
          tau.link = "log")
BCPEuntr(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
          tau.link = "log")
dBCPE(x, mu = 5, sigma = 0.1, nu = 1, tau = 2, log = FALSE)
pBCPE(q, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE, log.p = FALSE)
qBCPE(p, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE, log.p = FALSE)          
rBCPE(n, mu = 5, sigma = 0.1, nu = 1, tau = 2)
dBCPEo(x, mu = 5, sigma = 0.1, nu = 1, tau = 2, log = FALSE)
pBCPEo(q, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE, 
       log.p = FALSE)
qBCPEo(p, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE, 
       log.p = FALSE)          
rBCPEo(n, mu = 5, sigma = 0.1, nu = 1, tau = 2)
checkBCPE(obj = NULL, mu = 10, sigma = 0.1, nu = 0.5, tau = 2,...)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "inverse", "log" and "own"}
  \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse", "identity" and "own"}
  \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. Other links are "inverse", "log" and "own"}
  \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter. Other links are "logshifted", "identity" and "own"}
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values }
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of \code{nu} parameter values}
  \item{tau}{vector of \code{tau} parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
  \item{obj}{a gamlss BCPE family object  }
  \item{\dots}{for extra arguments}
  }
\details{
  The probability density function of the untrucated Box Cox Power Exponential distribution, (\code{BCPE.untr}), is defined as 
  
\deqn{f(y|\mu,\sigma,\nu,\tau)=\frac{y^{\nu-1} \tau \exp[-\frac{1}{2}|\frac{z}{c}|^\tau]}{\mu^{\nu} \sigma c  2^{(1+1/\tau)} \Gamma(\frac{1}{\tau})}}{f(y|mu,sigma,nu,tau)=(y^(nu-1)/mu^nu)*(tau/sigma)*exp(-0.5*|z/c|^tau)/(c*2^(1+1/tau)*Gamma(1/tau))}

where \eqn{c = [ 2^{(-2/\tau)}\Gamma(1/\tau)/\Gamma(3/\tau)]^{0.5}}{c=[2^(-2 /tau)*Gamma(1/tau)/Gamma(3/tau)]^0.5},
where if \eqn{\nu \neq 0}{nu!=0} then \eqn{z=[(y/\mu)^{\nu}-1]/(\nu \sigma)}{z=[(y/mu)^(nu)-1]/(nu*sigma)}
else \eqn{z=\log(y/\mu)/\sigma}{z=log(y/mu)/sigma}, 
for \eqn{y>0}{y>0}, \eqn{\mu>0}, \eqn{\sigma>0}, \eqn{\nu=(-\infty,+\infty)}{nu=(-Inf,+Inf)} and \eqn{\tau>0} see pp. 450-451 of Rigby et al. (2019).  

The Box-Cox Power Exponential, \code{BCPE}, adjusts the above density \eqn{f(y|\mu,\sigma,\nu,\tau)}{f(y|mu,sigma,nu,tau} for the 
truncation resulting from the condition \eqn{y>0}{y>0}. See Rigby and Stasinopoulos (2003) for details.   
}
\value{
 \code{BCPE()}  returns a \code{gamlss.family} object which can be used to fit a Box Cox Power Exponential distribution in the \code{gamlss()} function. 
  \code{dBCPE()} gives the density, \code{pBCPE()} gives the distribution
     function, \code{qBCPE()} gives the quantile function, and \code{rBCPE()}
     generates random deviates. 

}

\references{ 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A. and Stasinopoulos, D. M. (2004). Smooth centile curves for skew and kurtotic data modelled using the Box-Cox Power
Exponential distribution. \emph{Statistics in Medicine}, \bold{23}: 3053-3076.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}.
	An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.


Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}

\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}

\note{\eqn{\mu}{mu}, is the median of the distribution, \eqn{\sigma}{sigma} is approximately the coefficient of variation (for small \eqn{\sigma}{sigma} and moderate \code{nu>0}), 
 \eqn{\nu}{nu} controls the skewness and \eqn{\tau}{tau} the kurtosis of the distribution }

\section{Warning }{The \code{BCPE.untr} distribution may be unsuitable for some combinations of the parameters (mainly for large \eqn{\sigma}{sigma}) 
where the integrating constant is less than 0.99. A warning will be given if this is the case.

The \code{BCPE} distribution is suitable for all combinations of the parameters within their ranges [i.e. \eqn{\mu>0,\sigma>0, \nu=(-\infty,\infty) {\rm and} \tau>0}{mu>0, sigma>0, nu=(-Inf,+Inf) and tau>0} ]
} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{BCT}} }

\examples{
# BCPE()   #
# library(gamlss) 
# data(abdom)
#h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=BCPE, data=abdom) 
#plot(h)
plot(function(x)dBCPE(x, mu=5,sigma=.5,nu=1, tau=3), 0.0, 15, 
 main = "The BCPE  density mu=5,sigma=.5,nu=1, tau=3")
plot(function(x) pBCPE(x, mu=5,sigma=.5,nu=1, tau=3), 0.0, 15, 
 main = "The BCPE  cdf mu=5, sigma=.5, nu=1, tau=3")
}
\keyword{distribution}
\keyword{regression}% 
