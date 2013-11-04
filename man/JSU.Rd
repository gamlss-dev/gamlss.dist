\name{JSU}
\alias{JSU}
\alias{dJSU}
\alias{pJSU}
\alias{qJSU}
\alias{rJSU}


\title{ The Johnson's Su distribution for fitting a GAMLSS}
\description{
This function defines the , a four parameter distribution, for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  The functions \code{dJSU}, 
\code{pJSU}, \code{qJSU} and \code{rJSU} define the density, distribution function, quantile function and random
generation for the the Johnson's Su distribution. 
}
\usage{
JSU(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")
dJSU(x, mu = 0, sigma = 1, nu = 1, tau = 0.5, log = FALSE)
pJSU(q, mu = 0, sigma = 1, nu = 1, tau = 0.5, lower.tail = TRUE, log.p = FALSE)
qJSU(p, mu = 0, sigma = 1, nu = 0, tau = 0.5, lower.tail = TRUE, log.p = FALSE)
rJSU(n, mu = 0, sigma = 1, nu = 0, tau = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "inverse" "log" ans "own"}
  \item{sigma.link}{  Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse", "identity" ans "own"}
  \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. Other links are "onverse", "log" and "own"}
  \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter. Other links are "onverse", "identity" ans "own"}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values }
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of skewness \code{nu} parameter values}
  \item{tau}{vector of kurtosis \code{tau} parameter values}
   \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
  
\details{

  The probability density function of the Jonhson's SU distribution, (\code{JSU}), is defined as 
  
\deqn{f(y|n,\mu,\sigma\,\nu,\tau)==\frac{1}{c \sigma} \frac{1}{\tau(z^2+1)^{\frac{1}{2}}} \frac{1}{\sqrt{2\pi}} \hspace{1mm} \exp{\left[ -\frac{1}{2} r^2 \right]}}{f(y|mu,sigma,nu,tau)=1/(c*sigma*2*pi)*(1/(tau*(z^2+1)^.5))exp(-.5r^2)}

for \eqn{ -\infty < y < \infty }{0<y<0}, \eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)}, 
\eqn{\sigma>0}{sigma>0}, \eqn{\nu=(-\infty,+\infty)}{nu=(-Inf,+Inf)} and \eqn{\tau>0}{tau>0}. 
where \eqn{r=-\nu+\frac{1}{\tau} \sinh^{-1}(z)}{r=-nu+sinh^-1(z)/tau}, \eqn{z = \frac{y-(\mu+c\sigma w^{\frac{1}{2}}\sinh{\Omega})}{c\sigma}}{z=(y-(mu+c*sigma*w^(.5) sinh(omega)))/(c*sigma)},
\eqn{c = [ \frac{1}{2}(w-1)(w \cosh{2 \Omega} +1) ]^{\frac{1}{2}}}{c=(.5*(w-1)*(w*cosh(2*omega)+1))^(-.5)},
\eqn{w=e^{\tau^2}}{w=exp{tau^2}} and \eqn{\Omega = -\nu\tau}{ omega= -nu*tau}.

This is a reparameterization of the original Johnson Su distribution, Johnson (1954), 
so the parameters \code{mu} and \code{sigma} are the mean and the standard deviation of the distribution.
The parameter \code{nu} determines the skewness of the distribution with \code{nu>0} indicating positive skewness and
\code{nu<0} negative. The parameter \code{tau}  determines the kurtosis of the distribution. \code{tau} should be positive and most likely in the region
from zero to 1. As \code{tau} goes to  0 (and for \code{nu=0}) the distribution approaches  the the Normal density function.
The distribution is appropriate for leptokurtic data that is data with kurtosis larger that the Normal distribution one.

  
}
\value{
\code{JSU()}  returns a \code{gamlss.family} object which can be used to fit a Johnson's Su distribution in the \code{gamlss()} function. 
\code{dJSU()} gives the density, \code{pJSU()} gives the distribution
     function, \code{qJSU()} gives the quantile function, and \code{rJSU()}
     generates random deviates. 
}
\references{
Johnson, N. L. (1954). Systems of frequency curves derived from the first law of Laplace.,
    \emph{Trabajos de Estadistica}, \bold{5}, 283-291.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}

\author{Bob Rigby and Mikis Stasinopoulos}


\section{Warning }{The function \code{JSU} uses first derivatives square in the fitting procedure so 
standard errors should be interpreted with caution} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{JSUo}}, \code{\link{BCT}}  }
\examples{
JSU()   
plot(function(x)dJSU(x, mu=0,sigma=1,nu=-1, tau=.5), -4, 4, 
 main = "The JSU  density mu=0,sigma=1,nu=-1, tau=.5")
plot(function(x) pJSU(x, mu=0,sigma=1,nu=-1, tau=.5), -4, 4, 
 main = "The JSU  cdf mu=0, sigma=1, nu=-1, tau=.5")
# library(gamlss)
# data(abdom) 
# h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=JSU, data=abdom) 
}
\keyword{distribution}
\keyword{regression}% 
