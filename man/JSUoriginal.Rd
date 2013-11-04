\name{JSUo}
\alias{JSUo}
\alias{dJSUo}
\alias{pJSUo}
\alias{qJSUo}
\alias{rJSUo}


\title{ The original Johnson's Su distribution for fitting a GAMLSS}
\description{
This function defines the , a four parameter distribution, for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  The functions \code{dJSUo}, 
\code{pJSUo}, \code{qJSUo} and \code{rJSUo} define the density, distribution function, quantile function and random
generation for the the Johnson's Su distribution. 
}
\usage{
JSUo(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")
dJSUo(x, mu = 0, sigma = 1, nu = 0, tau = 1, log = FALSE)
pJSUo(q, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, log.p = FALSE)
qJSUo(p, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, log.p = FALSE)
rJSUo(n, mu = 0, sigma = 1, nu = 0, tau = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "inverse", "log" and "own"}
  \item{sigma.link}{  Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse", "identity" and "own"}
  \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. Other links are "inverse", "log" ans "own"}
  \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter. Other links are "inverse", "identity" and "own"}
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

  The probability density function of the orininal Jonhson's SU distribution, (\code{JSU}), is defined as 
  
\deqn{f(y|n,\mu,\sigma\,\nu,\tau)=\frac{\tau}{\sigma}\frac{1}{(z^{2}+1)^{\frac{1}{2}}}\frac{1}{\sqrt{2\pi}}\hspace{1mm} \exp{\left[ -\frac{1}{2} r^2 \right]}}{f(y|mu,sigma,nu,tau)=tau/(sigma)*(1/(z^2+1)^.5)*(1/2*Pi)^(.5)exp(-.5r^2)}

for \eqn{ -\infty < y < \infty }{0<y<0}, \eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)}, 
\eqn{\sigma>0}{sigma>0}, \eqn{\nu=(-\infty,+\infty)}{nu=(-Inf,+Inf)} and \eqn{\tau>0}{tau>0}. 
where \eqn{z = \frac{(y-\mu)}{\sigma}}{z=(y-mu)/sigma},
\eqn{r = \nu + \tau sinh^{-1}(z)}{nu + tau* asinh(z)}.


  
}
\value{
\code{JSUo()}  returns a \code{gamlss.family} object which can be used to fit a Johnson's Su distribution in the \code{gamlss()} function. 
\code{dJSUo()} gives the density, \code{pJSUo()} gives the distribution
     function, \code{qJSUo()} gives the quantile function, and \code{rJSUo()}
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

\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Bob Rigby \email{r.rigby@londonmet.ac.uk}}

\section{Warning }{The function \code{JSU} uses first derivatives square in the fitting procedure so 
standard errors should be interpreted with caution. It is recomented to be used only with \code{method=mixed(2,20)}} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{JSU}}, \code{\link{BCT}}  }
\examples{
JSU()   
plot(function(x)dJSUo(x, mu=0,sigma=1,nu=-1, tau=.5), -4, 15, 
 main = "The JSUo  density mu=0,sigma=1,nu=-1, tau=.5")
plot(function(x) pJSUo(x, mu=0,sigma=1,nu=-1, tau=.5), -4, 15, 
 main = "The JSUo  cdf mu=0, sigma=1, nu=-1, tau=.5")
# library(gamlss)
# data(abdom)
# h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=JSUo, 
#          data=abdom, method=mixed(2,20)) 
# plot(h)
}
\keyword{distribution}
\keyword{regression}% 
