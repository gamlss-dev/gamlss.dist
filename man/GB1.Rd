\name{GB1}
\alias{GB1}
\alias{dGB1}
\alias{pGB1}
\alias{qGB1}
\alias{rGB1}



\title{The generalized Beta type 1 distribution for fitting a GAMLSS}
\description{
This function defines the generalized beta  type 1 distribution, a four parameter distribution.
The function \code{GB1} creates a \code{gamlss.family} object which can be used to fit the distribution using the function 
\code{gamlss()}. Note the range of the response variable is from zero to one. 
The functions \code{dGB1}, 
\code{GB1}, \code{qGB1} and \code{rGB1} define the density, 
distribution function, quantile function and random
generation for the generalized beta type 1 distribution.   
}
\usage{
GB1(mu.link = "logit", sigma.link = "logit", nu.link = "log", 
      tau.link = "log")
dGB1(x, mu = 0.5, sigma = 0.4, nu = 1, tau = 1, log = FALSE)
pGB1(q, mu = 0.5, sigma = 0.4, nu = 1, tau = 1, lower.tail = TRUE, 
       log.p = FALSE)
qGB1(p, mu = 0.5, sigma = 0.4, nu = 1, tau = 1, lower.tail = TRUE, 
       log.p = FALSE)
rGB1(n, mu = 0.5, sigma = 0.4, nu = 1, tau = 1)
}
%
\arguments{
 \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. }
 \item{sigma.link}{  Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. }
 \item{nu.link}{Defines the  \code{nu.link}, with "log" link as the default for the \code{nu} parameter. }
 \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter.}
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
 The probability density function of the Generalized Beta type 1, (\code{GB1}), is defined as 
  
\deqn{f(y|\mu,\sigma\,\nu,\tau)= \frac{\tau \nu^\beta y^{\tau\alpha-1} (1-y^\tau)^{\beta-1}}{B(\alpha,\beta)[\nu+(1-\nu) y^\tau]^{\alpha+\beta}}}{f(y|mu,sigma,nu,tau)=(tau*nu^beta*y^(tau*alpha-1)(1-y^tau))^(beta-1)/(Beta(alpha,beta)*(nu+(1-nu)*y^tau))^(alpha*beta))}

where  \eqn{ 0 < y < 1 }{0<y<1}, \eqn{\alpha = \mu(1-\sigma^2)/\sigma^2}{alpha=mu*(1-sigma^2)/sigma^2}
and \eqn{\beta=(1-\mu)(1-\sigma^2)/\sigma^2}{(1-mu)*(1-sigma^2)/sigma^2}, and 
\eqn{\alpha>0}{alpha>0}, \eqn{\beta>0}{beta>0}. Note the \eqn{\mu=\alpha /(\alpha+\beta)}{alpha/(alpha+beta)},
\eqn{\sigma = (\alpha+\beta+1)^{-1/2}}{sigma=(alpha+beta+1)^(-1/2)}. 
. 


}
\value{
\code{GB1()}  returns a \code{gamlss.family} object which can be used to fit the GB1 distribution in the 
      \code{gamlss()} function. 
\code{dGB1()} gives the density, \code{pGB1()} gives the distribution
     function, \code{qGB1()} gives the quantile function, and \code{rGB1()}
     generates random deviates. 
}
\references{


Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }
\author{Bob Rigby \email{r.rigby@londonmet.ac.uk} and Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} }

\section{Warning}{The qSHASH and rSHASH are slow since they are relying on golden section for finding the quantiles} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{JSU}}, \code{\link{BCT}}}
\examples{
GB1()   # 
y<- rGB1(200, mu=.1, sigma=.6, nu=1, tau=4)
hist(y)
# library(gamlss)
# histDist(y, family=GB1, n.cyc=60)
curve(dGB1(x, mu=.1 ,sigma=.6, nu=1, tau=4), 0.01, 0.99, main = "The GB1  density mu=0.1, sigma=.6, nu=1, tau=4")

}
\keyword{distribution}
\keyword{ regression}
