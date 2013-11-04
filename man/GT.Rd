\name{GT}
\alias{GT}
\alias{dGT}
\alias{pGT}
\alias{qGT}
\alias{rGT}



\title{The generalized t distribution for fitting a GAMLSS}
\description{
This function defines the generalized t distribution, a four parameter distribution, 
for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  
The functions \code{dGT}, 
\code{pGT}, \code{qGT} and \code{rGT} define the density, 
distribution function, quantile function and random
generation for the generalized t distribution.   
}
\usage{
GT(mu.link = "identity", sigma.link = "log", nu.link = "log", 
   tau.link = "log")
dGT(x, mu = 0, sigma = 1, nu = 3, tau = 1.5, log = FALSE)
pGT(q, mu = 0, sigma = 1, nu = 3, tau = 1.5, lower.tail = TRUE, 
   log.p = FALSE)
qGT(p, mu = 0, sigma = 1, nu = 3, tau = 1.5, lower.tail = TRUE, 
   log.p = FALSE)
rGT(n, mu = 0, sigma = 1, nu = 3, tau = 1.5)
}
%- maybe also 'usage' for other objects documented here.
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
 The probability density function of the generalized t distribution, (\code{GT}), , is defined as 
  
\deqn{f(y|\mu,\sigma\,\nu,\tau)= \tau \left\{2\sigma \nu^{1/\tau} B\left(\frac{1}{\tau},\nu\right)[1+|z|^{\tau}/\nu]^{\nu+1/\tau} \right\}^{-1}}{f(y|mu,sigma,nu,tau)=}

where  \eqn{ -\infty < y < \infty }{0<y<0}, \eqn{z=(y-\mu)/\sigma}{z=(y-mu)/sigma}
\eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)},
\eqn{\sigma>0}{sigma>0},
\eqn{\nu>0}{nu>0} and
\eqn{\tau>0}{tau>0}. 


}
\value{
\code{GT()}  returns a \code{gamlss.family} object which can be used to fit the GT distribution in the 
      \code{gamlss()} function. 
\code{dGT()} gives the density, \code{pGT()} gives the distribution
     function, \code{qGT()} gives the quantile function, and \code{rGT()}
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
GT()   # 
y<- rGT(200, mu=5, sigma=1, nu=1, tau=4)
hist(y)
curve(dGT(x, mu=5 ,sigma=2,nu=1, tau=4), -2, 11, main = "The GT  density mu=5 ,sigma=1, nu=1, tau=4")
# library(gamlss)
# m1<-gamlss(y~1, family=GT) 
}
\keyword{distribution}
\keyword{ regression}
