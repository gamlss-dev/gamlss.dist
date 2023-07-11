\name{GB2}
\alias{GB2}
\alias{dGB2}
\alias{pGB2}
\alias{qGB2}
\alias{rGB2}
\alias{dGP}
\alias{pGP}
\alias{qGP}
\alias{rGP}
\alias{GP}

\title{The generalized Beta type 2 and generalized Pareto distributions for fitting a GAMLSS}
\description{
This function defines the generalized beta  type 2 distribution, a four parameter distribution.
The function \code{GB2} creates a \code{gamlss.family} object which can be used to fit the distribution using the function 
\code{gamlss()}. The response variable is 
in the range from zero to infinity.  
The functions \code{dGB2}, 
\code{GB2}, \code{qGB2} and \code{rGB2} define the density, 
distribution function, quantile function and random
generation for the generalized beta type 2 distribution.   
The generalised Pareto \code{GP} distribution is defined by setting the parameters \code{sigma} and \code{nu} of the \code{GB2} distribution to 1.  

}
\usage{
GB2(mu.link = "log", sigma.link = "log", nu.link = "log", 
     tau.link = "log")
dGB2(x, mu = 1, sigma = 1, nu = 1, tau = 0.5, log = FALSE)
pGB2(q, mu = 1, sigma = 1, nu = 1, tau = 0.5, lower.tail = TRUE, 
     log.p = FALSE)
qGB2(p, mu = 1, sigma = 1, nu = 1, tau = 0.5, lower.tail = TRUE, 
     log.p = FALSE)
rGB2(n, mu = 1, sigma = 1, nu = 1, tau = 0.5)

GP(mu.link = "log", sigma.link = "log")
dGP(x, mu = 1, sigma = 1, log = FALSE)
pGP(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qGP(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rGP(n, mu = 1, sigma = 1)

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
 The probability density function of the Generalized Beta type 2, (\code{GB2}), is defined as 
  
\deqn{f(y|\mu,\sigma,\nu,\tau)= |\sigma| y^{\hspace{0.01cm}\sigma v-1 } \{\mu^{\sigma \nu} \hspace{0.05cm}B(\nu,\tau) [1+(y/\mu)^\sigma]^{\nu+\tau}\}^{-1}}{f(y|mu,sigma,nu,tau)=abs(sigma)*y^{sigma*nu-1}(mu^(sigma*nu)*Beta(nu,tau)(1+(y/mu)^sigma)^(nu+tau))^-1}
where  \eqn{ y > 0}{y>0}, \eqn{\mu>0}{mu>0}, \eqn{\sigma >0}{sigma>0}, 
\eqn{\nu>0}{nu>0} and \eqn{\tau>0}{tau>0} see pp. 452-453 of Rigby et al. (2019).  

Note that by setting \eqn{\sigma=1} we have the Pearson type VI, by setting \eqn{\nu=1} we have the Burr type XII and by setting \eqn{\tau=1} the Burr type III.


}
\value{
\code{GB2()}  returns a \code{gamlss.family} object which can be used to fit the GB2 distribution in the 
      \code{gamlss()} function. 
\code{dGB2()} gives the density, \code{pGB2()} gives the distribution
     function, \code{qGB2()} gives the quantile function, and \code{rGB2()}
     generates random deviates. 
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape, (with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973} 

(see also \url{https://www.gamlss.com/}).
}
\author{Bob Rigby and Mikis Stasinopoulos}


\section{Warning}{The qSHASH and rSHASH are slow since they are relying on golden section for finding the quantiles} 

\seealso{  \code{\link{gamlss.family}}, \code{\link{JSU}}, \code{\link{BCT}}}
\examples{
GB2()   # 
y<- rGB2(200, mu=5, sigma=2, nu=1, tau=1)
library(MASS)
truehist(y)
fx<-dGB2(seq(0.01, 20, length=200), mu=5 ,sigma=2, nu=1, tau=1)
lines(seq(0.01,20,length=200),fx)
integrate(function(x) x*dGB2(x=x, mu=5, sigma=2, nu=1, tau=1), 0, Inf)
mean(y)
curve(dGB2(x, mu=5 ,sigma=2, nu=1, tau=1), 0.01, 20, 
            main = "The GB2  density mu=5, sigma=2, nu=1, tau=4")
}
\keyword{distribution}
\keyword{ regression}
