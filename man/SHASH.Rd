\name{SHASH}
\alias{SHASH}
\alias{dSHASH}
\alias{pSHASH}
\alias{qSHASH}
\alias{rSHASH}

\alias{SHASHo}
\alias{dSHASHo}
\alias{pSHASHo}
\alias{qSHASHo}
\alias{rSHASHo}

\alias{SHASHo2}
\alias{dSHASHo2}
\alias{pSHASHo2}
\alias{qSHASHo2}
\alias{rSHASHo2}

\alias{LOGSHASH}
\alias{dLOGSHASH}
\alias{pLOGSHASH}
\alias{qLOGSHASH}
\alias{rLOGSHASH}


\title{The Sinh-Arcsinh (SHASH) distribution for fitting a GAMLSS}
\description{
The Sinh-Arcsinh (SHASH) distribution is a four parameter distribution, 
for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  The functions \code{dSHASH}, 
\code{pSHASH}, \code{qSHASH} and \code{rSHASH} define the density, 
distribution function, quantile function and random
generation for the Sinh-Arcsinh (SHASH) distribution.   

There are 3 different \code{SHASH} distributions implemented in GAMLSS \code{SHASH}, \code{SHASHo} and \code{SHASH2o2} (see Details) but also there is an exponential version \code{LOGSHASH} for responses in \eqn{(0,\infty)}. 

}
\usage{
SHASH(mu.link = "identity", sigma.link = "log", nu.link = "log", 
      tau.link = "log")
dSHASH(x, mu = 0, sigma = 1, nu = 0.5, tau = 0.5, log = FALSE)
pSHASH(q, mu = 0, sigma = 1, nu = 0.5, tau = 0.5, lower.tail = TRUE, 
     log.p = FALSE)
qSHASH(p, mu = 0, sigma = 1, nu = 0.5, tau = 0.5, lower.tail = TRUE, 
     log.p = FALSE)
rSHASH(n, mu = 0, sigma = 1, nu = 0.5, tau = 0.5)

SHASHo(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
      tau.link = "log")
dSHASHo(x, mu = 0, sigma = 1, nu = 0, tau = 1, log = FALSE)
pSHASHo(q, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, 
     log.p = FALSE)
qSHASHo(p, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, 
     log.p = FALSE)
rSHASHo(n, mu = 0, sigma = 1, nu = 0, tau = 1)

SHASHo2(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
      tau.link = "log")
dSHASHo2(x, mu = 0, sigma = 1, nu = 0, tau = 1, log = FALSE)
pSHASHo2(q, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, 
     log.p = FALSE)
qSHASHo2(p, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, 
     log.p = FALSE)
rSHASHo2(n, mu = 0, sigma = 1, nu = 0, tau = 1)

LOGSHASH(mu.link = "identity", sigma.link = "log", nu.link = "log", 
     tau.link = "log")
dLOGSHASH(x, mu = 0, sigma = 1, nu = 0.5, tau = 0.5, log = FALSE)     
pLOGSHASH(q, mu = 0, sigma = 1, nu = 0.5, tau = 0.5, lower.tail = TRUE, 
     log.p = FALSE)     
qLOGSHASH(p, mu = 0, sigma = 1, nu = 0.5, tau = 0.5, lower.tail = TRUE, 
     log.p = FALSE)
rLOGSHASH(n, mu = 0, sigma = 1, nu = 0.5, tau = 0.5)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. }
 \item{sigma.link}{  Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. }
 \item{nu.link}{Defines the  \code{nu.link}, with "log" link as the default for the \code{nu} parameter.}
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

The probability density function of the Sinh-Arcsinh distribution, \code{SHASH}, Jones(2005), is defined as 
  
\deqn{f(y|\mu,\sigma\,\nu,\tau) = \frac{c}{\sqrt{2 \pi} \sigma (1+z^2)^{1/2}} e^{-r^2/2}}{f(y|mu,sigma,nu,tau)=c/(sqrt(2*pi)*sigma*(1+z^2)^(1/2)) exp(-(r^2)/2)}

where 

\deqn{r=\frac{1}{2} \left \{ \exp\left[ \tau \sinh^{-1}(z) \right] -\exp\left[ -\nu \sinh^{-1}(z)  \right] \right\}}{r=0.5*[exp(tau*sinh^(-1)(z))-exp(-nu*sinh^(-1)(z))]}

and

\deqn{c=\frac{1}{2} \left \{ \tau \exp\left[ \tau \sinh^{-1}(z) \right] + \nu \exp\left[ -\nu \sinh^{-1}(z)  \right] \right\}}{c=0.5*[rho*exp(tau*sinh^(-1)(z))+ nu * exp(-nu*sinh^(-1)(z))]}

and  \eqn{z=(y-\mu)/\sigma}{z=(y-mu)/sigma}
for \eqn{ -\infty < y < \infty }{0<y<0},
\eqn{-\infty<\mu<\infty}{mu=(-Inf,Inf)},
\eqn{\sigma>0}{sigma>0},
\eqn{\nu>0}{nu>0} and
\eqn{\tau>0}{tau>0}, see pp. 396-397 of Rigby et al. (2019).

The parameters \eqn{\mu}{mu} and \eqn{\sigma}{sigma} are the location  and scale of the distribution.
The parameter \eqn{\nu}{nu} determines the left hand tail of the distribution with \eqn{\nu>1}{nu>1} indicating a lighter tail  than the normal 
and 
\eqn{\nu<1} heavier tail than the normal. The parameter \eqn{\tau}{tau}  determines the right hand tail  of the distribution in the same way.

The second form of the Sinh-Arcsinh distribution can be found in Jones and Pewsey (2009, p.2) denoted by \code{SHASHo} and the probability density function is defined as,

\deqn{f(y|\mu,\sigma,\nu,\tau)= \frac{\tau c}{\sigma \sqrt{2 \pi} (1+z^2)^{1/2}} \exp{(-\frac{1}{2} r^2)}}

where 

\deqn{r= \sinh(\tau \, \sinh^{-1}(z)-\nu)}{r= sinh(tau*asinh(z)-nu)}

and 

\deqn{c= \cosh(\tau \sinh^{-1}(z)-\nu)}{c= cosh(\tau asinh(z)-\nu)}

and  \eqn{z=(y-\mu)/\sigma}{z=(y-mu)/sigma}
for \eqn{ -\infty < y < \infty }{0<y<0},
\eqn{-\infty<\mu<+\infty}{mu=(-Inf,+Inf)},
\eqn{\sigma>0}{sigma>0},
\eqn{-\infty<\nu<+\infty}{nu=(-Inf,+Inf)} and
\eqn{\tau>0}{tau>0}, see pp. 398-400 of Rigby et al. (2019)

The third form of the Sinh-Arcsinh distribution (Jones and Pewsey, 2009, p.8) divides the distribution by sigma for the density of the unstandardized variable. This distribution is denoted by \code{SHASHo2} and has pdf

\deqn{f(y|\mu,\sigma,\nu,\tau)= \frac{c}{\sigma} \frac{\tau}{\sqrt{2 \pi}}\frac{1}{\sqrt{1+z^2}}-\exp{-\frac{r^2}{2}}}{f(y|\mu,\sigma,\nu,\tau)= (c/sigma)*(tau/((2 \pi)^(1/2)))*(1/((1+z^2)^(1/2)))-exp(-r^2*0.5)}

where \eqn{z=(y-\mu)/(\sigma \tau)}{z=(y-mu)/(sigma*tau)}, with \eqn{r} and \eqn{c} as for the pdf of the \code{SHASHo} distribution,
for \eqn{ -\infty < y < \infty }{0<y<0},
\eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)},
\eqn{\sigma>0}{sigma>0},
\eqn{\nu=(-\infty,+\infty)}{nu=(-Inf,+Inf)} and
\eqn{\tau>0}{tau>0}. 

}
\value{
\code{SHASH()}  returns a \code{gamlss.family} object which can be used to fit the SHASH distribution in the \code{gamlss()} function. 
\code{dSHASH()} gives the density, \code{pSHASH()} gives the distribution
     function, \code{qSHASH()} gives the quantile function, and \code{rSHASH()}
     generates random deviates. 
}
\note{The parameters \code{sigma}, \code{nu} and \code{tau} are highly correlated. It is recomended to use \code{method=mixed(10,100)} for fitting.}
\references{

Jones, M. C. (2005) p 546-547 in the discussion of Rigby, R. A. and  Stasinopoulos D. M. (2005) 
\emph{Appl. Statist.}, \bold{54}, part 3.

Jones and Pewsey (2009) Sinh-arcsinh distributions. \emph{Biometrika.} \bold{96}(4), pp. 761?780. 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\author{Bob Rigby, Mikis Stasinopoulos and Fiona McElduff}


\section{Warning}{The qSHASH and rSHASH are slow since they are relying on golden section for finding the quantiles} 

\seealso{  \code{\link{gamlss.family}}, \code{\link{JSU}}, \code{\link{BCT}}}
\examples{
SHASH()   # 
plot(function(x)dSHASH(x, mu=0,sigma=1, nu=1, tau=2), -5, 5, 
 main = "The SHASH  density mu=0,sigma=1,nu=1, tau=2")
plot(function(x) pSHASH(x, mu=0,sigma=1,nu=1, tau=2), -5, 5, 
 main = "The BCPE  cdf mu=0, sigma=1, nu=1, tau=2")
dat<-rSHASH(100,mu=10,sigma=1,nu=1,tau=1.5)
hist(dat)
# library(gamlss)
# gamlss(dat~1,family=SHASH, control=gamlss.control(n.cyc=30))
}
\keyword{distribution}
\keyword{ regression}
