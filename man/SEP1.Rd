\name{SEP1}
\alias{SEP1}
\alias{dSEP1}
\alias{pSEP1}
\alias{qSEP1}
\alias{rSEP1}
\alias{SEP2}
\alias{dSEP2}
\alias{pSEP2}
\alias{qSEP2}
\alias{rSEP2}
\alias{SEP3}
\alias{dSEP3}
\alias{pSEP3}
\alias{qSEP3}
\alias{rSEP3}
\alias{SEP4}
\alias{dSEP4}
\alias{pSEP4}
\alias{qSEP4}
\alias{rSEP4}

\title{The Skew Power exponential type 1-4  distribution for fitting a GAMLSS}
\description{
These functions define the Skew Power exponential type 1 to 4  distributions. All of them are four 
parameter distributions and can be used to fit a GAMLSS model.   
The functions \code{dSEP1}, \code{dSEP2}, \code{dSEP3} and \code{dSEP4} define the probability distribution functions,
the functions \code{pSEP1}, \code{pSEP2}, \code{pSEP3} and \code{pSEP4} define the cumulative distribution functions 
the functions \code{qSEP1}, \code{qSEP2}, \code{qSEP3} and \code{qSEP4} define the inverse cumulative distribution functions and 
the functions \code{rSEP1}, \code{rSEP2}, \code{rSEP3} and \code{rSEP4} define the random generation for the Skew exponential power
distributions.   
}
\usage{
SEP1(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
     tau.link = "log")
dSEP1(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pSEP1(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
     log.p = FALSE)
qSEP1(p, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
     log.p = FALSE)
rSEP1(n, mu = 0, sigma = 1, nu = 0, tau = 2)

SEP2(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
      tau.link = "log")
dSEP2(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pSEP2(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
      log.p = FALSE)
qSEP2(p, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
      log.p = FALSE)
rSEP2(n, mu = 0, sigma = 1, nu = 0, tau = 2)

SEP3(mu.link = "identity", sigma.link = "log", nu.link = "log", 
      tau.link = "log")
dSEP3(x, mu = 0, sigma = 1, nu = 2, tau = 2, log = FALSE)
pSEP3(q, mu = 0, sigma = 1, nu = 2, tau = 2, lower.tail = TRUE, 
      log.p = FALSE)
qSEP3(p, mu = 0, sigma = 1, nu = 2, tau = 2, lower.tail = TRUE, 
      log.p = FALSE)

SEP4(mu.link = "identity", sigma.link = "log", nu.link = "log", 
      tau.link = "log")
dSEP4(x, mu = 0, sigma = 1, nu = 2, tau = 2, log = FALSE)      
pSEP4(q, mu = 0, sigma = 1, nu = 2, tau = 2, lower.tail = TRUE, 
      log.p = FALSE)
qSEP4(p, mu = 0, sigma = 1, nu = 2, tau = 2, lower.tail = TRUE, 
      log.p = FALSE)
rSEP4(n, mu = 0, sigma = 1, nu = 2, tau = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "inverse" and "log"}
 \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse" and "identity"}
 \item{nu.link}{Defines the  \code{nu.link}, with "log" link as the default for the \code{nu} parameter. Other links are "identity" and "inverse"}
 \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter. Other links are "inverse", and "identity}
 \item{x,q}{vector of quantiles}
 \item{mu}{vector of location parameter values}
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



 The probability density function of the Skew Power exponential distribution type 2, (\code{SEP2}), is defined as 
  
\deqn{f_Y(y|\mu,\sigma\,\nu,\tau)=\frac{\nu}{\sigma (1+\nu^2)2^{1/\tau} \Gamma(1+1/\tau)}\left\{\exp\left(- \frac{1}{2} \left|\frac{\nu (y-\mu)}{\sigma} \right|^\tau \right) I(y<\mu)+\exp\left(- \frac{1}{2} \left|\frac{(y-\mu)}{\sigma \nu} \right|^\tau \right) I(y \geq \mu)\right\}}{f(y|mu,sigma,nu,tau)=nu/(sigma*(1+nu^2)*2^(1/tau) Gamma(1+1/tau))*(exp(-.5*abs(nu(y-mu)/sigma))^tau*I(y<mu)+exp(-.5*abs((y-mu)/sigma*nu))^tau*I(y>=mu))}

for \eqn{ -\infty < y < \infty }{0<y<0},
\eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)},
\eqn{\sigma>0}{sigma>0},
\eqn{\nu>0)}{nu>0)} and
\eqn{\tau>0}{tau>0}.
}
\value{
\code{SEP2()}  returns a \code{gamlss.family} object which can be used to fit the SEP2 distribution in the \code{gamlss()} function. 
\code{dSEP2()} gives the density, \code{pSEP2()} gives the distribution
     function, \code{qSEP2()} gives the quantile function, and \code{rSEP2()}
     generates random deviates. 
}
\references{
Fernadez C., Osiewalski J. and Steel M.F.J.(1995) Modelling and inference with v-spherical distributions. 
\emph{JASA}, \bold{90}, pp 1331-1340. 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Bob Rigby \email{r.rigby@londonmet.ac.uk} and Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} }



\seealso{ \code{\link{gamlss.family}}, \code{\link{SEP}}}
\examples{
SEP1() 
curve(dSEP4(x, mu=5 ,sigma=1, nu=2, tau=1.5), -2, 10, main = "The SEP4  density mu=5 ,sigma=1, nu=1, tau=1.5")
# library(gamlss)
#y<- rSEP4(100, mu=5, sigma=1, nu=2, tau=1.5);hist(y)
#m1<-gamlss(y~1, family=SEP1, n.cyc=50)
#m2<-gamlss(y~1, family=SEP2, n.cyc=50)
#m3<-gamlss(y~1, family=SEP3, n.cyc=50)
#m4<-gamlss(y~1, family=SEP4, n.cyc=50) 
#GAIC(m1,m2,m3,m4)
}
\keyword{distribution}
\keyword{ regression}
