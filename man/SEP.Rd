\name{SEP}
\alias{SEP}
\alias{dSEP}
\alias{pSEP}
\alias{qSEP}
\alias{rSEP}



\title{The Skew Power exponential (SEP) distribution for fitting a GAMLSS}
\description{
This function defines the Skew Power exponential (SEP) distribution, a four parameter distribution, 
for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  The functions \code{dSEP}, 
\code{pSEP}, \code{qSEP} and \code{rSEP} define the density, 
distribution function, quantile function and random
generation for the Skew Power exponential (SEP) distribution.   
}
\usage{
SEP(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
    tau.link = "log")
dSEP(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pSEP(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
     log.p = FALSE)
qSEP(p, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
     log.p = FALSE, lower.limit = mu - 5 * sigma, 
     upper.limit = mu + 5 * sigma)
rSEP(n, mu = 0, sigma = 1, nu = 0, tau = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "\eqn{1/mu^2}" and "log"}
 \item{sigma.link}{  Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse" and "identity"}
 \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. Other links are "\eqn{1/nu^2}" and "log"}
 \item{tau.link}{Defines the  \code{tau.link}, with "log" link as the default for the \code{tau} parameter. Other links are "\eqn{1/tau^2}", and "identity}
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
 \item{lower.limit}{lower limit for the golden search to find quantiles from probabilities  }
 \item{upper.limit}{upper limit for the golden search to find quantiles from probabilities }                 
}
\details{   
 The probability density function of the Skew Power exponential distribution, (\code{SEP}), is defined as 
  
\deqn{f(y|n,\mu,\sigma\,\nu,\tau)==\frac{z}{\sigma} \Phi(\omega) \hspace{1mm} f_{EP}(z,0,1,\tau) }{f(y|mu,sigma,nu,tau)=(z/sigma)*pnorm(w)*dPE(z,0,1,tau)}

for \eqn{ -\infty < y < \infty }{0<y<0},
\eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)},
\eqn{\sigma>0}{sigma>0},
\eqn{\nu=(-\infty,+\infty)}{nu=(-Inf,+Inf)} and
\eqn{\tau>0}{tau>0}. where \eqn{z =
\frac{y-\mu}{\sigma}}{z=(y-mu)/(sigma)}, \eqn{\omega =
sign(z)|z|^{\tau/2}\nu \sqrt{2/\tau}}{w=sign(z)|z|^(t/2)
*nu*sqrt(2/tau)}  and \eqn{f_{EP}(z,0,1,\tau)}{dPE(z,0,1,tau)} is
the pdf of an Exponential Power distribution.
 
}
\value{
\code{SEP()}  returns a \code{gamlss.family} object which can be used to fit the SEP distribution in the \code{gamlss()} function. 
\code{dSEP()} gives the density, \code{pSEP()} gives the distribution
     function, \code{qSEP()} gives the quantile function, and \code{rSEP()}
     generates random deviates. 
}
\references{
Diciccio, T. J. and Mondi A. C. (2004). Inferential Aspects of the Skew Exponential Power distribution.,
    \emph{JASA}, \bold{99}, 439-450.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Bob Rigby \email{r.rigby@londonmet.ac.uk} and Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} }


\section{Warning}{The qSEP and rSEP are slow since they are relying on golden section for finding the quantiles} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{JSU}}, \code{\link{BCT}}}
\examples{
SEP()   # 
plot(function(x)dSEP(x, mu=0,sigma=1, nu=1, tau=2), -5, 5, 
 main = "The SEP  density mu=0,sigma=1,nu=1, tau=2")
plot(function(x) pSEP(x, mu=0,sigma=1,nu=1, tau=2), -5, 5, 
 main = "The BCPE  cdf mu=0, sigma=1, nu=1, tau=2")
dat <- rSEP(100,mu=10,sigma=1,nu=-1,tau=1.5)
# library(gamlss)
# gamlss(dat~1,family=SEP, control=gamlss.control(n.cyc=30))
}
\keyword{distribution}
\keyword{ regression}
