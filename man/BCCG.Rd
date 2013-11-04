\name{BCCG}

\alias{BCCG}
\alias{BCCGuntr}
\alias{dBCCG}
\alias{pBCCG}
\alias{qBCCG}
\alias{rBCCG}
\alias{BCCGo}

\title{Box-Cox Cole and Green distribution (or Box-Cox normal) for fitting a GAMLSS}
\description{
The function \code{BCCG}  defines the Box-Cox Cole and Green distribution (Box-Cox normal), a three parameter distribution,
for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
The functions \code{dBCCG}, \code{pBCCG}, \code{qBCCG} and \code{rBCCG} define the density, distribution function, quantile function and random generation for the specific parameterization of the Box-Cox Cole and Green distribution.  
[The function \code{BCCGuntr()} is the original version of the function suitable only for the untruncated Box-Cox Cole and Green distribution 
See Cole and Green (1992) and Rigby and Stasinopoulos (2003a,2003b) for details.
The function \code{BCCGo} is identical to \code{BCCG} but with log link for mu.

}
\usage{
BCCG(mu.link = "identity", sigma.link = "log", nu.link = "identity")
BCCGo(mu.link = "log", sigma.link = "log", nu.link = "identity")
BCCGuntr(mu.link = "identity", sigma.link = "log", nu.link = "identity")
dBCCG(x, mu = 1, sigma = 0.1, nu = 1, log = FALSE)
pBCCG(q, mu = 1, sigma = 0.1, nu = 1, lower.tail = TRUE, log.p = FALSE)
qBCCG(p, mu = 1, sigma = 0.1, nu = 1, lower.tail = TRUE, log.p = FALSE)
rBCCG(n, mu = 1, sigma = 0.1, nu = 1)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the mu parameter, other links are "inverse", "log" and "own"}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter, other links are "inverse", "identity" and "own"}
  \item{nu.link}{Defines the \code{nu.link}, with "identity" link as the default for the nu parameter, other links are "inverse", "log" and "own"}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of skewness parameter values}
   \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
The probability distribution function of the untrucated Box-Cox Cole and Green distribution, \code{BCCGuntr}, is defined as 
\deqn{f(y|\mu,\sigma,\nu)=\frac{1}{\sqrt{2\pi}\sigma}\frac{y^{\nu-1}}{\mu^\nu} \exp(-\frac{z^2}{2})}{f(y|mu,sigma,nu)=(1/(sqrt(2*pi)*sigma))*(y^(nu-1)/mu^nu)*exp(-z^2/2)}
 
where if \eqn{\nu \neq 0}{\nu!=0} then \eqn{z=[(y/\mu)^{\nu}-1]/(\nu \sigma)}{z=[(y/mu)^(nu)-1]/(nu*sigma)} else 
\eqn{z=\log(y/\mu)/\sigma}{z=log(y/mu)/sigma}, 
for \eqn{y>0}, \eqn{\mu>0}, \eqn{\sigma>0} and \eqn{\nu=(-\infty,+\infty)}{\nu=(-Inf,+Inf)}. 

The Box-Cox Cole anf Green distribution,  \code{BCCG}, adjusts the above density \eqn{f(y|\mu,\sigma,\nu)}{f(y|mu,sigma,nu} for the 
truncation resulting from the condition \eqn{y>0}{y>0}. See Rigby and Stasinopoulos (2003a,2003b) for details.   
}
\value{
\code{BCCG()} returns a \code{gamlss.family} object which can be used to fit a Cole and Green distribution in the \code{gamlss()} function.
  \code{dBCCG()} gives the density, \code{pBCCG()} gives the distribution
     function, \code{qBCCG()} gives the quantile function, and \code{rBCCG()}
     generates random deviates.
}
\references{ 
Cole,  T. J. and Green,  P. J. (1992) Smoothing reference centile curves: the LMS method and penalized likelihood, \emph{Statist. Med.} \bold{11}, 1305--1319

Rigby, R. A. and Stasinopoulos, D. M. (2004). Smooth centile curves for skew and kurtotic data modelled using the Box-Cox Power
Exponential distribution. \emph{Statistics in Medicine}, \bold{23}: 3053-3076.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R.A. Stasinopoulos, D.M. (2006).   Using the Box-Cox \emph{t} distribution in GAMLSS to mode skewnees and  and kurtosis.
to appear in \emph{Statistical Modelling}.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{ Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Kalliope Akantziliotou}
\note{\eqn{\mu}{mu} is the median of the distribution \eqn{\sigma}{sigma} is approximately the coefficient of variation (for small values of \eqn{\sigma}{sigma}), 
 and \eqn{\nu}{nu} controls the skewness.
 
 The \code{BCCG} distribution is suitable for all combinations of the parameters within their ranges 
[i.e. \eqn{\mu>0,\sigma>0, {\rm and} \nu=(-\infty,\infty)}{mu>0, sigma>0, and nu=(-Inf,+Inf)} ] 
 
 
 }

\section{Warning }{The \code{BCCGuntr} distribution may be unsuitable for some combinations of the parameters 
(mainly for large \eqn{\sigma}{sigma}) 
where the integrating constant is less than 0.99. A warning will be given if this is the case.
The BCCG distribution is suitable for all combinations of the distributional parameters within
their range [i.e. \eqn{\mu>0}{mu>0}, \eqn{\sigma>0}{sigma>0}, \eqn{\nu=(-\infty, +\infty)}{nu=(-Inf,+Inf)}]} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{BCPE}}, \code{\link{BCT}}}
\examples{
BCCG()   # gives information about the default links for the Cole and Green distribution 
# library(gamlss)
#data(abdom)
#h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=BCCG, data=abdom) 
#plot(h)
plot(function(x) dBCCG(x, mu=5,sigma=.5,nu=-1), 0.0, 20, 
 main = "The BCCG  density mu=5,sigma=.5,nu=-1")
plot(function(x) pBCCG(x, mu=5,sigma=.5,nu=-1), 0.0, 20, 
 main = "The BCCG  cdf mu=5, sigma=.5, nu=-1")
}
\keyword{distribution}
\keyword{regression}% 
