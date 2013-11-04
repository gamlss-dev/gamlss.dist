\name{BE}
\alias{BE}
\alias{dBE}
\alias{pBE}
\alias{qBE}
\alias{rBE}
\alias{BEo}
\alias{dBEo}
\alias{pBEo}
\alias{qBEo}
\alias{rBEo}


\title{ The beta distribution for fitting a GAMLSS}
\description{
The functions \code{BE()} and \code{BEo()} define the beta distribution, a two parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. \code{BE()} has  mean equal to the parameter \code{mu}
and \code{sigma} as scale parameter, see below. \code{BE()} is the original parameterizations  of the beta distribution as in \code{dbeta()} with 
\code{shape1}=mu and \code{shape2}=sigma. 
The functions \code{dBE} and \code{dBEo}, \code{pBE} and \code{pBEo}, \code{qBE} and \code{qBEo}  and finally \code{rBE} and \code{rBE}  
define the density, distribution function, quantile function and random
generation for the \code{BE} and \code{BEo} parameterizations respectively of the beta distribution.      
}
\usage{
BE(mu.link = "logit", sigma.link = "logit")
dBE(x, mu = 0.5, sigma = 0.02, log = FALSE)
pBE(q, mu = 0.5, sigma = 0.02, lower.tail = TRUE, log.p = FALSE)
qBE(p, mu = 0.5, sigma = 0.02, lower.tail = TRUE, log.p = FALSE)
rBE(n, mu = 0.5, sigma = 0.02)
BEo(mu.link = "log", sigma.link = "log")
dBEo(x, mu = 0.5, sigma = 0.02, log = FALSE)
pBEo(q, mu = 0.5, sigma = 0.02, lower.tail = TRUE, log.p = FALSE)
qBEo(p, mu = 0.5, sigma = 0.02, lower.tail = TRUE, log.p = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the \code{mu} link function with default \code{logit}}
  \item{sigma.link}{the \code{sigma} link function with default \code{logit}}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values} 
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}   
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}  
}
\details{

The original beta distributions distribution is given as 
 \deqn{f(y|\alpha,\beta)=\frac{1}{B(\alpha, \beta)} y^{\alpha-1}(1-y)^{\beta-1}}{f(y|a,b)=1/(Beta(a,b)) y^(a-1)(1-y)^(b-1)} 
for \eqn{y=(0,1)}{y=(0,1)}, \eqn{\alpha>0} and \eqn{\beta>0}. In the \code{gamlss} implementation of \code{BEo} 
\eqn{\alpha=\mu} and \eqn{\beta>\sigma}.   The reparametrization in the function \code{BE()} is 
 \eqn{\mu=\frac{\alpha}{\alpha+\beta}}{mu=a/(a+b)} and  \eqn{\sigma=\frac{1}{\alpha+\beta+1}}{sigma=1/(a+b+1)}
 for \eqn{\mu=(0,1)}{mu=(0,1)} and \eqn{\sigma=(0,1)}{sigma=(0,1)}.
 The expected value of y is \eqn{\mu}{mu} and the variance is \eqn{\sigma^2\mu*(1-\mu)}{sigma^2*mu*(1-mu)}. 
}
\value{
 returns a \code{gamlss.family} object which can be used to fit a normal distribution in the \code{gamlss()} function. 
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}

\author{Bob Rigby and Mikis Stasinopoulos }
\note{Note that for \code{BE}, \code{mu} is the mean and \code{sigma} a scale parameter contributing to the variance of y}


\seealso{  \code{\link{gamlss.family}}, \code{\link{BEINF}} }
\examples{
BE()# gives information about the default links for the normal distribution
dat1<-rBE(100, mu=.3, sigma=.5)
hist(dat1)        
#library(gamlss)
# mod1<-gamlss(dat1~1,family=BE) # fits a constant for mu and sigma 
#fitted(mod1)[1]
#fitted(mod1,"sigma")[1]
plot(function(y) dBE(y, mu=.1 ,sigma=.5), 0.001, .999)
plot(function(y) pBE(y, mu=.1 ,sigma=.5), 0.001, 0.999)
plot(function(y) qBE(y, mu=.1 ,sigma=.5), 0.001, 0.999)
plot(function(y) qBE(y, mu=.1 ,sigma=.5, lower.tail=FALSE), 0.001, .999)
dat2<-rBEo(100, mu=1, sigma=2)
#mod2<-gamlss(dat2~1,family=BEo) # fits a constant for mu and sigma 
#fitted(mod2)[1]
#fitted(mod2,"sigma")[1]
}
\keyword{distribution}
\keyword{regression}
