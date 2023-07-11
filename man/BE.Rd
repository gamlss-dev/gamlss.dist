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


\title{The beta distribution for fitting a GAMLSS}
\description{
The functions \code{BE()} and \code{BEo()} define the beta distribution, a two parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. \code{BE()} has  mean equal to the parameter \code{mu}
and \code{sigma} as scale parameter, see below. \code{BEo()} is the original parameterizations  of the beta distribution as in \code{dbeta()} with 
\code{shape1}=mu and \code{shape2}=sigma. 
The functions \code{dBE} and \code{dBEo}, \code{pBE} and \code{pBEo}, \code{qBE} and \code{qBEo}  and finally \code{rBE} and \code{rBE}  
define the density, distribution function, quantile function and random
generation for the \code{BE} and \code{BEo} parameterizations respectively of the beta distribution.      
}
\usage{
BE(mu.link = "logit", sigma.link = "logit")
dBE(x, mu = 0.5, sigma = 0.2, log = FALSE)
pBE(q, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE)
qBE(p, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE)
rBE(n, mu = 0.5, sigma = 0.2)
BEo(mu.link = "log", sigma.link = "log")
dBEo(x, mu = 0.5, sigma = 0.2, log = FALSE)
pBEo(q, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE)
qBEo(p, mu = 0.5, sigma = 0.2, lower.tail = TRUE, log.p = FALSE)
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

The standard parametrization of the  beta distribution is given as:
 \deqn{f(y|\alpha,\beta)=\frac{1}{B(\alpha, \beta)} y^{\alpha-1}(1-y)^{\beta-1}}{f(y|a,b)=1/(Beta(a,b)) y^(a-1)(1-y)^(b-1)} 
for \eqn{y=(0,1)}{y=(0,1)}, \eqn{\alpha>0} and \eqn{\beta>0}. 

The first \code{gamlss} implementation the  beta distribution is called  \code{BEo}, and it is identical  to the standard parametrization  with  
\eqn{\alpha=\mu} and \eqn{\beta = \sigma}, see pp. 460-461 of Rigby et al. (2019): 

\deqn{f(y|\mu,\sigma)=\frac{1}{B(\mu, \sigma)} y^{\mu-1}(1-y)^{\sigma-1}}{f(y|mu,sigma)=1/(Beta(mu,sigma)) y^(mu-1)(1-y)^(sigma-1)} 

for \eqn{y=(0,1)}{y=(0,1)}, \eqn{\mu>0} and \eqn{\sigma>0}.  The problem with this parametrization is that with mean \eqn{E(y)=\mu/(\mu+\sigma)} it is not  convenient for modelling the response y as function of the explanatory variables. The second parametrization, \code{BE}  see pp. 461-463 of Rigby et al. (2019), is using 
\deqn{\mu=\frac{\alpha}{\alpha+\beta}}{mu=a/(a+b)}
 \deqn{\sigma= \frac{1}{\alpha+\beta+1)^{1/2}} }{sigma=(1/(a+b+1))^0.5}
(of the   standard parametrization) and it is more convenient because 
\eqn{\mu} now is the mean with variance equal to \eqn{Var(y)=\sigma^2 \mu (1-\mu)}. Note however that \eqn{0<\mu<1} and \eqn{0<\sigma<1}. 


%The re-parametrization in the function \code{BE()} is 
% for \eqn{\mu=(0,1)}{mu=(0,1)} and \eqn{\sigma=(0,1)}{sigma=(0,1)}.
% The expected value of y is \eqn{\mu}{mu} and the variance is \eqn{\sigma^2\mu*(1-\mu)}{sigma^2*mu*(1-mu)}. 
}
\value{
 \code{BE()} and \code{BEo()} return a \code{gamlss.family} object which can be used to fit a beta distribution in the \code{gamlss()} function. 
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{https://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).

}

\author{Bob Rigby and Mikis Stasinopoulos }
\note{Note that for \code{BE}, \code{mu} is the mean and \code{sigma} a scale parameter contributing to the variance of y}


\seealso{  \code{\link{gamlss.family}}, \code{\link{BE}}, \code{\link{LOGITNO}}, \code{\link{GB1}}, \code{\link{BEINF}} }
\examples{
BE()# gives information about the default links for the beta distribution
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
