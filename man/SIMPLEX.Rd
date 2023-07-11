\name{SIMPLEX}
\alias{SIMPLEX}
\alias{dSIMPLEX}
\alias{pSIMPLEX}
\alias{qSIMPLEX}
\alias{rSIMPLEX}
\title{
The simplex distribution for fitting a GAMLSS
}
\description{The functions \code{SIMPLEX()}  define the simplex distribution, a two parameter distribution, for a  \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. \code{SIMPLEX()} has  mean equal to the parameter \code{mu} and \code{sigma} as scale parameter, see below. 
The functions \code{dSIMPLEX},  \code{pSIMPLEX}  \code{qSIMPLEX} and  \code{rSIMPLEX}
define the density, comulative distribution function, quantile function and random
generation for the simplex distribution.}
\usage{
SIMPLEX(mu.link = "logit", sigma.link = "log")
dSIMPLEX(x, mu = 0.5, sigma = 1, log = FALSE)
pSIMPLEX(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qSIMPLEX(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rSIMPLEX(n = 1, mu = 0.5, sigma = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the \code{mu} link function with default \code{logit}}
  \item{sigma.link}{the \code{sigma} link function with default \code{log}}
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
The simplex distribution,  \code{SIMPLEX}, is given as 
 \deqn{f(y|\mu, \sigma) = \frac{1}{\left( 2 \pi \sigma^2 y^3 (1-y)^3\right)^{1/2}} \exp(-\frac{1}{2\sigma^2} \frac{(y-\mu)^2}{y(1-y) \mu^2 (1-\mu)^2 } )}{f(y|mu,sigma) = 1/(2*pi*sigma^2*(y*(1-y))^3)^(1/2)  exp(-(1/(2*sigma^2)) * ((y-mu)^2)/(y*(1-y)*mu^2*(1-mu)^2))} 
for \eqn{0<y<1}, \eqn{0<\mu<1} and \eqn{\sigma>0} see p 464 of Rigby et al. (2019).  
 }	
 	
\value{
 \code{SIMPLEX()} returns a \code{gamlss.family} object which can be used to fit a simplex distribution in the \code{gamlss()} function. 
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}). 

}
\author{Bob Rigby, Mikis Stasinopoulos and Fernanda De Bastiani}


\examples{
SIMPLEX()#  default links for the simplex distribution
plot(function(y) dSIMPLEX(y, mu=.5 ,sigma=1), 0.001, .999)
plot(function(y) pSIMPLEX(y, mu=.5 ,sigma=1), 0.001, 0.999)
plot(function(y) qSIMPLEX(y, mu=.5 ,sigma=1), 0.001, 0.999)
plot(function(y) qSIMPLEX(y, mu=.5 ,sigma=1, lower.tail=FALSE), 0.001, .999)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}