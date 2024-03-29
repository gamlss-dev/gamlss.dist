\name{NO2}

\alias{NO2}
\alias{dNO2}
\alias{pNO2}
\alias{qNO2}
\alias{rNO2}


\title{Normal distribution (with variance as sigma parameter) for fitting a GAMLSS}
\description{
The function \code{NO2()} defines the normal distribution, a two parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()} with mean equal to \code{mu}
and variance equal to \code{sigma}.  
The functions \code{dNO2}, \code{pNO2}, \code{qNO2} and \code{rNO2} define the density, distribution function, quantile function and random
generation for this specific parameterization of the normal distribution. 
 
[A alternative parameterization with \code{sigma} as the standard deviation is given in the function \code{NO()}]   
}
\usage{
NO2(mu.link = "identity", sigma.link = "log")
dNO2(x, mu = 0, sigma = 1, log = FALSE)
pNO2(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qNO2(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rNO2(n, mu = 0, sigma = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
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
  The parametrization of the normal distribution given in the function \code{NO2()} is 
 \deqn{f(y|\mu,\sigma)=\frac{1}{\sqrt{2 \pi \sigma}}\exp\left[-\frac{1}{2}\frac{(y-\mu)^2}{\sigma}\right]}{f(y|mu,sigma)=(1/(sqrt(2*pi*sigma))* exp(-0.5*(y-mu)^2/sigma)} 
 
 for \eqn{y=(-\infty,\infty)}{y=(-Inf,+Inf)}, \eqn{\mu=(-\infty,+\infty)}{\mu=(-Inf,+Inf)} and \eqn{\sigma>0} see p. 370 of Rigby et al. (2019). 
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a normal distribution in the \code{gamlss()} function. 
    
}
\references{
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

\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}

\note{ For the function \code{NO()}, \eqn{\mu}{mu} is the mean and \eqn{\sigma}{sigma} is the standard deviation (not the variance) of the normal distribution. 
       [The function \code{NO2()} defines the normal distribution with \eqn{\sigma}{sigma} as the variance.]}



\seealso{  \code{\link{gamlss.family}}, \code{\link{NO}} }

\examples{
NO()# gives information about the default links for the normal distribution
dat<-rNO(100)
hist(dat)        
plot(function(y) dNO(y, mu=10 ,sigma=2), 0, 20)
plot(function(y) pNO(y, mu=10 ,sigma=2), 0, 20)
plot(function(y) qNO(y, mu=10 ,sigma=2), 0, 1)
# library(gamlss)
# gamlss(dat~1,family=NO) # fits a constant for mu and sigma 
}
\keyword{distribution}
\keyword{regression}% 
