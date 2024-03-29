\name{LO}
\alias{LO}
\alias{dLO}
\alias{pLO}
\alias{qLO}
\alias{rLO}



\title{Logistic distribution for fitting a GAMLSS}
\description{
The function \code{LO()}, or equivalently \code{Logistic()}, defines the logistic distribution, a two parameter distribution, 
for a \code{gamlss.family} object to be used in GAMLSS fitting using the function gamlss()   
}
\usage{
LO(mu.link = "identity", sigma.link = "log")
dLO(x, mu = 0, sigma = 1, log = FALSE)
pLO(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qLO(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rLO(n, mu = 0, sigma = 1)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
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
  Definition file for Logistic distribution. 
 \deqn{f(y|\mu,\sigma)=\frac{1}{\sigma} e^{-\left(\frac{y-\mu}{\sigma}\right)} [1+e^{-\left(\frac{y-\mu}{\sigma}\right)}]^{-2}}{f(y|mu,sigma)=(1/sigma)*exp((y-mu)/sigma)*((1+exp(-(y-mu)/sigma))^(-2))} 
 for \eqn{y=(-\infty,\infty)}{y=(-Inf,+Inf)}, \eqn{\mu=(-\infty,\infty)}{\mu=(-Inf,+Inf)} and \eqn{\sigma>0}, see page 368 of Rigby et al. (2019). 
}
\value{
  \code{LO()} returns a \code{gamlss.family} object which can be used to fit a logistic distribution in the \code{gamlss()} function.
  \code{dLO()} gives the density, \code{pLO()} gives the distribution
     function, \code{qLO()} gives the quantile function, and \code{rLO()}
     generates random deviates for the logistic distribution. 
     The latest functions are based on the equivalent \code{R} functions for logistic distribution.
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

\author{Mikis Stasinopoulos, Bob Rigby  and Calliope Akantziliotou}


\note{\eqn{\mu}{mu} is the mean and \eqn{\sigma \pi/ \sqrt3}{sigma*pi/sqrt(3)} is the standard deviation for the logistic distribution}


\seealso{ \code{\link{gamlss.family}}, \code{\link{NO}}, \code{\link{TF}}}

\examples{
LO()# gives information about the default links for the Logistic distribution 
plot(function(y) dLO(y, mu=10 ,sigma=2), 0, 20)
plot(function(y) pLO(y, mu=10 ,sigma=2), 0, 20)
plot(function(y) qLO(y, mu=10 ,sigma=2), 0, 1)
# library(gamlss)
# data(abdom)
# h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=LO, data=abdom) # fits 
# plot(h)
}
\keyword{distribution}
\keyword{regression}% 
