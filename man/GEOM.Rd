\name{GEOM}
\alias{GEOM}
\alias{dGEOM}
\alias{pGEOM}
\alias{qGEOM}
\alias{rGEOM}
\alias{GEOMo}
\alias{dGEOMo}
\alias{pGEOMo}
\alias{qGEOMo}
\alias{rGEOMo}


\title{Geometric distribution for fitting a GAMLSS model}
\description{
The functions  \code{GEOMo()} and  \code{GEOM()} define  two parametrizations of the geometric distribution. The geometric distribution is a  one parameter 
distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}.  The  mean of \code{GEOM()} is equal to the parameter \code{mu}. 
The functions \code{dGEOM}, \code{pGEOM}, \code{qGEOM} and \code{rGEOM} define 
the density, distribution function, quantile function and random generation for 
the \code{GEOM} parameterization of the Geometric distribution. 
}
\usage{
GEOM(mu.link = "log")
dGEOM(x, mu = 2, log = FALSE)
pGEOM(q, mu = 2, lower.tail = TRUE, log.p = FALSE)
qGEOM(p, mu = 2, lower.tail = TRUE, log.p = FALSE)
rGEOM(n, mu = 2)
GEOMo(mu.link = "logit")
dGEOMo(x, mu = 0.5, log = FALSE)
pGEOMo(q, mu = 0.5, lower.tail = TRUE, log.p = FALSE)
qGEOMo(p, mu = 0.5, lower.tail = TRUE, log.p = FALSE)
rGEOMo(n, mu = 0.5)
}

\arguments{
	\item{mu.link}{Defines the \code{mu.link}, with \code{log} link as the default for the \code{mu} parameter}
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
}
\details{
The parameterization of the GAMLSS geometric distribution in the function
\code{GEOM} is:

\deqn{f(y|\mu) = \mu^y/(\mu+1)^{y+1}}{f(y|\mu) = \mu^y/(\mu+1)^{y+1} }
for \eqn{y=0,1,2,3,...} and \eqn{\mu>0}, see pp 472-473 of Rigby et al. (2019). Note that in this parametrization \eqn{E(y)=\mu}. 



The parameterization of the original geometric distribution, \code{GEOMo}  is
\deqn{f(y|\mu) = (1-\mu)^y\, \mu}{f(y|\mu) = (1-\mu)^y \mu}
for \eqn{y=0,1,2,3,...} and \eqn{\mu>0},   see pp 473-474 of Rigby et al. (2019). In this parametrization \eqn{E(y)=(1-\mu)\mu^{-1}}.

}
\value{
returns a \code{gamlss.family} object which can be used to fit a Geometric distribution in the \code{gamlss()} function.}
\author{
Fiona McElduff, Bob Rigby and Mikis Stasinopoulos.
}
\references{

Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). \emph{Univariate discrete distributions.}
Wiley.

Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), \emph{Appl. Statist.}, 54, part 3, pp 507-554. 

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R. \emph{Journal of Statistical Software}, Vol. 23, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).

}

\seealso{\code{\link{gamlss.family}}} 

\examples{
par(mfrow=c(2,2))
y<-seq(0,20,1)
plot(y, dGEOM(y), type="h")
q <- seq(0, 20, 1)
plot(q, pGEOM(q), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p , qGEOM(p), type="s")
dat <- rGEOM(100)
hist(dat)
#summary(gamlss(dat~1, family=GEOM))
par(mfrow=c(2,2))
y<-seq(0,20,1)
plot(y, dGEOMo(y), type="h")
q <- seq(0, 20, 1)
plot(q, pGEOMo(q), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p , qGEOMo(p), type="s")
dat <- rGEOMo(100)
hist(dat)
#summary(gamlss(dat~1, family="GE"))
}
\keyword{distribution}
\keyword{regression}
