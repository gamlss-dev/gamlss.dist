\name{GEOM}
\alias{GEOM}
\alias{dGEOM}
\alias{pGEOM}
\alias{qGEOM}
\alias{rGEOM}
\title{
Geometric distribution for fitting a GAMLSS model
}
\description{
The function \code{GEOM()} defines the Geometric distribution, a one parameter 
distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}, with mean equal to the parameter \code{mu}. 
The functions \code{dGEOM}, \code{pGEOM}, \code{qGEOM} and \code{rGEOM} define 
the density, distribution function, quantile function and random generation for 
the \code{GEOM} parameterization of the Geometric distribution. 
}
\usage{

GEOM(mu.link = "log")
dGEOM(x, mu = 2, log = FALSE)
pGEOM(q, mu = 2, lower.tail = TRUE, log.p = FALSE)
qGEOM(p, mu = 2, lower.tail = TRUE, log.p = FALSE, max.value=10000)
rGEOM(n, mu = 2)
}

\arguments{
	\item{mu.link}{Defines the \code{mu.link}, with \code{log} link as the default for the \code{mu} parameter}
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
  \item{max.value}{constant; generates a sequence of values for the cdf function.}
}
\details{
The parameterization of the Geometric distribution in the function \code{GEOM} is
\deqn{f(y|\mu) = (1-p)^y\,p}{f(y|mu) = (1-p)^y p}
where
\eqn{p = 1/(mu+1)}
for \eqn{y>=0} and \eqn{mu>0}.
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

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R. Accompanying documentation in the current GAMLSS help files, (see also \url{http://www.gamlss.org/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R. \emph{Journal of Statistical Software}, Vol. 23, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}. 

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
}
\keyword{distribution}
\keyword{regression}
