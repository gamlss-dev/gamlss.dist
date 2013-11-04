\name{EXP}
\alias{EXP}
\alias{pEXP}
\alias{dEXP}
\alias{qEXP}
\alias{rEXP}

\title{ Exponential distribution for fitting a GAMLSS }
\description{
  The function EXP defines the exponential distribution, a one parameter distribution for a 
\code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}.
The \code{mu} parameter represents the mean of the distribution. 
The functions \code{dEXP}, \code{pEXP}, \code{qEXP} and \code{rEXP} define the density, 
distribution function, quantile function and random generation for the specific parameterization 
of the exponential distribution defined by function EXP.
}
\usage{
EXP(mu.link ="log")
dEXP(x, mu = 1, log = FALSE)
pEXP(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
qEXP(p, mu = 1, lower.tail = TRUE, log.p = FALSE)
rEXP(n, mu = 1)
}

\arguments{
  \item{mu.link}{Defines the mu.link, with "log" link as the default for the \code{mu} parameter,                                   other links are "inverse" and "identity" }
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p). }
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x] }
  \item{p}{vector of probabilities }
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required}
}

\details{
  The specific parameterization of the exponential distribution used in EXP is
\deqn{f(y|\mu)=\frac{1}{\mu} \exp\left\{-\frac{y}{\mu}\right\}}{f(y|mu)=1/mu*exp(-y/mu)},
for y>0, \eqn{\mu>0}{mu>0}.
}

\value{
  EXP() returns a gamlss.family object which can be used to fit an exponential distribution in 
the gamlss() function. dEXP() gives the density, pEXP() gives the distribution function, qEXP() gives the quantile function, 
and rEXP() generates random deviates. 
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby 
        \email{r.rigby@londonmet.ac.uk} and Nicoleta Motpan}

\seealso{ \code{\link{gamlss.family}}}

\examples{
y<-rEXP(1000,mu=1) # generates 1000 random observations 
hist(y)
# library(gamlss)
# histDist(y, family=EXP) 
}
\keyword{distribution}
\keyword{regression}%
