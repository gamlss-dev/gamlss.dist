\name{GE}
\alias{GE}
\alias{dGE}
\alias{pGE}
\alias{qGE}
\alias{rGE}
\title{
Geometric distribution for fitting a GAMLSS
}
\description{
The function \code{GE()} defines the Yule distribution, a one parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}, with mean equal to the parameter \code{mu}. The functions \code{dGE}, \code{pGE}, \code{qGE} and \code{rGE} define the density, distribution function, quantile function and random generation for the \code{GE} parameterization of the Geometric distribution. 
}
\usage{

GE(mu.link = "log")
dGE(x, mu = 2, log = FALSE)
pGE(q, mu = 2, lower.tail = TRUE, log.p = FALSE)
qGE(p, mu = 2, lower.tail = TRUE, log.p = FALSE, max.value=10000)
rGE(n, mu = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{mu.link}{Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
  \item{max.value}{constant; generates a sequence of values for the cdf function}
}
\details{
The parameterization of the Geometric distribution in the function \code{GE} is
\deqn{f(y|mu) = (1-p)^y p}
where
\eqn{p = 1/(mu+1)}
for \eqn{y>=0} and \eqn{mu>0}.
}
\value{
returns a gamlss.family object which can be used to fit a Geometric distribution in the \code{gamlss()} function.}
\author{Bob Rigby \email{r.rigby@londonmet.ac.uk}, Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Fiona McElduff \email{F.Mcelduff@londonmet.ac.uk} and Kalliope Akantziliotou
}
\references{
Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), \emph{Appl. Statist.}, 54, part 3, pp 507--554. 

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R. Accompanying documentation in the current GAMLSS help files, (see also \url{http://www.gamlss.com/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R. \emph{Journal of Statistical Software}, Vol. 23, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}. 

}

\seealso{
\code{\link{gamlss.family}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(0,20,1)
plot(y, dGE(y), type="h")
q <- seq(0, 20, 1)
plot(q, pGE(q), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p , qGE(p), type="s")
dat <- rGE(100)
hist(dat)
#summary(gamlss(dat~1, family="GE"))
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
