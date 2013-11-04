\name{YULE}
\alias{YULE}
\alias{dYULE}
\alias{pYULE}
\alias{qYULE}
\alias{rYULE}
\title{Yule distribution for fitting a GAMLSS model}
\description{
The function \code{YULE} defines the Yule distribution, a one parameter 
distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}, with mean equal to the parameter \code{mu}. 
The functions \code{dYULE}, \code{pYULE}, \code{qYULE} and \code{rYULE} define 
the density, distribution function, quantile function and random generation for 
the \code{YULE} parameterization of the Yule distribution. 
}
\usage{
YULE(mu.link = "log")
dYULE(x, mu = 2, log = FALSE)
pYULE(q, mu = 2, lower.tail = TRUE, log.p = FALSE) 
qYULE(p, mu = 2, lower.tail = TRUE, log.p = FALSE, 
    max.value = 10000)
rYULE(n, mu = 2)
}
\arguments{
 \item{mu.link}{Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{x}{vector of (non-negative integer) quantiles.}
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of random values to return.}
  \item{mu}{vector of positive \code{mu} values.}
  \item{lower.tail}{logical; if \code{TRUE} (default) probabilities are \eqn{P[Y\leq y]}, otherwise, \eqn{P[Y>y]}.}
  \item{log, log.p}{logical; if \code{TRUE} probabilities p are given as log(p).}
  \item{max.value}{constant; generates a sequence of values for the cdf function.}
}
\details{
The Yule distribution has density
\deqn{p(y)=beta(lambda+1, x+1)/beta(lambda, 1)}
where 
\deqn{lambda = (mu+1)/mu}
for \eqn{y=0,1,2,\ldots} and \eqn{mu>0}.  

}
\value{
Returns a \code{gamlss.family} object which can be used to fit a Yule distribution in the \code{gamlss()} function.
}
\references{
Wimmer, G. and Altmann, G. (1999) \emph{Thesaurus of univariate discrete probability distributions.} Stamm.
}
\author{
Fiona McElduff, Bob Rigby and Mikis Stasinopoulos.
}

\seealso{
\code{\link{gamlss.family}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(0,20,1)
plot(y, dYULE(y), type="h")
q <- seq(0, 20, 1)
plot(q, pYULE(q), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p , qYULE(p), type="s")
dat <- rYULE(100)
hist(dat)
#summary(gamlss(dat~1, family=YULE))
}
\keyword{distribution}
\keyword{regression}