\name{PARETO2}
\alias{PARETO2}
\alias{dPARETO2}
\alias{pPARETO2}
\alias{qPARETO2}
\alias{rPARETO2}
\alias{PARETO2o}
\alias{dPARETO2o}
\alias{pPARETO2o}
\alias{qPARETO2o}
\alias{rPARETO2o}
\alias{PARETO}
\alias{dPARETO}
\alias{pPARETO}
\alias{qPARETO}
\alias{rPARETO}
\alias{PARETO1}
\alias{dPARETO1}
\alias{pPARETO1}
\alias{qPARETO1}
\alias{rPARETO1}
\alias{PARETO1o}
\alias{dPARETO1o}
\alias{pPARETO1o}
\alias{qPARETO1o}
\alias{rPARETO1o}
\title{
Pareto distributions for fitting in GAMLSS}
\description{

The functions \code{PARETO()} defines the one parameter Pareto distribution for \code{y>1}.

The functions \code{PARETO1()} defines the one parameter Pareto distribution for \code{y>0}.

The functions \code{PARETOo1()} defines the one parameter Pareto distribution for \code{y>mu} therefor requires \code{mu} to be fixed.

The functions \code{PARETO2()} and  \code{PARETO2o()} define the Pareto Type 2 distribution, for \code{y>0}, a two parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}.
The parameters are  \code{mu} and \code{sigma} in both functions but the parameterasation different.  The \code{mu} is identical for both \code{PARETO2()} and  \code{PARETO2o()}. The \code{sigma} in  \code{PARETO2o()} is the inverse of the \code{sigma} in \code{PARETO2()} and coresponse to the usual parameter \code{alpha} of the Pareto distribution. 


The functions \code{dPARETO2}, \code{pPARETO2}, \code{qPARETO2} and \code{rPARETO2} define the density, distribution function, quantile function and random generation for the \code{PARETO2} parameterization of the Pareto type 2 distribution while the functions \code{dPARETO2o}, \code{pPARETO2o}, \code{qPARETO2o} and \code{rPARETO2o} define the density, distribution function, quantile function and random generation for the original \code{PARETO2o} parameterization of the Pareto type 2 distribution.
}
\usage{
PARETO(mu.link = "log")
dPARETO(x, mu = 1, log = FALSE)
pPARETO(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
qPARETO(p, mu = 1, lower.tail = TRUE, log.p = FALSE)
rPARETO(n, mu = 1)

PARETO1(mu.link = "log")
dPARETO1(x, mu = 1, log = FALSE)
pPARETO1(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
qPARETO1(p, mu = 1, lower.tail = TRUE, log.p = FALSE)
rPARETO1(n, mu = 1)

PARETO1o(mu.link = "log", sigma.link = "log")
dPARETO1o(x, mu = 1, sigma = 0.5, log = FALSE)
pPARETO1o(q, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
qPARETO1o(p, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
rPARETO1o(n, mu = 1, sigma = 0.5)

PARETO2(mu.link = "log", sigma.link = "log")
dPARETO2(x, mu = 1, sigma = 0.5, log = FALSE)
pPARETO2(q, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
qPARETO2(p, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
rPARETO2(n, mu = 1, sigma = 0.5)

PARETO2o(mu.link = "log", sigma.link = "log")
dPARETO2o(x, mu = 1, sigma = 0.5, log = FALSE)
pPARETO2o(q, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
qPARETO2o(p, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
rPARETO2o(n, mu = 1, sigma = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "`"' link sa the default for the mu parameter}
  \item{sigma.link}{Defines the \code{sigma.link}, with "`log"' as the default for the sigma parameter}
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
}
\details{
The parameterization of the one parameter Pareto distribution in the function \code{PARETO} is:

\deqn{f(y|\mu) = \mu y^{\mu+1}}{f(y|mu) = (mu) y^(mu+1)}

for \eqn{y>1} and \eqn{\mu>0}.


The parameterization of the Pareto Type 1 original distribution in the function \code{PARETO1o} is:

\deqn{f(y|\mu, \sigma) = \frac{\sigma \mu^{\sigma}}{y^{\sigma+1}}} 

for \eqn{y>=0}, \eqn{\mu>0} and \eqn{\sigma>0} see pp. 430-431 of Rigby et al. (2019).

The parameterization of the Pareto Type 2 original distribution in the function \code{PARETO2o} is:

\deqn{f(y|\mu, \sigma) = \frac{\sigma \mu^{\sigma}}{(y+\mu)^{\sigma+1}}} 

for \eqn{y>=0}, \eqn{\mu>0} and \eqn{\sigma>0} see pp. 432-433 of Rigby et al. (2019).

The parameterization of the Pareto Type 2 distribution in the function \code{PARETO2} is:

\deqn{f(y|\mu, \sigma) = \frac{1}{\sigma} \mu^{\frac{1}{\sigma}} \, (y+\mu)^{-\frac{1
}{\sigma+1}}}{f(y | mu, sigma) = (1/sigma) mu^(1/sigma) (y+mu)^(-(1/sigma+1))}

for \eqn{y>=0}, \eqn{\mu>0} and \eqn{\sigma>0} see pp.433-434 The parameterization of the Pareto Type 1 original distribution in the function \code{PARETO1o} is:

\deqn{f(y|\mu, \sigma) = \frac{\sigma \mu^{\sigma}}{y^{\sigma+1}}} 

for \eqn{y>=0}, \eqn{\mu>0} and \eqn{\sigma>0} see pp. 430-431 of Rigby et al. (2019).
}
\value{
returns a gamlss.family object which can be used to fit a Pareto type 2 distribution in the \code{gamlss()} function.
}
\author{
Fiona McElduff, Bob Rigby and Mikis Stasinopoulos}

\references{

Johnson, N., Kotz, S., and Balakrishnan, N. (1997). \emph{Discrete Multivariate
Distributions.} Wiley-Interscience, NY, USA.

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

\seealso{
\code{\link{gamlss.family}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(0.2,20,0.2)
plot(y, dPARETO2(y), type="l" , lwd=2)
q<-seq(0,20,0.2)
plot(q, pPARETO2(q), ylim=c(0,1), type="l", lwd=2) 
p<-seq(0.0001,0.999,0.05)
plot(p, qPARETO2(p), type="l", lwd=2)
dat <- rPARETO2(100)
hist(rPARETO2(100), nclass=30)
#summary(gamlss(a~1, family="PARETO2"))
}
\keyword{distribution}
\keyword{regression}
