\name{SN2}
\alias{SN2}
\alias{dSN2}
\alias{pSN2}
\alias{qSN2}
\alias{rSN2}
\title{
Skew Normal Type 2 distribution for fitting a GAMLSS
}
\description{
The function \code{SN2()} defines the Skew Normal Type 2 distribution, a three parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}, with parameters \code{mu}, \code{sigma} and \code{nu}. The functions \code{dSN2}, \code{pSN2}, \code{qSN2} and \code{rSN2} define the density, distribution function, quantile function and random generation for the \code{SN2} parameterization of the Skew Normal Type 2 distribution. 
}
\usage{

SN2(mu.link = "identity", sigma.link = "log", nu.link = "log")
dSN2(x, mu = 0, sigma = 1, nu = 2, log = FALSE)
pSN2(q, mu = 0, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)
qSN2(p, mu = 0, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)
rSN2(n, mu = 0, sigma = 1, nu = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{mu.link}{Defines the \code{mu.link}, with "`identity"' links the default for the mu parameter}
  \item{sigma.link}{Defines the \code{sigma.link}, with "`log"' as the default for the sigma parameter}
  \item{nu.link}{Defines the \code{nu.link}, with "`log"' as the default for the sigma parameter}
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of scale parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
}
\details{
The parameterization of the Skew Normal Type 2 distribution in the function \code{SN2} is ...
}
\value{
returns a gamlss.family object which can be used to fit a Skew Normal Type 2 distribution in the \code{gamlss()} function.}
\author{
Mikis Stasinopoulos, Bob Rigby and Fiona McElduff.
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
}

\note{
This is a special case of the Skew Exponential Power type 3 distribution (\code{SEP3})where \code{tau=2}.
}

\seealso{
\code{\link{gamlss.family}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(-3,3,0.2)
plot(y, dSN2(y), type="l" , lwd=2)
q<-seq(-3,3,0.2)
plot(q, pSN2(q), ylim=c(0,1), type="l", lwd=2) 
p<-seq(0.0001,0.999,0.05)
plot(p, qSN2(p), type="l", lwd=2)
dat <- rSN2(100)
hist(rSN2(100), nclass=30)
}
\keyword{distribution}
\keyword{ regression}