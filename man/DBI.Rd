\name{DBI}
\alias{DBI}
\alias{pDBI}
\alias{dDBI}
\alias{qDBI}
\alias{rDBI}
\alias{GetBI_C}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Double binomial distribution
}
\description{
The function \code{DBI()} defines the double binomial distribution, a two parameters distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. The functions \code{dDBI}, \code{pDBI}, \code{qDBI} and \code{rDBI} define the density, distribution function, quantile function and random generation for the double binomial, \code{DBI()}, distribution. The function \code{GetBI_C} calculates numericaly the constant of proportionality needed for the pdf to sum up to 1.
}
\usage{
DBI(mu.link = "logit", sigma.link = "log")
dDBI(x, mu = 0.5, sigma = 1, bd = 2, log = FALSE)
pDBI(q, mu = 0.5, sigma = 1, bd = 2, lower.tail = TRUE, 
       log.p = FALSE)
qDBI(p, mu = 0.5, sigma = 1, bd = 2, lower.tail = TRUE, 
       log.p = FALSE)
rDBI(n, mu = 0.5, sigma = 1, bd = 2)       
GetBI_C(mu, sigma, bd)       
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mu.link}{the link function for \code{mu} with default \code{log}}
\item{sigma.link}{the link function for \code{sigma} with default \code{log}}
\item{x, q}{vector of (non-negative integer) quantiles}
\item{bd}{vector of binomial denominator}
\item{p}{vector of probabilities}
\item{mu}{the \code{mu} parameter}
\item{sigma}{the \code{sigma} parameter}
\item{lower.tail}{logical; if \code{TRUE} (default), probabilities are P[X <= x], otherwise, P[X > x]}
\item{log, log.p}{logical; if \code{TRUE}, probabilities p are given as log(p)}
\item{n}{how many random values  to  generate}
}
\details{
 The definition for the Double Poisson distribution first introduced by Efron (1986) is:   
 
 \deqn{p_Y(y|n, \mu,\sigma)= frac{1}{C(n,\mu,\sigma)} \frac{\Gamma(n+1)}{\Gamma(y+1)\Gamma(n-y+1)} \frac{y^y \left(n-y \right)^{n-y}}{n^n}
 \frac{n^{n/\sigma} \mu^{y/\sigma} \left( 1-\mu\right)^{(n-y)/\sigma}}
{y^{y/\sigma} \left( n-y\right)^{(n-y)/\sigma}}}{f(y| n, \mu,\sigma)=[1/C(n,\mu,\sigma)] [\Gamma(n+1)/\Gamma(y+1)\Gamma(n-y+1)] [y^y (n-y)^{n-y}/n^n][n^{n/\sigma} \mu^{y/\sigma} ( 1-\mu)^{(n-y)/\sigma}/ y^{y/\sigma} ( n-y)^{(n-y)/\sigma}]} 
for \eqn{y=0,1,2,\ldots,\infty}{y=0,1,2, ...,Inf}, \eqn{\mu>0} and \eqn{\sigma>0} where \eqn{C}{C} is the constant of proportinality which is calculated numerically using the function \code{GetBI_C()}.  

}
\value{
The function \code{DBI} returns a \code{gamlss.family} object which can be used to fit a double binomial distribution in the \code{gamlss()} function. 
}
\references{

Efron, B., 1986. Double exponential families and their use in generalized linear Regression. Journal of the American Statistical Association 81 (395), 709-721.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. 

}
\author{
Mikis Stasinopoulos, Bob Rigby, Marco Enea and Fernanda de Bastiani
}
\seealso{\code{\link{BI}},\code{\link{BB}}}
\examples{
DBI()
x <- 0:20
# underdispersed DBI
plot(x, dDBI(x, mu=.5, sigma=.2, bd=20), type="h", col="green", lwd=2)
# binomial
lines(x+0.1, dDBI(x, mu=.5, sigma=1, bd=20), type="h", col="black", lwd=2)
# overdispersed DBI
lines(x+.2, dDBI(x, mu=.5, sigma=2, bd=20), type="h", col="red",lwd=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
