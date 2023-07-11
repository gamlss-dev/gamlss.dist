\name{gamlss.dist-package}
\alias{gamlss.dist-package}
\alias{gamlss.dist}
\docType{package}
\title{
\packageTitle{gamlss.dist}
}
\description{
\packageDescription{gamlss.dist}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{gamlss.dist}
\packageIndices{gamlss.dist}
}
\author{
\packageAuthor{gamlss.dist}

Maintainer: \packageMaintainer{gamlss.dist}
}
\references{
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
\keyword{ package }
\seealso{
\code{\link[gamlss.dist]{gamlss.family}}
}
\examples{
# pdf plot
plot(function(y) dSICHEL(y, mu=10, sigma = 0.1 , nu=1 ), 
              from=0, to=30, n=30+1, type="h")
# cdf plot
PPP <- par(mfrow=c(2,1))
plot(function(y) pSICHEL(y, mu=10, sigma =0.1, nu=1 ), 
             from=0, to=30, n=30+1, type="h") # cdf
cdf<-pSICHEL(0:30, mu=10, sigma=0.1, nu=1) 
sfun1  <- stepfun(1:30, cdf, f = 0)
plot(sfun1, xlim=c(0,30), main="cdf(x)")
par(PPP)
}
