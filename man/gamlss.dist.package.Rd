\name{gamlss.dist-package}
\alias{gamlss.dist-package}
\alias{gamlss.dist}
\docType{package}
\title{
The GAMLSS distributions
}
\description{This package contains all distributions to be used for GAMLSS models.    
Each distributions has its probability function, \code{d}, its commutative probability function, \code{p}, the 
inverse of the commutative probability function, \code{q}, its random
generation function, \code{r}, and also the \code{gamlss.family} generating function  
}
\details{
\tabular{ll}{
Package: \tab gamlss.dist\cr
Type: \tab Package\cr
Version: \tab 1.5.0\cr
Date: \tab 2006-12-13\cr
License: \tab GPL (version 2 or later)\cr
}
This package is design to be used with the package \pkg{gamlss} but the \code{d}, \code{p}, \code{q} and \code{r} functions
can be used separately.  
}
\author{
Mikis Stasinopoulos <d.stasinopoulos@londonmet.ac.uk>, Bob Rigby <r.rigby@londonmet.ac.uk> with
        contributions from Calliope Akantziliotou  and Raydonal Ospina <rospina@ime.usp.br>.

Maintainer: Mikis Stasinopoulos <d.stasinopoulos@londonmet.ac.uk>
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/})

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\seealso{ \code{\link{gamlss.family}} } 

\examples{
plot(function(y) dSICHEL(y, mu=10, sigma = 0.1 , nu=1 ), from=0, to=30, n=30+1, type="h") # pdf
# cdf plot
PPP <- par(mfrow=c(2,1))
plot(function(y) pSICHEL(y, mu=10, sigma =0.1, nu=1 ), from=0, to=30, n=30+1, type="h") # cdf
cdf<-pSICHEL(0:30, mu=10, sigma=0.1, nu=1) 
sfun1  <- stepfun(1:30, cdf, f = 0)
plot(sfun1, xlim=c(0,30), main="cdf(x)")
par(PPP)
}
\keyword{ package }
\keyword{ distribution}
