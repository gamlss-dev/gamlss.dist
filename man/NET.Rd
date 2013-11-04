\name{NET}
\alias{NET}
\alias{dNET}
\alias{pNET}

\title{Normal Exponential t distribution (NET) for fitting a GAMLSS}
\description{
This function defines the  Power Exponential t distribution (NET), a four parameter distribution, for a \code{gamlss.family} object to be used for a  
GAMLSS fitting using the function \code{gamlss()}.  The functions \code{dNET}, 
\code{pNET} define the density and  distribution function the NET distribution. 
}
\usage{
NET(mu.link = "identity", sigma.link = "log")
pNET(q, mu = 5, sigma = 0.1, nu = 1, tau = 2)
dNET(x, mu = 0, sigma = 1, nu = 1.5, tau = 2, log = FALSE)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. Other links are "inverse", "log" and "own" }
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. Other links are "inverse", "identity" and "own"}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values }
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of \code{nu} parameter values}
  \item{tau}{vector of \code{tau} parameter values}
   \item{log}{ logical; if TRUE, probabilities p are given as log(p).}
}
\details{
The NET distribution was introduced by Rigby and Stasinopoulos (1994) as a robust distribution for a response
variable with heavier tails than the normal. The NET
distribution is the abbreviation of the Normal Exponential Student t distribution. 
The NET distribution is a four parameter continuous distribution, although in the GAMLSS implementation only 
the two parameters, \code{mu} and \code{sigma}, of the  distribution are modelled  with 
\code{nu} and \code{tau} fixed.  
The distribution takes its names because it is normal up to
\code{nu}, Exponential from \code{nu} to \code{tau} (hence \code{abs(nu)<=abs(tau)})  and Student-t with
\code{nu*tau-1} degrees of freedom after \code{tau}. Maximum
likelihood estimator of the third and forth parameter can be
obtained, using the GAMLSS functions, \code{find.hyper} or \code{prof.dev}.
}
\value{
 \code{NET()}  returns a \code{gamlss.family} object which can be used to fit a Box Cox Power Exponential distribution in the \code{gamlss()} function. 
  \code{dNET()} gives the density, \code{pNET()} gives the distribution
     function. 
}
\references{ 
Rigby, R. A. and Stasinopoulos, D. M. (1994), Robust fitting of an additive model for variance heterogeneity,
\emph{COMPSTAT : Proceedings in Computational Statistics}, editors:R. Dutter and W. Grossmann, pp 263-268, Physica,
Heidelberg.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou }


\seealso{  \code{\link{gamlss.family}}, \code{\link{BCPE}} }
\examples{
NET()   # 
data(abdom)
plot(function(x)dNET(x, mu=0,sigma=1,nu=2, tau=3), -5, 5)
plot(function(x)pNET(x, mu=0,sigma=1,nu=2, tau=3), -5, 5) 
# fit NET with nu=1 and tau=3
# library(gamlss)
#h<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=NET, 
#        data=abdom, nu.start=2, tau.start=3) 
#plot(h)
}
\keyword{distribution}
\keyword{regression}
