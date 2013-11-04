\name{RG}
\alias{RG}
\alias{dRG}
\alias{pRG}
\alias{qRG}
\alias{rRG}



\title{The Reverse Gumbel distribution for fitting a GAMLSS  }
\description{
The function \code{RG} defines the reverse Gumbel distribution, a two parameter distribution, for a
\code{gamlss.family} object to be used in GAMLSS fitting using the
function  \code{gamlss()}.
The functions \code{dRG}, \code{pRG}, \code{qRG} and \code{rRG} define the density, distribution function, quantile function and random
generation for the specific parameterization of the reverse Gumbel distribution. 
}
\usage{
RG(mu.link = "identity", sigma.link = "log")
dRG(x, mu = 0, sigma = 1, log = FALSE)
pRG(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qRG(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rRG(n, mu = 0, sigma = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter. other available link is "inverse", "log" and "own"}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter, other links are the "inverse", "identity" and "own"}
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
 The specific parameterization of the reverse Gumbel  distribution used in \code{RG} is
 \deqn{f(y|\mu,\sigma)= \frac{1}{\sigma} \hspace{1mm}
\exp\left\{-\left(\frac{y-\mu}{\sigma}\right)-\exp\left[-\frac{(y-\mu)}{\sigma}\right]\right\}}{f(y|mu,sigma)=(1/sigma)*exp(-((y-mu)/sigma)-exp(-((y-mu)/sigma)))  }
for \eqn{y=(-\infty,\infty)}{y=(-Inf,+Inf)}, \eqn{\mu=(-\infty,+\infty)}{\mu=(-Inf,+Inf)} and \eqn{\sigma>0}.
}

\value{
\code{RG()} returns a \code{gamlss.family} object which can be used to fit a Gumbel distribution in the \code{gamlss()} function.
  \code{dRG()} gives the density, \code{pGU()} gives the distribution
     function, \code{qRG()} gives the quantile function, and \code{rRG()}
     generates random deviates.
     }
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).   

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{ Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Calliope Akantziliotou }
\note{ The mean of the distribution is  \eqn{\mu+0.57722 \sigma}{mu-0.57722*sigma} and the variance is 
\eqn{\pi^2 \sigma^2/6}{pi^2 sigma^2/6}.   }

\seealso{  \code{\link{gamlss.family}} }
\examples{

plot(function(x) dRG(x, mu=0,sigma=1), -3, 6, 
 main = "{Reverse Gumbel  density mu=0,sigma=1}")
RG()# gives information about the default links for the Gumbel distribution      
dat<-rRG(100, mu=10, sigma=2) # generates 100 random observations 
# library(gamlss)
# gamlss(dat~1,family=RG) # fits a constant for each parameter mu and sigma 

}
\keyword{distribution}
\keyword{regression}% 
