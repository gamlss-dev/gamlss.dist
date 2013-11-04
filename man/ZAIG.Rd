\name{ZAIG}
\alias{ZAIG}
\alias{plotZAIG}
\alias{dZAIG}
\alias{pZAIG}
\alias{qZAIG}
\alias{rZAIG}
\alias{meanZAIG}


\title{The zero adjusted Inverse Gaussian  distribution for fitting a GAMLSS model
}

\description{
 The function \code{ZAIG()} defines the  zero adjusted Inverse Gaussian distribution, a three parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. The zero adjusted Inverse Gaussian distribution is similar to the Inverse Gaussian distribution
but allows zeros as y values. The extra parameter models 
the probabilities at zero.    
The functions \code{dZAIG}, \code{pZAIG}, \code{qZAIG} and \code{rZAIG} define the density, distribution function, quantile function and random
generation for the \code{ZAIG} parameterization of the zero adjusted Inverse Gaussian distribution. 
\code{plotZAIG} can be used to plot the distribution. \code{meanZAIG} calculates the expected value of the response for a fitted model. 
}
\usage{
ZAIG(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZAIG(x, mu = 1, sigma = 1, nu = 0.1, log = FALSE)
pZAIG(q, mu = 1, sigma = 1, nu = 0.1, lower.tail = TRUE, log.p = FALSE)
qZAIG(p, mu = 1, sigma = 1, nu = 0.1, lower.tail = TRUE, log.p = FALSE, 
     upper.limit = mu + 10 * sqrt(sigma^2 * mu^3))
rZAIG(n, mu = 1, sigma = 1, nu = 0.1, ...)
plotZAIG(mu = 5, sigma = 1, nu = 0.1, from = 0, to = 10, n = 101, ...)
meanZAIG(obj)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "log" link as the default for the mu parameter }
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{Defines the  \code{nu.link}, with "logit" link as the default for the sigma parameter}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values }
  \item{nu}{vector of probability at zero parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{upper.limit}{the argument \code{upper.limit} sets the upper limit in the golden section search for q, 
           the default is 10 time its standard deviation}
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}  
  \item{from}{where to start plotting the distribution from}
  \item{to}{up to  where to plot the distribution  }
  \item{obj}{ a fitted \code{BEINF} object}              
  \item{\dots}{\code{\dots} can be used to pass the uppr.limit argument to \code{qIG}  }       
  
  
}
\details{
The Zero adjusted IG distribution is given as 
\deqn{f(y|\mu,\sigma\,\nu)=\nu}{f(y|mu,sigma,nu)=nu} if (y=0)
\deqn{f(y|\mu,\sigma,\nu)=(1-\nu)\frac{1}{\sqrt{2 \pi \sigma^2 y^3}} \exp(- \frac{(y-\mu)^2}{2\mu^2\sigma^2y})}{f(y|mu,sigma,nu)=(1-nu)*(2*pi*sigma^2*y^3)^.5 exp(-(y-mu)^2/(2*mu^2sigma^2*y))}  otherwise 

for \eqn{y=(0,\infty)}{y=(0,Inf)}, \eqn{\mu>0}{mu>0}, \eqn{\sigma>0}{sigma>0} and \eqn{0< \nu< 1}{0<nu<1}. 
\eqn{E(y)=(1-\nu)\mu}{E(y)=(1-nu)*mu} and \eqn{Var(y)=(1-\nu)\mu^2(\nu+\mu\sigma^2)}{Var(y)=(1-nu)*mu^2*(nu+mu*sigma^2)}. 
 
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a zero adjusted inverse Gaussian distribution in the \code{gamlss()} function. 
}
\references{ 
Heller, G.  Stasinopoulos M and Rigby R.A. (2006)
The zero-adjusted Inverse Gaussian distribution as a model for
insurance claims. in \emph{Proceedings of the 21th International
Workshop on Statistial Modelling}, eds J. Hinde, J. Einbeck and  J.
Newell, pp 226-233,  Galway, Ireland.


Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

}
\author{Bob Rigby and Mikis Stasinopoulos}
 

\seealso{  \code{\link{gamlss.family}}, \code{\link{IG}} }
\examples{
ZAIG()# gives information about the default links for the ZAIG distribution
# plotting the distribution
plotZAIG( mu =10 , sigma=.5, nu = 0.1, from = 0, to=10, n = 101)
# plotting the cdf
plot(function(y) pZAIG(y, mu=10 ,sigma=.5, nu = 0.1 ), 0, 1)
# plotting the inverse cdf
plot(function(y) qZAIG(y, mu=10 ,sigma=.5, nu = 0.1 ), 0.001, .99)
# generate random numbers
dat <- rZAIG(100,mu=10,sigma=.5, nu=.1)
# fit a model to the data 
# library(gamlss)
# m1<-gamlss(dat~1,family=ZAIG)
# meanZAIG(m1)[1]

}
\keyword{distribution}
\keyword{regression}
