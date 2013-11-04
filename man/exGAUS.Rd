\name{exGAUS}
\alias{exGAUS}
\alias{dexGAUS}
\alias{pexGAUS}
\alias{qexGAUS}
\alias{rexGAUS}


\title{The ex-Gaussian distribution}
\description{
The ex-Gaussian distribution is often used by psychologists to model response time (RT). It is defined by adding two 
random variables, one from a normal distribution and the other from an exponential. The parameters \code{mu} and 
\code{sigma} are the mean and standard deviation from the normal distribution variable while the parameter \code{nu} 
is the mean of the exponential variable. 
The functions \code{dexGAUS}, \code{pexGAUS}, \code{qexGAUS} and \code{rexGAUS} define the density, distribution function,
 quantile function and random generation for the ex-Gaussian distribution. 
      
}
\usage{
exGAUS(mu.link = "identity", sigma.link = "log", nu.link = "log")
dexGAUS(x, mu = 5, sigma = 1, nu = 1, log = FALSE)
pexGAUS(q, mu = 5, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE)
qexGAUS(p, mu = 5, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE)
rexGAUS(n, mu = 5, sigma = 1, nu = 1, ...)
}

\arguments{
 \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. }
  \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter.}
  \item{nu.link}{Defines the  \code{nu.link}, with "log" link as the default for the \code{nu} parameter. 
                   Other links are "inverse", "identity", "logshifted" (shifted from one) and "own"}
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of \code{mu} parameter values }
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of \code{nu} parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
  \item{\dots}{for extra arguments}
}
\details{

The probability density function of the ex-Gaussian distribution, (\code{exGAUS}), is defined as 
  
\deqn{f(y|\mu,\sigma,\nu)=\frac{1}{\nu} e^{\frac{\mu-y}{\nu}+\frac{\sigma^2}{2 \nu^2}} \Phi(\frac{y-\mu}{\sigma}-\frac{\sigma}{\nu})}{f(y|mu,sigma,nu)=(1/nu)*exp(((mu-y)/nu)+(sigma^2/(2*nu^2)))*Phi(((y-mu)/sigma)+(sigma/tau))}

where \eqn{\Phi}{Phi} is the cdf of the standard normal distribution, 
for \eqn{-\infty<y<\infty}{-Inf<y<Inf}, \eqn{-\infty<\mu<\infty}{-Inf<mu<Inf}, \eqn{\sigma>0} and \eqn{\nu>0}.   
}
\value{
\code{exGAUS()}  returns a \code{gamlss.family} object which can be used to fit ex-Gaussian distribution in the \code{gamlss()} function. 
  \code{dexGAUS()} gives the density, \code{pexGAUS()} gives the distribution  function, 
  \code{qexGAUS()} gives the quantile function, and \code{rexGAUS()}
  generates random deviates. 
  
}
\references{ 

Cousineau, D. Brown, S. and Heathecote A. (2004) Fitting distributions using maximum likelihood: Methods and packages, 
\emph{Behavior Research Methods, Instruments and Computers}, \bold{46}, 742-756.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }
\author{Mikis Stasinopoulos and Bob Rigby  }
\note{ The mean of the ex-Gaussian is \eqn{\mu+\nu}{mu+nu} and the variance is \eqn{\sigma^2+\nu^2}{sigma^2+nu^2}.
}
\seealso{ \code{\link{gamlss.family}}, \code{\link{BCCG}}, \code{\link{GA}}, 
\code{\link{IG}}  \code{\link{LNO}}}
\examples{
exGAUS()   # 
y<- rexGAUS(100, mu=300, nu=100, sigma=35)
hist(y)
# library(gamlss)
# m1<-gamlss(y~1, family=exGAUS) 
# plot(m1)
curve(dexGAUS(x, mu=300 ,sigma=35,nu=100), 100, 600, 
 main = "The ex-GAUS  density mu=300 ,sigma=35,nu=100")
plot(function(x) pexGAUS(x, mu=300,sigma=35,nu=100), 100, 600, 
 main = "The ex-GAUS  cdf mu=300, sigma=35, nu=100")
}
\keyword{distribution}% 
\keyword{regression}% 
