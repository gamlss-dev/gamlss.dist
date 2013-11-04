\name{ZAGA}
\Rdversion{1.1}
\alias{ZAGA}
\alias{plotZAGA}
\alias{dZAGA}
\alias{pZAGA}
\alias{qZAGA}
\alias{rZAGA}
\alias{meanZAGA}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The zero adjusted Gamma  distribution for fitting a GAMLSS model
}
\description{
The function \code{ZAGA()} defines the  zero adjusted  Gamma distribution, a three parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
The zero adjusted Gamma distribution is similar to the Gamma distribution
but allows zeros as y values. The extra parameter \code{nu} models 
the probabilities at zero.    
The functions \code{dZAGA}, \code{pZAGA}, \code{qZAGA} and \code{rZAGA} define the density, distribution function,
quartile function and random
generation for the \code{ZAGA} parameterization of the zero adjusted Gamma distribution. 
\code{plotZAGA} can be used to plot the distribution. \code{meanZAGA} calculates the expected value of the response for a fitted model. 
}
\usage{
ZAGA(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZAGA(x, mu = 1, sigma = 1, nu = 0.1, log = FALSE)
pZAGA(q, mu = 1, sigma = 1, nu = 0.1, lower.tail = TRUE, 
      log.p = FALSE)
qZAGA(p, mu = 1, sigma = 1, nu = 0.1, lower.tail = TRUE, 
       log.p = FALSE, 
       upper.limit = mu + 10 * sqrt(sigma^2 * mu^2))
rZAGA(n, mu = 1, sigma = 1, nu = 0.1, ...)
plotZAGA(mu = 5, sigma = 1, nu = 0.1, from = 0, to = 10, 
          n = 101, ...)
meanZAGA(obj)

}
%- maybe also 'usage' for other objects documented here.
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
  \item{obj}{ a fitted \code{gamlss} object}              
  \item{\dots}{\code{\dots} can be used to pass the uppr.limit argument to \code{qIG}  }      
}
\details{
The Zero adjusted GA distribution is given as 
\deqn{f(y|\mu,\sigma\,\nu)=\nu}{f(y|mu,sigma,nu)=nu} if (y=0)
\deqn{f(y|\mu,\sigma,\nu)=(1-\nu)\left[\frac{1}{(\sigma^2 \mu)^{1/\sigma^2}}\hspace{1mm}\frac{y^{\frac{1}{\sigma^2}-1}\hspace{1mm} e^{-y/(\sigma^2 \mu)}}{\Gamma{(1/\sigma^2)}} \right]}{f(y|mu,sigma,nu)=(1-nu)*(y^((1/sigma^2)-1)*exp[-y/((sigma^2)*mu)])/((sigma^2*mu)^(1/sigma^2) Gamma(1/sigma^2))}  otherwise 

for \eqn{y=(0,\infty)}{y=(0,Inf)}, \eqn{\mu>0}{mu>0}, \eqn{\sigma>0}{sigma>0} and \eqn{0< \nu< 1}{0<nu<1}. 
\eqn{E(y)=(1-\nu)\mu}{E(y)=(1-nu)*mu} and \eqn{Var(y)=(1-\nu)\mu^2(\nu+\sigma^2)}{Var(y)=(1-nu)*mu^2*(nu+sigma^2)}. 
 
}
\value{
The function \code{ZAGA} returns a \code{gamlss.family} object which can be used to fit a 
zero adjusted Gamma distribution in the \code{gamlss()} function. 
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{
Bob Rigby and Mikis Stasinopoulos
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gamlss.family}}, \code{\link{GA}}, \code{\link{ZAIG}}
}

\examples{
ZAGA()# gives information about the default links for the ZAGA distribution
# plotting the function
PPP <- par(mfrow=c(2,2))
plotZAGA(mu=1, sigma=.5, nu=.2, from=0,to=3)
#curve(dZAGA(x,mu=1, sigma=.5, nu=.2), 0,3) # pdf
curve(pZAGA(x,mu=1, sigma=.5, nu=.2), 0,3,  ylim=c(0,1)) # cdf
curve(qZAGA(x,mu=1, sigma=.5, nu=.2), 0,.99) # inverse cdf
y<-rZAGA(100, mu=1, sigma=.5, nu=.2) # randomly generated values
hist(y)
par(PPP)
# check that the  positive part sums up to .8 (since nu=0.2)  
integrate(function(x) dZAGA(x,mu=1, sigma=.5, nu=.2), 0,Inf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
