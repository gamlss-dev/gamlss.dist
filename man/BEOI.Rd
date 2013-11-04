\name{BEOI}
\alias{BEOI}
\alias{dBEOI}
\alias{pBEOI}
\alias{qBEOI}
\alias{rBEOI}
\alias{plotBEOI}
\alias{meanBEOI}


\title{The one-inflated beta distribution for fitting a GAMLSS}
\description{
The function \code{BEOI()} defines the one-inflated beta distribution, a
three parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}.
The one-inflated beta is similar to the beta distribution but allows ones as y values. This distribution
is an extension of the beta distribution using a parameterization of the beta law that is
indexed by mean and precision parameters (Ferrari and Cribari-Neto, 2004).
The extra parameter models the probability at one.
The functions \code{dBEOI}, \code{pBEOI}, \code{qBEOI} and \code{rBEOI} define the density,
distribution function, quantile function and random
generation for the \code{BEOI} parameterization of the one-inflated beta distribution. 
\code{plotBEOI} can be used to plot the distribution. \code{meanBEOI} calculates the expected value of 
the response for a fitted model. 
}
\usage{
BEOI(mu.link = "logit", sigma.link = "log", nu.link = "logit")

dBEOI(x, mu = 0.5, sigma = 1, nu = 0.1, log = FALSE)

pBEOI(q, mu = 0.5, sigma = 1, nu = 0.1, lower.tail = TRUE, log.p = FALSE)

qBEOI(p, mu = 0.5, sigma = 1, nu = 0.1, lower.tail = TRUE,
        log.p = FALSE)
        
rBEOI(n, mu = 0.5, sigma = 1, nu = 0.1)

plotBEOI(mu = .5, sigma = 1, nu = 0.1, from = 0.001, to = 1, n = 101, 
    ...)
    
meanBEOI(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the \code{mu} link function with default \code{logit}}
  \item{sigma.link}{the \code{sigma} link function with default \code{log}}
  \item{nu.link}{the \code{nu} link function with default \code{logit}}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values} 
  \item{sigma}{vector of precision parameter values}
  \item{nu}{vector of parameter values modelling the probability at one}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}   
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}  
  \item{from}{where to start plotting the distribution from}
  \item{to}{up to  where to plot the distribution  }
  \item{obj}{ a fitted \code{BEOI} object}
  \item{\dots}{ other graphical parameters for plotting}
}
\details{ The one-inflated beta distribution is given as
\deqn{f(y)=\nu}{f(y)=nu} if \eqn{(y=1)}{(y=1)}
\deqn{f(y|\mu,\sigma)=(1-\nu)\frac{\Gamma(\sigma)}{\Gamma(\mu \sigma)\Gamma((1-\mu) \sigma)} y^{\mu \sigma}(1-y)^{((1-\mu)\sigma)-1}}{f(y|mu,sigma)=(1-nu)*(Gamma(sigma)/Gamma(mu*sigma)*Gamma((1-mu)*sigma))*y^(mu*sigma-1)*(1-y)^(((1-mu)*sigma)-1)
}
if \eqn{y=(0,1)}{y=(0,1)}. The parameters satisfy \eqn{0<\mu<0}{0<mu<1}, \eqn{\sigma>0}{sigma>0} and \eqn{0<\nu< 1}{0<nu<1}.
 
Here \eqn{E(y)=\nu+(1-\nu)\mu}{E(y)=nu+(1-nu)*mu} and 
\eqn{Var(y)=(1-\nu)\frac{\mu(1-\mu)}{\sigma+1}+\nu(1-\nu)(1-\mu)^2}{Var(y)=(1-nu)*(mu*(1-mu))/(sigma+1) + nu*(1-nu)*(1-mu)^2}. 
}

\value{
returns a \code{gamlss.family} object which can be used to fit a one-inflated beta distribution in the \code{gamlss()} function. 
}
\references{

Ferrari, S.L.P., Cribari-Neto, F. (2004). Beta regression for
     modelling rates and proportions. \emph{Journal of Applied Statistics},
     \bold{31} (1), 799-815.

Ospina R. and Ferrari S. L. P. (2010) Inflated beta distributions, \emph{Statistical Papers}, 
    \bold{23},  111-126. 

Rigby, R. A. and  Stasinopoulos D. M. (2005).
Generalized additive models for location, scale and shape (with discussion). 
\emph{Applied Statistics}, \bold{54} (3), 507-554.


Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006).
Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files
(see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}


\author{Raydonal Ospina, Department of Statistics, University of Sao Paulo, Brazil.

\email{rospina@ime.usp.br}
}

\note{
This work is part of my PhD project at the University of Sao
Paulo under the supervion of Professor Silvia Ferrari.
My thesis is concerned with regression modelling of rates and
proportions with excess of zeros and/or ones}


\seealso{\code{\link{gamlss.family}}, \code{\link{BEOI}} }
\examples{

BEOI()# gives information about the default links for the BEOI distribution
# plotting the distribution
plotBEOI( mu =0.5 , sigma=5, nu = 0.1, from = 0.001, to=1, n = 101)
# plotting the cdf
plot(function(y) pBEOI(y, mu=.5 ,sigma=5, nu=0.1), 0.001, 0.999)
# plotting the inverse cdf
plot(function(y) qBEOI(y, mu=.5 ,sigma=5, nu=0.1), 0.001, 0.999)
# generate random numbers
dat<-rBEOI(100, mu=.5, sigma=5, nu=0.1)
# fit a model to the data. 
# library(gamlss)
#mod1<-gamlss(dat~1,sigma.formula=~1, nu.formula=~1, family=BEOI) 
#fitted(mod1)[1]
#summary(mod1)
#fitted(mod1,"mu")[1]        #fitted mu
#fitted(mod1,"sigma")[1]     #fitted sigma
#fitted(mod1,"nu")[1]        #fitted nu
#meanBEOI(mod1)[1] # expected value of the response
}
\keyword{distribution}
\keyword{regression}
