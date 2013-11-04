\name{BEZI}
\alias{BEZI}
\alias{dBEZI}
\alias{pBEZI}
\alias{qBEZI}
\alias{rBEZI}
\alias{plotBEZI}
\alias{meanBEZI}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{The zero-inflated beta distribution for fitting a GAMLSS}
\description{
The function \code{BEZI()} defines the zero-inflated beta distribution, a
three parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}.
The zero-inflated beta is similar to the beta distribution but allows zeros as y values. This distribution
is an extension of the beta distribution using a parameterization of the beta law that is
indexed by mean and precision parameters (Ferrari and Cribari-Neto, 2004).
The extra parameter models the probability at zero.
The functions \code{dBEZI}, \code{pBEZI}, \code{qBEZI} and \code{rBEZI} define the density,
distribution function, quantile function and random
generation for the \code{BEZI} parameterization of the zero-inflated beta distribution. 
\code{plotBEZI} can be used to plot the distribution. \code{meanBEZI} calculates the expected value of the response for a fitted model. 
}
\usage{
BEZI(mu.link = "logit", sigma.link = "log", nu.link = "logit")

dBEZI(x, mu = 0.5, sigma = 1, nu = 0.1, log = FALSE)

pBEZI(q, mu = 0.5, sigma = 1, nu = 0.1, lower.tail = TRUE, log.p = FALSE)

qBEZI(p, mu = 0.5, sigma = 1, nu = 0.1, lower.tail = TRUE,
        log.p = FALSE)
        
rBEZI(n, mu = 0.5, sigma = 1, nu = 0.1)

plotBEZI(mu = .5, sigma = 1, nu = 0.1, from = 0, to = 0.999, n = 101, 
    ...)
    
meanBEZI(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the \code{mu} link function with default \code{logit}}
  \item{sigma.link}{the \code{sigma} link function with default \code{log}}
  \item{nu.link}{the \code{nu} link function with default \code{logit}}

  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values} 
  \item{sigma}{vector of precision parameter values}
  \item{nu}{vector of parameter values modelling the probability at zero}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}   
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}  
  \item{from}{where to start plotting the distribution from}
  \item{to}{up to  where to plot the distribution  }
  \item{obj}{ a fitted \code{BEZI} object}
  \item{\dots}{ other graphical parameters for plotting}
}
\details{ The zero-inflated beta distribution is given as
\deqn{f(y)=\nu}{f(y)=nu} if \eqn{(y=0)}{(y=0)}
\deqn{f(y|\mu,\sigma)=(1-\nu)\frac{\Gamma(\sigma)}{\Gamma(\mu\sigma)\Gamma((1-\mu)\sigma)} y^{\mu\sigma}(1-y)^{((1-\mu)\sigma)-1}}{f(y|mu,sigma)=(1-nu)*(Gamma(sigma)/Gamma(mu*sigma)*Gamma((1-mu)*sigma))*y^(mu*sigma-1)*(1-y)^(((1-mu)*sigma)-1)
}
if \eqn{y=(0,1)}{y=(0,1)}. The parameters satisfy \eqn{0<\mu<0}{0<mu<1}, \eqn{\sigma>0}{sigma>0} and \eqn{0<\nu< 1}{0<nu<1}.

Here \eqn{E(y)=(1-\nu)\mu}{E(y)=(1-nu)*mu} and 
\eqn{Var(y)=(1-\nu)\frac{\mu(1-\mu)}{\sigma+1}+\nu(1-\nu)\mu^2}{Var(y)=(1-nu)*(mu*(1-mu))/(sigma+1) + nu*(1-nu)*mu^2}. 
}

\value{
returns a \code{gamlss.family} object which can be used to fit a zero-inflated beta distribution in the \code{gamlss()} function. 
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


\seealso{ \code{\link{gamlss.family}}, \code{\link{BEZI}} }
\examples{

 BEZI()# gives information about the default links for the BEZI distribution
# plotting the distribution
plotBEZI( mu =0.5 , sigma=5, nu = 0.1, from = 0, to=0.99, n = 101)
# plotting the cdf
plot(function(y) pBEZI(y, mu=.5 ,sigma=5, nu=0.1), 0, 0.999)
# plotting the inverse cdf
plot(function(y) qBEZI(y, mu=.5 ,sigma=5, nu=0.1), 0, 0.999)
# generate random numbers
dat<-rBEZI(100, mu=.5, sigma=5, nu=0.1)
# fit a model to the data. Tits a constant for mu, sigma and nu
# library(gamlss)
#mod1<-gamlss(dat~1,sigma.formula=~1, nu.formula=~1, family=BEZI) 
#fitted(mod1)[1]
#summary(mod1)
#fitted(mod1,"mu")[1]         #fitted mu   
#fitted(mod1,"sigma")[1]      #fitted sigma 
#fitted(mod1,"nu")[1]         #fitted nu  
#meanBEZI(mod1)[1] # expected value of the response
}
\keyword{distribution}
\keyword{regression}
