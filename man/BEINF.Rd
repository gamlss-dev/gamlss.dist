\name{BEINF}
\alias{BEINF}
\alias{dBEINF}
\alias{pBEINF}
\alias{qBEINF}
\alias{rBEINF}
\alias{plotBEINF}
\alias{meanBEINF}
\alias{BEINF0}
\alias{dBEINF0}
\alias{pBEINF0}
\alias{qBEINF0}
\alias{rBEINF0}
\alias{BEINF1}
\alias{dBEINF1}
\alias{pBEINF1}
\alias{qBEINF1}
\alias{rBEINF1}
\alias{meanBEINF0}
\alias{meanBEINF1}
\alias{plotBEINF0}
\alias{plotBEINF1}

\title{The beta inflated distribution for fitting a GAMLSS}
\description{
The function \code{BEINF()} defines the beta inflated distribution, a four parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. 
The beta inflated is similar to the beta but allows zeros and ones as  values for the response variable. 
The two extra parameters model the probabilities at zero and one.
 
The functions \code{BEINF0()} and \code{BEINF1()} are three parameter beta inflated distributions allowing  
zeros or ones only at the response respectively.  \code{BEINF0()} and \code{BEINF1()} are re-parameterize versions 
of the distributions \code{\link{BEZI}} and \code{\link{BEOI}} contributed to gamlss by Raydonal Ospina 
(see Ospina and Ferrari (2010)).

The functions \code{dBEINF}, \code{pBEINF}, \code{qBEINF} and \code{rBEINF} define the density, distribution function, 
quantile function and random
generation for the \code{BEINF} parametrization of the beta inflated distribution. 

The functions \code{dBEINF0}, \code{pBEINF0}, \code{qBEINF0} and \code{rBEINF0} define the density, distribution function, 
quantile function and random
generation for the \code{BEINF0} parametrization of the beta inflated at zero distribution.
 
The functions \code{dBEINF1}, \code{pBEINF1}, \code{qBEINF1} and \code{rBEINF1} define the density, distribution function, 
quantile function and random
generation for the \code{BEINF1} parametrization of the beta inflated at one distribution.

\code{plotBEINF}, \code{plotBEINF0} and \code{plotBEINF1} can be used to plot the distributions. 
\code{meanBEINF}, \code{meanBEINF0} and \code{meanBEINF1} calculates the expected value of the response for a fitted model. 

}
\usage{
BEINF(mu.link = "logit", sigma.link = "logit", nu.link = "log", 
      tau.link = "log")
BEINF0(mu.link = "logit", sigma.link = "logit", nu.link = "log")
BEINF1(mu.link = "logit", sigma.link = "logit", nu.link = "log")
      
dBEINF(x, mu = 0.5, sigma = 0.1, nu = 0.1, tau = 0.1, 
       log = FALSE)
dBEINF0(x, mu = 0.5, sigma = 0.1, nu = 0.1, log = FALSE)
dBEINF1(x, mu = 0.5, sigma = 0.1, nu = 0.1, log = FALSE)       

pBEINF(q, mu = 0.5, sigma = 0.1, nu = 0.1, tau = 0.1, 
       lower.tail = TRUE, log.p = FALSE)
pBEINF0(q, mu = 0.5, sigma = 0.1, nu = 0.1, 
       lower.tail = TRUE, log.p = FALSE)       
pBEINF1(q, mu = 0.5, sigma = 0.1, nu = 0.1, 
        lower.tail = TRUE, log.p = FALSE)

qBEINF(p, mu = 0.5, sigma = 0.1, nu = 0.1, tau = 0.1, 
       lower.tail = TRUE, log.p = FALSE)
qBEINF0(p, mu = 0.5, sigma = 0.1, nu = 0.1, tau = 0.1, 
        lower.tail = TRUE, log.p = FALSE)
qBEINF1(p, mu = 0.5, sigma = 0.1, nu = 0.1, 
        lower.tail = TRUE, log.p = FALSE)
       
rBEINF(n, mu = 0.5, sigma = 0.1, nu = 0.1, tau = 0.1)
rBEINF0(n, mu = 0.5, sigma = 0.1, nu = 0.1)
rBEINF1(n, mu = 0.5, sigma = 0.1, nu = 0.1)

plotBEINF(mu = 0.5, sigma = 0.5, nu = 0.5, tau = 0.5, 
          from = 0.001, to = 0.999, n = 101, ...)
plotBEINF0(mu = 0.5, sigma = 0.5, nu = 0.5, 
          from = 1e-04, to = 0.9999, n = 101, ...)
plotBEINF1(mu = 0.5, sigma = 0.5, nu = 0.5, 
          from = 1e-04, to = 0.9999, n = 101, ...)

meanBEINF(obj)
meanBEINF0(obj)
meanBEINF1(obj)

}

\arguments{
  \item{mu.link}{the \code{mu} link function with default \code{logit}}
  \item{sigma.link}{the \code{sigma} link function with default \code{logit}}
  \item{nu.link}{the \code{nu} link function with default \code{log}}
  \item{tau.link}{the \code{tau} link function with default \code{log}}
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values} 
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of parameter values modelling the probability at zero}
  \item{tau}{vector of parameter values modelling the probability at one} 
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}   
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}  
  \item{from}{where to start plotting the distribution from}
  \item{to}{up to  where to plot the distribution  }
  \item{obj}{ a fitted \code{BEINF} object}
  \item{\dots}{ other graphical parameters for plotting}
}
\details{
   The beta inflated distribution is given as 
 \deqn{f(y)=p_0}{f(y)=p0} if (y=0)
 \deqn{f(y)=p_1}{f(y)=p1} if (y=1)
 \deqn{f(y|\alpha,\beta)=\frac{1}{B(\alpha, \beta)} y^{\alpha-1}(1-y)^{\beta-1}}{f(y|a,b)=(1/(Beta(a,b))) y^(a-1)(1-y)^(b-1)} 
otherwise 

for \eqn{y=(0,1)}{y=(0,1)}, \eqn{\alpha>0} and \eqn{\beta>0}. The parametrization in the function \code{BEINF()} is 
 \eqn{\mu=\frac{\alpha}{\alpha+\beta}}{mu=a/(a+b)} and  \eqn{\sigma=\frac{1}{\alpha+\beta+1}}{sigma=1/(a+b+1)}
 for \eqn{\mu=(0,1)}{mu=(0,1)} and \eqn{\sigma=(0,1)}{sigma=(0,1)} and  \eqn{\nu=\frac{p_0}{p_2}}{nu=p0/p2},
  \eqn{\tau=\frac{p_1}{p_2}}{tau=p1/p2} where  \eqn{p_2=1-p_0-p_1}{p2=1-p0-p1}. 
 
}
\value{
returns a \code{gamlss.family} object which can be used to fit a beta inflated distribution in the \code{gamlss()} function. 
  ...
}



\references{

Ospina R. and Ferrari S. L. P. (2010) Inflated beta distributions, \emph{Statistical Papers}, 
    \bold{23},  111-126. 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Bob Rigby and Mikis Stasinopoulos }


\seealso{ \code{\link{gamlss.family}}, \code{\link{BE}}, \code{\link{BEo}}, \code{\link{BEZI}}, \code{\link{BEOI}}}
\examples{
BEINF()# gives information about the default links for the beta inflated distribution
BEINF0()
BEINF1()
# plotting the distributions
op<-par(mfrow=c(2,2)) 
plotBEINF( mu =.5 , sigma=.5, nu = 0.5, tau = 0.5, from = 0, to=1, n = 101)
plotBEINF0( mu =.5 , sigma=.5, nu = 0.5,  from = 0, to=1, n = 101)
plotBEINF1( mu =.5 , sigma=.5, nu = 0.5,  from = 0.001, to=1, n = 101)
curve(dBE(x, mu =.5, sigma=.5),  0.01, 0.999)
par(op)
# plotting the cdf
op<-par(mfrow=c(2,2)) 
plotBEINF( mu =.5 , sigma=.5, nu = 0.5, tau = 0.5, from = 0, to=1, n = 101, main="BEINF")
plotBEINF0( mu =.5 , sigma=.5, nu = 0.5,  from = 0, to=1, n = 101, main="BEINF0")
plotBEINF1( mu =.5 , sigma=.5, nu = 0.5,  from = 0.001, to=1, n = 101, main="BEINF1")
curve(dBE(x, mu =.5, sigma=.5),  0.01, 0.999, main="BE")
par(op)
#---------------------------------------------
op<-par(mfrow=c(2,2)) 
plotBEINF( mu =.5 , sigma=.5, nu = 0.5, tau = 0.5, from = 0, to=1, n = 101, main="BEINF")
plotBEINF0( mu =.5 , sigma=.5, nu = 0.5,  from = 0, to=1, n = 101, main="BEINF0")
plotBEINF1( mu =.5 , sigma=.5, nu = 0.5,  from = 0.001, to=1, n = 101, main="BEINF1")
curve(dBE(x, mu =.5, sigma=.5),  0.01, 0.999, main="BE")
par(op)
#---------------------------------------------
op<-par(mfrow=c(2,2)) 
curve( pBEINF(x, mu=.5 ,sigma=.5, nu = 0.5, tau = 0.5,), 0, 1, ylim=c(0,1), main="BEINF" )
curve(pBEINF0(x, mu=.5 ,sigma=.5, nu = 0.5), 0, 1, ylim=c(0,1), main="BEINF0")
curve(pBEINF1(x, mu=.5 ,sigma=.5, nu = 0.5), 0, 1, ylim=c(0,1), main="BEINF1")
curve(    pBE(x, mu=.5 ,sigma=.5), .001, .99, ylim=c(0,1), main="BE")
par(op)
#---------------------------------------------
op<-par(mfrow=c(2,2)) 
curve(qBEINF(x, mu=.5 ,sigma=.5, nu = 0.5, tau = 0.5), .01, .99, main="BEINF" )
curve(qBEINF0(x, mu=.5 ,sigma=.5, nu = 0.5), .01, .99, main="BEINF0" )
curve(qBEINF1(x, mu=.5 ,sigma=.5, nu = 0.5), .01, .99, main="BEINF1" )
curve(qBE(x, mu=.5 ,sigma=.5), .01, .99 , main="BE")
par(op)

#---------------------------------------------
op<-par(mfrow=c(2,2)) 
hist(rBEINF(200, mu=.5 ,sigma=.5, nu = 0.5, tau = 0.5))
hist(rBEINF0(200, mu=.5 ,sigma=.5, nu = 0.5))
hist(rBEINF1(200, mu=.5 ,sigma=.5, nu = 0.5))
hist(rBE(200, mu=.5 ,sigma=.5))
par(op)
# fit a model to the data 
# library(gamlss)
#m1<-gamlss(dat~1,family=BEINF)
#meanBEINF(m1)[1]
}
\keyword{distribution}
\keyword{regression}
