\name{LNO}
\alias{LNO}
\alias{dLNO}
\alias{pLNO}
\alias{qLNO}
\alias{rLNO}
\alias{LOGNO}
\alias{dLOGNO}
\alias{pLOGNO}
\alias{qLOGNO}
\alias{rLOGNO}
\alias{LOGNO2}
\alias{dLOGNO2}
\alias{pLOGNO2}
\alias{qLOGNO2}
\alias{rLOGNO2}

\title{Log Normal distribution for fitting in GAMLSS}
\description{
The functions  \code{LOGNO} and \code{LOGNO2} define a \code{gamlss.family} distribution to fits the log-Normal distribution. 
The difference between them is that while \code{LOGNO} retains the original parametrization for \code{mu}, (identical to the normal distribution \code{NO}) and therefore \eqn{\mu=(-\infty,+\infty)}{mu=(-Inf,+Inf)}, the function  \code{LOGNO2} use \code{mu} as the median, so \eqn{\mu=(0,+\infty)}{mu=(0,+Inf)}.

The function   \code{LNO} is more general and can fit a Box-Cox transformation 
to data using the \code{gamlss()} function.
In the  \code{LOGNO} (and \code{LOGNO2}) there are two parameters involved \code{mu} \code{sigma}, while in the 
\code{LNO} there are three parameters \code{mu} \code{sigma},
and the transformation parameter \code{nu}. 
The transformation parameter \code{nu} in \code{LNO} is a 'fixed' parameter (not estimated) and it has its default value equal to 
zero allowing the fitting of the log-normal distribution as in \code{LOGNO}.   
See the example below on how to fix \code{nu} to be a particular value. 
In order to estimate (or model) the parameter \code{nu}, use the \code{\link{gamlss.family}}
\code{\link{BCCG}} distribution which uses a reparameterized version of the the Box-Cox transformation.
The functions \code{dLOGNO}, \code{pLOGNO}, \code{qLOGNO} and \code{rLOGNO} define the density, distribution function, quantile function and random
generation for the specific parameterization of the log-normal distribution.

The functions \code{dLOGNO2}, \code{pLOGNO2}, \code{qLOGNO2} and \code{rLOGNO2} define the density, distribution function, quantile function and random
generation when \code{mu} is the median  of the log-normal distribution.

The functions \code{dLNO}, \code{pLNO}, \code{qLNO} and \code{rLNO} define the density, distribution function, quantile function and random
generation for the specific parameterization of the log-normal distribution and more generally a Box-Cox transformation. 
}


\usage{


LNO(mu.link = "identity", sigma.link = "log")
LOGNO(mu.link = "identity", sigma.link = "log")
LOGNO2(mu.link = "log", sigma.link = "log")
dLNO(x, mu = 1, sigma = 0.1, nu = 0, log = FALSE)
dLOGNO(x, mu = 0, sigma = 1, log = FALSE)
dLOGNO2(x, mu = 1, sigma = 1, log = FALSE)
pLNO(q, mu = 1, sigma = 0.1, nu = 0, lower.tail = TRUE, log.p = FALSE)
pLOGNO(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
pLOGNO2(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qLNO(p, mu = 1, sigma = 0.1, nu = 0, lower.tail = TRUE, log.p = FALSE)
qLOGNO(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qLOGNO2(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rLNO(n, mu = 1, sigma = 0.1, nu = 0)
rLOGNO(n, mu = 0, sigma = 1)
rLOGNO2(n, mu = 1, sigma = 1)

}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" or "log" link depending on te parametrization}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter. Other links are "inverse", "identity" ans "own"}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of shape parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
  The probability density function in \code{LOGNO} is defined as  
\deqn{f(y|\mu,\sigma)=\frac{1}{y \sqrt{2\pi}\sigma} \exp [-\frac{1}{2 \sigma^2}(\log y-\mu)^2 ] }{f(y|mu,sigma)=(1/(y*sqrt(2*pi)*sigma))*exp(-0.5*((log y-mu)/(sigma))^2)}  
for \eqn{y>0}, \eqn{-\infty<\mu<\infty} and \eqn{\sigma>0} see pp. 428-429 of Rigby et al. (2019).


The probability density function in \code{LOGNO2} is defined as  
\deqn{f(y|\mu,\sigma)=\frac{1}{y \sqrt{2\pi}\sigma} \exp [-\frac{1}{2 \sigma^2}(\log y-\log\mu)^2 ] }{f(y|mu,sigma)=(1/(y*sqrt(2*pi)*sigma))*exp(-0.5*((log(y)-mu)/(sigma))^2)}  
for \eqn{y>0}, \eqn{-\infty<\mu<\infty} and \eqn{\sigma>0} see pp. 429-430 of Rigby et al. (2019).

The probability density function in \code{LNO} is defined as  
\deqn{f(y|\mu,\sigma,\nu)=\frac{y^{\nu-1}}{\sqrt{2\pi}\sigma} \exp [-\frac{1}{2 \sigma^2}(z-\mu)^2 ] }{f(y|mu,sigma,nu)=(1/(sqrt(2*pi)*sigma))*(y^(nu-1))*exp(-((z-mu)/(2sigma))^2)}

where if \eqn{\nu \neq 0}{\nu!=0} \eqn{z =(y^{\nu}-1)/\nu}{z=(y^nu-1)/nu}  else \eqn{z=\log(y)} and \eqn{z \sim N(0,\sigma^2)}, 
for \eqn{y>0}, \eqn{\mu>0}, \eqn{\sigma>0} and \eqn{\nu=(-\infty,+\infty)}{\nu=(-Inf,+Inf)}. This is not a proper distribution see for example p. 447 of Rigby et al. (2019).

}
\value{
  \code{LNO()} returns a \code{gamlss.family} object which can be used to fit a log-normal distribution in the \code{gamlss()} function.
  \code{dLNO()} gives the density, \code{pLNO()} gives the distribution
     function, \code{qLNO()} gives the quantile function, and \code{rLNO()}
     generates random deviates.
}
\references{

Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with discussion), \emph{J. R. Statist. Soc.} B., \bold{26}, 211--252

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}.An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).

}
\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}

\note{\eqn{\mu}{mu} is the mean of z (and also the median of y), the Box-Cox transformed variable and \eqn{\sigma}{sigma} is the standard deviation of z
           and approximate the coefficient of variation of y}
\section{Warning }{This is a two parameter fit for \eqn{\mu}{mu} and \eqn{\sigma}{sigma} while \eqn{\nu}{nu} is fixed. 
             If you wish to model \eqn{\nu}{nu} use the gamlss family \code{BCCG}.} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{BCCG}} }

\examples{
LOGNO()#  gives information about the default links for the log normal distribution 
LOGNO2()
LNO()# gives information about the default links for the Box Cox distribution 

# plotting the d, p, q, and r functions
op<-par(mfrow=c(2,2))
curve(dLOGNO(x, mu=0), 0, 10)
curve(pLOGNO(x, mu=0), 0, 10)
curve(qLOGNO(x, mu=0), 0, 1)
Y<- rLOGNO(200)
hist(Y)
par(op)

# plotting the d, p, q, and r functions
op<-par(mfrow=c(2,2))
curve(dLOGNO2(x, mu=1), 0, 10)
curve(pLOGNO2(x, mu=1), 0, 10)
curve(qLOGNO2(x, mu=1), 0, 1)
Y<- rLOGNO(200)
hist(Y)
par(op)

# library(gamlss)
# data(abdom)
# h1<-gamlss(y~cs(x), family=LOGNO, data=abdom)#fits the log-Normal distribution  
# h2<-gamlss(y~cs(x), family=LNO, data=abdom)  #should be identical to the one above   
# to change to square root transformation, i.e. fix nu=0.5 
# h3<-gamlss(y~cs(x), family=LNO, data=abdom, nu.fix=TRUE, nu.start=0.5)
}
\keyword{distribution}
\keyword{regression}% 
