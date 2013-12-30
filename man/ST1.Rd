\name{ST1}
\alias{ST1}
\alias{dST1}
\alias{pST1}
\alias{qST1}
\alias{rST1}
\alias{ST2}
\alias{dST2}
\alias{pST2}
\alias{qST2}
\alias{rST2}
\alias{ST3}
\alias{dST3}
\alias{pST3}
\alias{qST3}
\alias{rST3}
\alias{ST4}
\alias{dST4}
\alias{pST4}
\alias{qST4}
\alias{rST4}
\alias{ST5}
\alias{dST5}
\alias{pST5}
\alias{qST5}
\alias{rST5}
\alias{SST}
\alias{dSST}
\alias{pSST}
\alias{qSST}
\alias{rSST}

\title{The skew t distributions, type 1 to 5 }
\description{


There are 5 different skew t distributions implemented in GAMLSS. 

The Skew t type 3 distribution Jones and Faddy (2003). 
The functions \code{dST3}, \code{pST3}, \code{qST3} and \code{rST3} define the density, distribution function,
 quantile function and random generation for the skew t distribution type 3. 
The \code{SST} is a reparametrised version of  \code{dST3} where \code{sigma} is the standard deviation of the distribution.  
 
      
}
\usage{
ST1(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link="log")
dST1(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pST1(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
qST1(p, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
rST1(n, mu = 0, sigma = 1, nu = 0, tau = 2)

ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")
dST2(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pST2(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
qST2(p, mu = 1, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
rST2(n, mu = 0, sigma = 1, nu = 0, tau = 2)

ST3(mu.link = "identity", sigma.link = "log", nu.link = "log", tau.link = "log")
dST3(x, mu = 0, sigma = 1, nu = 1, tau = 10, log = FALSE)
pST3(q, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
qST3(p, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
rST3(n, mu = 0, sigma = 1, nu = 1, tau = 10)

SST(mu.link = "identity", sigma.link = "log", nu.link = "log", 
   tau.link = "logshiftto2")
dSST(x, mu = 0, sigma = 1, nu = 0.8, tau = 7, log = FALSE)
pSST(q, mu = 0, sigma = 1, nu = 0.8, tau = 7, lower.tail = TRUE, log.p = FALSE)
qSST(p, mu = 0, sigma = 1, nu = 0.8, tau = 7, lower.tail = TRUE, log.p = FALSE)
rSST(n, mu = 0, sigma = 1, nu = 0.8, tau = 7)

ST4(mu.link = "identity", sigma.link = "log", nu.link = "log", tau.link = "log")
dST4(x, mu = 0, sigma = 1, nu = 1, tau = 10, log = FALSE)
pST4(q, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
qST4(p, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
rST4(n, mu = 0, sigma = 1, nu = 1, tau = 10)

ST5(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")
dST5(x, mu = 0, sigma = 1, nu = 0, tau = 1, log = FALSE)
pST5(q, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, log.p = FALSE)
qST5(p, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, log.p = FALSE)
rST5(n, mu = 0, sigma = 1, nu = 0, tau = 1)
}

\arguments{
 \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. 
                  Other links are "\eqn{1/mu^2}" and "log"}
  \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. 
                      Other links are "inverse" and "identity"}
  \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. 
                  Other links are "\eqn{1/mu^2}" and "log"}
   \item{tau.link}{Defines the  \code{nu.link}, with "log" link as the default for the \code{nu} parameter. 
                   Other links are "inverse", "identity" }                
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of \code{mu} parameter values }
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of \code{nu} parameter values}
   \item{tau}{vector of \code{tau} parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
  \item{\dots}{for extra arguments}
}
\details{
The probability density function of the skew t distribution type 1, (\code{ST1}), Azzalini (1986) is defined as 
  
\deqn{f(y|\mu,\sigma,\nu, \tau)=\frac{z}{\sigma} \mbox{\hspace{0.1cm}}f_z(z) \mbox{\hspace{0.1cm}} F_z(\nu z)}{f(y|mu,sigma,nu,tau)=z/sigma *f(z)F(nu*z)}

for  \eqn{-\infty<y<\infty}{-Inf<y<Inf} and  \eqn{z \sim TF(0,1, \tau)}{z~TF(0,1)} has a t distribution with 
\eqn{\tau>0}{tau>0} degrees of freedom with \eqn{\tau}{tau} treated as a continuous parameter. 

The probability density function of the skew t distribution type 2, (\code{ST2}), Azzalini and Capitano (2003), 
is defined as 
  
\deqn{f(y|\mu,\sigma,\nu,\frac{z}{\sigma} \mbox{\hspace{0.1cm}} f_{z_1}(z) \mbox{\hspace{0.1cm}} F_{z_2}(w) \tau)=}{f(y|mu,sigma,nu,tau)=z/sigma f_z1(z)F_z2(w)}

for \eqn{-\infty<y<\infty}{-Inf<y<Inf}, where \eqn{z=(y-\mu)/\sigma}{z=(y-mu)/sigma}, 
\eqn{w=\nu \lambda^{1/2}z}{w=nu*sqrt(lambda)*z}, \eqn{\lambda=(\tau+1)/(\tau+z^2)}{lambda=(tau+1)/(tau+z*z)} and 
\eqn{z_1 \sim TF(0,1,\tau)}{z_1 ~ TF(01,1,tau)} and \eqn{z_2 \sim TF(0,1, \tau+1)}{z_2 ~ TF(0,1,tau+1)}.

The probability density function of the skew t distribution type q, (\code{ST3}), is defined in Chapter 10 of the 
GAMLSS manual.  
  

The probability density function of the skew t distribution type q, (\code{ST4}), is defined in Chapter of the 
GAMLSS manual.  


The probability density function of the skew t distribution type 5, (\code{ST5}), is defined as 
\deqn{f(y|\mu,\sigma,\nu, \tau)=\frac{1}{c} \left[ 1+ \frac{z}{(a+b +z^2)^{1/2}} \right]^{a+1/2} \left[ 1- \frac{z}{(a+b+z^2)^{1/2}}\right]^{b+1/2}}{f(y|mu,sigma,nu,tau)=(1/c)*(1+(z/(a+b+z^2)^0.5))^(a+0.5)*(1-(a+b+z^2)^0.5)^(b+0.5)}

where \eqn{c=2^{a +b-1} (a+b)^{1/2} B(a,b)}{c=2^(a+b-1)*(a+b)^0.5 *B(a,b)}, and 
\eqn{B(a,b)=\Gamma(a)\Gamma(b)/ \Gamma(a+b)}{Gamma(a)*Gamma(b)/Gamma(a+b)} and 
\eqn{z=(y-\mu)/\sigma}{(y-mu)/sigma} and 
\eqn{\nu=(a-b)/\left[ab(a+b) \right]^{1/2}}{nu=(a-b)/(a*b*(a+b))^0.5}
and
\eqn{\tau=2/(a+b)}{tau=2/(a+b)} 
for \eqn{-\infty<y<\infty}{-Inf<y<Inf}, \eqn{-\infty<\mu<\infty}{-Inf<mu<Inf}, \eqn{\sigma>0}, 
 \eqn{-\infty<\nu>\infty}{-Inf<nu<Inf} and \eqn{\tau>0}{tau>0}.   
}
\value{
  \code{ST1()}, \code{ST2()}, \code{ST3()}, \code{ST4()} and \code{ST5()}  return a \code{gamlss.family} object 
    which can be used to fit the skew t type 1-5 distribution in the \code{gamlss()} function. 
  \code{dST1()}, \code{dST2()}, \code{dST3()}, \code{dST4()} and \code{dST5()} give the density functions, 
  \code{pST1()}, \code{pST2()}, \code{pST3()}, \code{pST4()} and \code{pST5()}  give the cumulative distribution  functions, 
  \code{qST1()}, \code{qST2()}, \code{qST3()}, \code{qST4()} and \code{qST5()} give the quantile function, and 
  \code{rST1()}, \code{rST2()}, \code{rST3()}, \code{rST4()} and \code{rST3()}  generates random deviates. 
}
\references{ 

Jones, M.C. and Faddy, M. J. (2003) A skew extension of the t distribution, with applications. 
\emph{Journal of the Royal Statistical Society}, Series B, \bold{65}, pp 159-174.  

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R. A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

 }
\author{Bob Rigby and Mikis Stasinopoulos  }
\note{ The mean of the ex-Gaussian is \eqn{\mu+\nu}{mu+nu} and the variance is \eqn{\sigma^2+\nu^2}{sigma^2+nu^2}.
}
\seealso{ \code{\link{gamlss.family}}, \code{\link{BCCG}}, \code{\link{GA}}, 
\code{\link{IG}}  \code{\link{LNO}}}
\examples{ 
y<- rST5(200, mu=5, sigma=1, nu=.1)
hist(y)
curve(dST5(x, mu=30 ,sigma=5,nu=-1), -50, 50, main = "The ST5  density mu=30 ,sigma=5,nu=1")
# library(gamlss)
# m1<-gamlss(y~1, family=ST1)
# m2<-gamlss(y~1, family=ST2)
# m3<-gamlss(y~1, family=ST3)
# m4<-gamlss(y~1, family=ST4)
# m5<-gamlss(y~1, family=ST5) 
# GAIC(m1,m2,m3,m4,m5)
}
\keyword{distribution}% 
\keyword{regression}% 
