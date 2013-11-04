\name{PE}
\alias{PE}
\alias{dPE}
\alias{pPE}
\alias{qPE}
\alias{rPE}
\alias{PE2}
\alias{dPE2}
\alias{pPE2}
\alias{qPE2}
\alias{rPE2}

\title{ Power Exponential distribution for fitting a GAMLSS}
\description{
The functions define the Power Exponential distribution, a three parameter distribution, for a \code{gamlss.family} 
object to be used in GAMLSS 
fitting using the function \code{gamlss()}. 
The functions \code{dPE}, \code{pPE}, \code{qPE} and \code{rPE} define the density, distribution function, 
quantile function and random generation for the specific parameterization of the power exponential distribution 
showing below.
The functions \code{dPE2}, \code{pPE2}, \code{qPE2} and \code{rPE2} define the density, distribution function, 
quantile function and random generation of a standard  parameterization of the power exponential distribution.  
   
}
\usage{
PE(mu.link = "identity", sigma.link = "log", nu.link = "log")
dPE(x, mu = 0, sigma = 1, nu = 2, log = FALSE)
pPE(q, mu = 0, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)
qPE(p, mu = 0, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)
rPE(n, mu = 0, sigma = 1, nu = 2)
PE2(mu.link = "identity", sigma.link = "log", nu.link = "log")
dPE2(x, mu = 0, sigma = 1, nu = 2, log = FALSE)
pPE2(q, mu = 0, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)
qPE2(p, mu = 0, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)
rPE2(n, mu = 0, sigma = 1, nu = 2)
}

\arguments{
  \item{mu.link}{Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link}, with "log" link as the default for the nu parameter}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of kurtosis parameter }
   \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
Power Exponential distribution (PE) is defined as 
\deqn{f(y|\mu,\sigma,\nu)=\frac{\nu \exp[-(\frac{1}{2})|\frac{z}{c}|^{\nu}]}{\sigma c 2^{(1+1/\nu)}\Gamma(\frac{1}{\nu})}}{f(y|mu,sigma,nu)=(1/sigma)*(nu*exp(-0.5*|z/c|^nu))/(c*2^(1+1/nu)*Gamma(1/nu))}
where \eqn{c=[2^{-2/\nu}\Gamma(1/\nu)/\Gamma(3/\nu) ]^{0.5}}{c=[2^(-2/nu)Gamma(1/nu)/Gamma(3/nu)]^0.5},
for \eqn{y=(-\infty,+\infty)}{y=(-Inf,+Inf)}, \eqn{\mu=(-\infty,+\infty)}{\mu=(-Inf,+Inf)}, \eqn{\sigma>0} and \eqn{\nu>0}. 
This parametrization was used by Nelson (1991) and ensures \eqn{\mu}{mu} is the mean and \eqn{\sigma}{sigma} is the standard deviation of y 
(for all parameter values of \eqn{\mu}{mu}, \eqn{\sigma}{sigma} and \eqn{\nu}{nu} within the rages above)

Thw Power Exponential distribution (PE2) is defined as 
\deqn{f(y|\mu,\sigma,\nu)=\frac{\nu \exp[-\left|z\right|^\nu]} {2\sigma \Gamma\left(\frac{1}{\nu}\right)}}{f(y|mu,sigma,nu)=(nu *exp(-abs(z)^2))/(2*sigma*Gamma(1/nu))}

}
\value{
  returns a \code{gamlss.family} object which can be used to fit a Power Exponential distribution in the \code{gamlss()} function.   
}
\references{
Nelson, D.B. (1991) Conditional heteroskedasticity in asset returns: a new approach.  \emph{Econometrica}, \bold{57}, 347-370.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk}}
\note{\eqn{\mu}{mu} is the mean and \eqn{\sigma}{sigma} is the standard deviation of the Power Exponential distribution}



\seealso{  \code{\link{gamlss.family}}, \code{\link{BCPE}}}

\examples{
PE()# gives information about the default links for the Power Exponential distribution  
# library(gamlss)
# data(abdom)
# h1<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=PE, data=abdom) # fit
# h2<-gamlss(y~cs(x,df=3), sigma.formula=~cs(x,1), family=PE2, data=abdom) # fit 
# plot(h1)
# plot(h2)
# leptokurtotic
plot(function(x) dPE(x, mu=10,sigma=2,nu=1), 0.0, 20, 
 main = "The PE  density mu=10,sigma=2,nu=1")
# platykurtotic
plot(function(x) dPE(x, mu=10,sigma=2,nu=4), 0.0, 20, 
 main = "The PE  density mu=10,sigma=2,nu=4") 
}
\keyword{distribution}
\keyword{regression}% 
