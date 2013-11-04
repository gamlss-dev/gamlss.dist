\name{RGE}
\alias{RGE}
\alias{dRGE}
\alias{pRGE}
\alias{qRGE}
\alias{rRGE}

\title{Reverse generalized extreme family distribution for fitting a GAMLSS}
\description{
The function \code{RGE} defines the reverse generalized extreme family distribution, a three parameter distribution,
 for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. 
 The functions \code{dRGE}, \code{pRGE}, \code{qRGE} and \code{rRGE} define the density, distribution function, quantile function and random
generation for the specific parameterization of the reverse generalized extreme distribution given in details below.
}
\usage{
RGE(mu.link = "identity", sigma.link = "log", nu.link = "log")
dRGE(x, mu = 1, sigma = 0.1, nu = 1, log = FALSE)
pRGE(q, mu = 1, sigma = 0.1, nu = 1, lower.tail = TRUE, log.p = FALSE)
qRGE(p, mu = 1, sigma = 0.1, nu = 1, lower.tail = TRUE, log.p = FALSE) 
rRGE(n, mu = 1, sigma = 0.1, nu = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link}, with "log" link as the default for the nu parameter}
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of the shape parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
  Definition file for reverse generalized extreme family distribution. 

The probability density function of the generalized extreme value
distribution is obtained from Johnson \emph{et al.} (1995), Volume 2,
p76, equation (22.184) [where \eqn{(\xi, \theta, \gamma)
\longrightarrow (\mu, \sigma, \nu)}{(xi,theta,gamma)->(mu,sigma, nu)}].

The probability density function of the reverse generalized extreme value distribution 
is then obtained by replacing y by -y and \eqn{\mu} by \eqn{-\mu}.

Hence the probability density function of the reverse generalized extreme value distribution 
with \eqn{\nu>0} is given by

\deqn{f(y|\mu,\sigma, \nu)=\frac{1}{\sigma}\left[1+\frac{\nu(y-\mu)}{\sigma}\right]^{\frac{1}{\nu}-1}S_1(y|\mu,\sigma,\nu)}{f(y|mu,sigma,nu)=(1/sigma)(1+(nu*(y-mu))/(sigma))^(1/(nu-1))*S1(y|mu,sigma,nu)} 

for \deqn{\mu-\frac{\sigma}{\nu}<y<\infty} 

where

\deqn{S_1(y|\mu,\sigma,\nu)=\exp\left\{-\left[1+\frac{\nu(y-\mu)}{\sigma}\right]^\frac{1}{\nu}\right\}}{S1(y|mu,sigma,nu)=exp(-[1+(nu*(y-mu))/(sigms)]^(1/nu))}

and where \eqn{-\infty<\mu<y+\frac{\sigma}{\nu}}, \eqn{\sigma>0}
and \eqn{\nu>0}. Note that only the case \eqn{\nu>0}{nu>0} is allowed here. The reverse generalized extreme value distribution is denoted
as RGE(\eqn{\mu,\sigma,\nu}) or as Reverse Generalized.Extreme.Family(\eqn{\mu,\sigma,\nu}).

Note the the above distribution is a reparameterization of the three parameter Weibull distribution given by

\deqn{f(y|\alpha_1,\alpha_2,\alpha_3)=\frac{\alpha_3}{\alpha_2}\left[\frac{y-\alpha_1}{\alpha_2}\right]^{\alpha_3-1} \exp\left[ -\left(\frac{y-\alpha_1}{\alpha_2} \right)^{\alpha_3} \right]}{f(y|mu,sigma,nu)=(a3/a2)*((y-a1)/a2)^(a3-1)exp(-((y-a1)/a2)^a3)}

given by setting \eqn{\alpha_1=\mu-\sigma/\nu}{a1=mu-(sigma/nu)}, \eqn{\alpha_2=\sigma/\nu}{a2=sigma/nu}, \eqn{\alpha_3=1/\nu}{1/nu}. 

}
\value{
 \code{RGE()} returns a \code{gamlss.family} object which can be used to fit a reverse generalized extreme distribution in the \code{gamlss()} function.
  \code{dRGE()} gives the density, \code{pRGE()} gives the distribution
     function, \code{qRGE()} gives the quantile function, and \code{rRGE()}
     generates random deviates. 
}
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).
 
Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 
}
\author{Bob Rigby \email{r.rigby@londonmet.ac.uk}, Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Kalliope Akantziliotou }


\note{This distribution is very difficult to fit because the y values depends
on the parameter  values. The \code{RS()} and \code{CG()} algorithms are not appropriate for this type of problem.}


\seealso{  \code{\link{gamlss.family}}}

\examples{
RGE()# gives information about the default links for the reverse generalized extreme family distribution 
newdata<-rRGE(100,mu=0,sigma=1,nu=5) # generates 100 random observations
# library(gamlss)
# gamlss(newdata~1, family=RGE, method=mixed(5,50)) # difficult to converse 
}
\keyword{distribution}
\keyword{regression}% 
