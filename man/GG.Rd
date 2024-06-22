\name{GG}
\alias{GG}
\alias{pGG}
\alias{dGG}
\alias{qGG}
\alias{rGG}



%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generalized Gamma distribution for fitting a GAMLSS }
\description{
  The function GG defines the generalized gamma distribution, a three parameter distribution, 
for a gamlss.family object to be used in GAMLSS fitting using the function gamlss(). 
The parameterization used has a rather compicated mean function  which is depends on all the paramneters of the distribution,  \code{mu}, \code{sigma} and \code{nu}, see page 444 of Rigby \emph{et al.} (2019). The functions `dGG`, `pGG`, `qGG` and `rGG` define the density, 
distribution function, quantile function and random generation for the specific parameterization 
of the generalized gamma distribution defined by function GG.

}
\usage{
GG(mu.link = "log", sigma.link = "log", 
                       nu.link = "identity")
dGG(x, mu=1, sigma=0.5, nu=1,  
                      log = FALSE)
pGG(q, mu=1, sigma=0.5, nu=1,  lower.tail = TRUE, 
                     log.p = FALSE)
qGG(p, mu=1, sigma=0.5, nu=1,  lower.tail = TRUE, 
                     log.p = FALSE )
rGG(n, mu=1, sigma=0.5, nu=1)
}

\arguments{
  \item{mu.link}{ Defines the mu.link, with "log" link as the default for the mu parameter, 
                                  other links are "inverse" and "identity" }
  \item{sigma.link}{ Defines the sigma.link, with "log" link as the default for the sigma parameter, 
                                           other links are "inverse" and "identity" }
  \item{nu.link}{ Defines the nu.link, with "identity" link as the default for the sigma parameter, 
                                       other links are \eqn{1/nu^2}{"1/nu^2"} and "log"}
  \item{x,q}{ vector of quantiles }
  \item{mu}{ vector of location parameter values }
  \item{sigma}{ vector of scale parameter values }
  \item{nu}{ vector of shape parameter values }
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p). }
  \item{lower.tail}{ logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x] }
  \item{p}{ vector of probabilities }
  \item{n}{ number of observations. If length(n) > 1, the length is taken to be the number required}

}
\details{
   The specific parameterization of the generalized gamma distribution used in GG is 
\deqn{f(y|\mu,\sigma,\nu)= \frac{\theta^\theta z^\theta |\nu| e^{-\theta z}}{(\Gamma(\theta)y)}}{f(y|mu,sigma,nu)=theta^theta*z^theta*nu*e^(-theta*z)/(Gamma(theta)*y)}
where \eqn{z =(y/\mu)^\nu}{z =(y/mu)^nu}, 
\eqn{\theta = 1/(\sigma^2\nu^2)}{theta = 1/(sigma^2*abs(nu)^2)}
for y>0, \eqn{\mu>0}{mu>0}, \eqn{\sigma>0}{sigma>0} and \eqn{-\infty<\nu<+\infty}{-Inf>nu>Inf} see pp. 443-444 of Rigby et al. (2019). Note that for 
\eqn{\nu=0}{nu=0} the distribution is log normal.

}
\value{
  GG() returns a gamlss.family object which can be used to fit a generalized gamma distribution in 
the gamlss() function. dGG() gives the density, pGG() gives the distribution function, qGG() gives the quantile function, 
and rGG() generates random deviates. 

}
\references{ 
 Lopatatzidis, A. and Green, P. J. (2000), Nonparametric quantile regression using the gamma distribution, unpublished.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\author{ Mikis Stasinopoulos, Bob Rigby and Nicoleta Motpan}

\seealso{\code{\link{gamlss.family}}, \code{\link{GA}}}

\examples{
y<-rGG(100,mu=1,sigma=0.1, nu=-.5) # generates 100 random observations  
hist(y)
# library(gamlss)
#histDist(y, family=GG)
#m1 <-gamlss(y~1,family=GG)
#prof.dev(m1, "nu", min=-2, max=2, step=0.2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}%
