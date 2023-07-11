\name{GAF}
\alias{GAF}
\alias{dGAF}
\alias{pGAF}
\alias{qGAF}
\alias{rGAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Gamma distribution family}
\description{
The function \code{GAF()} defines a gamma distribution family, which has three parameters. This is not the generalised gamma distribution which is called  \code{GG}. The third parameter here is  to model the mean and variance relationship.  The distribution can be fitted using the function \code{gamlss()}. The  mean of \code{GAF} is equal to \code{mu}. The variance is equal to \code{sigma^2*mu^nu} so the standard deviation is   \code{sigma*mu^(nu/2)}. The function is design for cases where the variance is proportional to a power of the mean. This is an instance of the Taylor's power low, see Enki et al. (2017).  The functions \code{dGAF}, \code{pGAF}, \code{qGAF} and \code{rGAF} define the density, distribution function,
quantile function and random generation for the \code{GAF} parametrization of the gamma family.
}
\usage{
GAF(mu.link = "log", sigma.link = "log", nu.link = "identity")
dGAF(x, mu = 1, sigma = 1, nu = 2, log = FALSE)
pGAF(q, mu = 1, sigma = 1, nu = 2, lower.tail = TRUE, 
    log.p = FALSE)
qGAF(p, mu = 1, sigma = 1, nu = 2, lower.tail = TRUE, 
    log.p = FALSE)
rGAF(n, mu = 1, sigma = 1, nu = 2)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link} with "identity" link as the default for the nu parameter}
  \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of power parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
  }

\details{

 The parametrization of the gamma family given in the function \code{GAF()} is: 
 
 \deqn{f(y|\mu,\sigma_1)=\frac{y^{(1/\sigma_1^2-1)}\exp[-y/(\sigma_1^2 \mu)]}{(\sigma_1^2 \mu)^{(1/\sigma^2)} \Gamma(1/\sigma^2)}}{f(y|\mu,\sigma_1)=(y^((1/\sigma_1^2)-1)*exp[-y/((\sigma_1^2)*\mu)])/((\sigma_1^2*\mu)^(1/\sigma_1^2) Gamma(1/\sigma_1^2)) }
for \eqn{y>0}, \eqn{\mu>0} where \eqn{\sigma_1=\sigma \mu^{(\nu/2)-1} }
\eqn{\sigma>0} and \eqn{-\infty <\nu< \infty} see pp. 442-443 of Rigby et al. (2019).


}
\value{
 \code{GAF()} returns a \code{gamlss.family} object which can be used to fit the gamma family in the \code{gamlss()} function. 
    
}

\references{

Enki, D G, Noufaily, A.,  Farrington, P., Garthwaite, P., Andrews, N. and Charlett, A. (2017) Taylor's power law and the statistical modelling of infectious disease surveillance data, Journal of the Royal Statistical Society: Series A (Statistics in Society), volume=180, number=1, pages=45-72.


Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}..

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos, Robert Rigby and Fernanda De Bastiani
}
\note{
For the function \code{GAF()}, \eqn{\mu} is the mean and \eqn{\sigma \mu^{\nu/2}}
is the standard deviation of the gamma family.
The \code{GAF} is design for fitting regression type models where the variance is proportional to a power of the mean. 

Note that because the high correlation between the \code{sigma} and the \code{nu} parameter the \code{mixed()} method should be used in the fitting.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{gamlss.family}}, \code{\link{GA}}, \code{\link{GG}}
}
\examples{
GAF()
\dontrun{
m1<-gamlss(y~poly(x,2),data=abdom,family=GAF, method=mixed(1,100),  
           c.crit=0.00001)
# using RS()
m2<-gamlss(y~poly(x,2),data=abdom,family=GAF,  n.cyc=5000, c.crit=0.00001)
# the estimates of nu slightly different
fitted(m1, "nu")[1]
fitted(m2, "nu")[1]
# global deviance almost identical
AIC(m1, m2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
