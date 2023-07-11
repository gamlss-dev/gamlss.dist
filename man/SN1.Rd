\name{SN1}
\alias{SN1}
\alias{dSN1}
\alias{pSN1}
\alias{qSN1}
\alias{rSN1}
\title{
Skew Normal Type 1 distribution for fitting a GAMLSS
}
\description{
The function \code{SN1()} defines the Skew Normal Type 1 distribution, a three parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}, with parameters \code{mu}, \code{sigma} and \code{nu}. The functions \code{dSN1}, \code{pSN1}, \code{qSN1} and \code{rSN1} define the density, distribution function, quantile function and random generation for the \code{SN1} parameterization of the Skew Normal Type 1 distribution. 
}
\usage{

SN1(mu.link = "identity", sigma.link = "log", nu.link="identity")
dSN1(x, mu = 0, sigma = 1, nu = 0, log = FALSE)
pSN1(q, mu = 0, sigma = 1, nu = 0, lower.tail = TRUE, log.p = FALSE)
qSN1(p, mu = 0, sigma = 1, nu = 0, lower.tail = TRUE, log.p = FALSE)
rSN1(n, mu = 0, sigma = 1, nu = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{mu.link}{Defines the \code{mu.link}, with "`identity"' links the default for the mu parameter}
  \item{sigma.link}{Defines the \code{sigma.link}, with "`log"' as the default for the sigma parameter}
  \item{nu.link}{Defines the \code{nu.link}, with "`identity"' as the default for the sigma parameter}
  \item{x, q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of scale parameter values}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x]}
  \item{p}{vector of probabilities}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
}
\details{
The parameterization of the Skew Normal Type 1 distribution in the function \code{SN1} is 
\deqn{f(y|\mu,\sigma,\nu)=\frac{2}{\sigma}  \phi(z) \Phi(\nu z)}{f(y|mu,sigma,nu)=(nu *exp(-abs(z)^2))/(2*sigma*Gamma(1/nu))}
for \eqn{(-\infty<y<+\infty)}{y=(-Inf,+Inf)}, \eqn{(-\infty<\mu<+\infty)}{\mu=(-Inf,+Inf)}, \eqn{\sigma>0} and \eqn{(-\infty<\nu<+\infty)}{\mu=(-Inf,+Inf)} 
where  \eqn{z=(y-\mu)/ \sigma}{z=(y-mu)/(sima)} and \eqn{\phi()}{phi()} and \eqn{\Phi()}{Phi()} are the pdf and cdf of the standard nornal distribution, respectively,  see pp. 378-379 of Rigby et al. (2019).
}
\value{
returns a gamlss.family object which can be used to fit a Skew Normal Type 1 distribution in the \code{gamlss()} function.}
\author{
Mikis Stasinopoulos, Bob Rigby and Fiona McElduff
}
\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}

\note{
This is a special case of the Skew Exponential Power type 1 distribution (\code{SEP1}) where \code{tau=2}.
}

\seealso{
\code{\link{gamlss.family}}
}
\examples{
par(mfrow=c(2,2))
y<-seq(-3,3,0.2)
plot(y, dSN1(y), type="l" , lwd=2)
q<-seq(-3,3,0.2)
plot(q, pSN1(q), ylim=c(0,1), type="l", lwd=2) 
p<-seq(0.0001,0.999,0.05)
plot(p, qSN1(p), type="l", lwd=2)
dat <- rSN1(100)
hist(rSN1(100), nclass=30)
}
\keyword{distribution}
\keyword{ regression}