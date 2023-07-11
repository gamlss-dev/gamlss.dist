\name{NBF}
\alias{NBF}
\alias{dNBF}
\alias{pNBF}
\alias{qNBF}
\alias{rNBF}
\alias{ZINBF}
\alias{dZINBF}
\alias{pZINBF}
\alias{qZINBF}
\alias{rZINBF}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Negative Binomial Family distribution for fitting a GAMLSS}
\description{
The \code{NBF()} function defines the Negative Binomial family distribution, a three parameter distribution, for a \code{gamlss.family} object to be used  in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dNBF}, \code{pNBF}, \code{qNBF} and \code{rNBF} define the density, distribution function, quantile function and random generation for the negative binomial family, \code{NBF()}, distribution.

The functions \code{dZINBF}, \code{pZINBF}, \code{qZINBF} and \code{rZINBF} define the density, distribution function, quantile function and random generation for the zero inflated negative binomial family, \code{ZINBF()}, distribution a four parameter distribution.
}
\usage{
NBF(mu.link = "log", sigma.link = "log", nu.link = "log")

dNBF(x, mu = 1, sigma = 1, nu = 2, log = FALSE)

pNBF(q, mu = 1, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)

qNBF(p, mu = 1, sigma = 1, nu = 2, lower.tail = TRUE, log.p = FALSE)

rNBF(n, mu = 1, sigma = 1, nu = 2)

ZINBF(mu.link = "log", sigma.link = "log", nu.link = "log", 
      tau.link = "logit")
      
dZINBF(x, mu = 1, sigma = 1, nu = 2, tau = 0.1, log = FALSE)

pZINBF(q, mu = 1, sigma = 1, nu = 2, tau = 0.1, lower.tail = TRUE, 
      log.p = FALSE)
      
qZINBF(p, mu = 1, sigma = 1, nu = 2, tau = 0.1, lower.tail = TRUE, 
      log.p = FALSE)      
      
rZINBF(n, mu = 1, sigma = 1, nu = 2, tau = 0.1)      
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{The link function for \code{mu}}
  \item{sigma.link}{The link function for \code{sigma}}
  \item{nu.link}{The link function for \code{nu}}
  \item{tau.link}{The link function for \code{tau}}
  \item{x}{vector of (non-negative integer)}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of positive dispersion parameter}
  \item{nu}{vector of power parameter}
  \item{tau}{vector of inflation parameter}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x]}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
}

\details{
 The definition for Negative Binomial Family distribution , \code{NBF}, is similar to the Negative Binomial type I. The probability function of the \code{NBF} can be obtained by replacing \eqn{\sigma} with  \eqn{\sigma \mu^{\nu-2}} where \eqn{\nu} is a power parameter.
The distribution has mean \eqn{\mu} and variance \eqn{\mu+\sigma \mu^{\nu}.} 
For more details see pp 507-508 of Rigby \emph{et al.} (2019).   

The zero inflated negative binomial family \code{ZINBF} is defined as an inflated at zero \code{NBF}.
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a Negative Binomial Family distribution in the \code{gamlss()} function. 
}
\references{
Anscombe, F. J. (1950) Sampling theory of the negative binomial and logarithmic distributions, \emph{Biometrika}, \bold{37}, 358-382.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).

}
\author{Bob Rigby and Mikis Stasinopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{NBI}}, \code{\link{NBII}}}
\examples{
NBF() # default link functions for the Negative Binomial Family 
# plotting the distribution
plot(function(y) dNBF(y, mu = 10, sigma = 0.5, nu=2 ), from=0, 
     to=40, n=40+1, type="h")
# creating random variables and plot them 
tN <- table(Ni <- rNBF(1000, mu=5, sigma=0.5, nu=2))
r <- barplot(tN, col='lightblue')
# zero inflated NBF
ZINBF() # default link functions  for the zero inflated NBF 
# plotting the distribution
plot(function(y) dZINBF(y, mu = 10, sigma = 0.5, nu=2, tau=.1 ), 
     from=0, to=40, n=40+1, type="h")
# creating random variables and plot them 
tN <- table(Ni <- rZINBF(1000, mu=5, sigma=0.5, nu=2, tau=0.1))
r <- barplot(tN, col='lightblue')
\dontrun{
library(gamlss)
data(species)
species <- transform(species, x=log(lake))
m6 <- gamlss(fish~poly(x,2), sigma.fo=~1, data=species, family=NBF, 
          n.cyc=200)
fitted(m6, "nu")[1]
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
