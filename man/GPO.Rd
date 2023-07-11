\name{GPO}
\alias{GPO}
\alias{dGPO}
\alias{pGPO}
\alias{qGPO}
\alias{rGPO}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{The generalised Poisson distribution}
\description{
The \code{GPO()} function defines the generalised Poisson distribution, a two parameter discrete distribution, for a \code{gamlss.family} object to be used  in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dGPO}, \code{pGPO}, \code{qGPO} and \code{rGPO} define the density, distribution function, quantile function and random
generation for the Delaporte \code{GPO()}, distribution.
}
\usage{
GPO(mu.link = "log", sigma.link = "log")

dGPO(x, mu = 1, sigma = 1, log = FALSE)

pGPO(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)

qGPO(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE, max.value = 10000)

rGPO(n, mu = 1, sigma = 1,  max.value = 10000)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the \code{mu} parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive \code{mu}}
  \item{sigma}{vector of positive dispersion parameter \code{sigma}}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm  should look for q}
}
\details{
The probability function of the Generalised Poisson distribution is given by
\deqn{P(Y=y|\mu,\sigma)= \left(\frac{\mu}{1+\sigma \mu}\right)^y  \frac{\left(1+\sigma y \right)^{y-1}}{y!} \exp \left[\frac{- \mu \left( 1+\sigma y\right)}{1+\sigma \mu} \right] }{f(y|mu,sigma)= (\mu/ (1+\sigma+\mu))^y  (1+\sigma y)^(y-1)/y! exp(-\mu (1+\sigma y)/(1+\sigma+\mu)) } 
for \eqn{y=0,1,2,...,\infty}{y=0,1,2,...} 
where  \eqn{\mu>0}{mu>0} and \eqn{\sigma>0}{\sigma>0} see pp. 481-483 of Rigby \emph{et al}. (2019).
} 
  
\value{
  Returns a \code{gamlss.family} object which can be used to fit a Generalised Poisson distribution in the \code{gamlss()} function. 
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.
	
Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\author{Rigby, R. A.,  Stasinopoulos D. M.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{gamlss.family}}, \code{\link{PO}} , \code{\link{DPO}}
}
\examples{
GPO()# gives information about the default links for the
#plot the pdf using plot 
plot(function(y) dGPO(y, mu=10, sigma=1 ), from=0, to=100, n=100+1, type="h") # pdf
# plot the cdf
plot(seq(from=0,to=100),pGPO(seq(from=0,to=100), mu=10, sigma=1), type="h")   # cdf
# generate random sample
tN <- table(Ni <- rGPO(100, mu=5, sigma=1))
r <- barplot(tN, col='lightblue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
