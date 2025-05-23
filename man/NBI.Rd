\name{NBI}

\alias{NBI}
\alias{dNBI}
\alias{pNBI}
\alias{qNBI}
\alias{rNBI}

\title{Negative Binomial type I distribution for fitting a GAMLSS}
\description{
The \code{NBI()} function defines the Negative Binomial type I distribution, a two parameter distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dNBI}, \code{pNBI}, \code{qNBI} and \code{rNBI} define the density, distribution function, quantile function and random
generation for the Negative Binomial type I, \code{NBI()}, distribution. 
}
\usage{
NBI(mu.link = "log", sigma.link = "log")
dNBI(x, mu = 1, sigma = 1, log = FALSE)
pNBI(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qNBI(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rNBI(n, mu = 1, sigma = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of positive despersion parameter}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
}
\details{
  Definition file for Negative Binomial type I distribution. 
 \deqn{P(Y=y|\mu, \sigma)=
\frac{\Gamma(y+\frac{1}{\sigma})}{\Gamma(\frac{1}{\sigma})
\Gamma(y+1)}\hspace{1mm}\left( \frac{\sigma \mu}{1+\sigma
\mu}\right)^y \hspace{1mm}\left( \frac{1}{1+\sigma \mu}
\right)^{1/\sigma}}{P(Y=y|\mu, \sigma)=
\Gamma(y+1/\sigma)/\Gamma(1/\sigma) \Gamma(y+1) ((\sigma \mu)/ (1+\sigma
\mu))^y(1/(1+\sigma \mu))^{1/\sigma}} 
 for \eqn{y=0,1,2,\ldots,\infty}{y=0,1,2, ...,Inf}, \eqn{\mu>0} and \eqn{\sigma>0}. This
parameterization is equivalent to that used by Anscombe (1950) except he used \eqn{\alpha=1/\sigma}{alpha=1/sigma} instead of \eqn{\sigma}{sigma}, see also pp. 483-485 of Rigby \emph{et al}. (2019). The expected value is \eqn{E(y)=\mu} and the variance  \eqn{E(y)=\mu+\sigma \mu^2}.
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a Negative Binomial type I distribution in the \code{gamlss()} function.   
}

\references{
Anscombe, F. J. (1950) Sampling theory of the negative bimomial and logarithmic distributiona, \emph{Biometrika}, \bold{37}, 358-382.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).
 }
\author{Mikis Stasinopoulos, Bob Rigby  and Calliope Akantziliotou}
\note{\eqn{\mu}{mu} is the mean and \eqn{(\mu+\sigma \mu^2)^{0.5}}{(mu+sigma*mu^2)^0.5} is the standard deviation of the Negative Binomial 
type I distribution (so \eqn{\sigma}{sigma} is the dispersion parameter in the usual GLM for the negative binomial type I distribution)}

\section{Warning }{For values of \eqn{\sigma<0.0001}{sigma<0.0001}  the d,p,q,r functions switch to the Poisson distribution} 

\seealso{  \code{\link{gamlss.family}}, \code{\link{NBII}}, \code{\link{PIG}}, \code{\link{SI}}}

\examples{
NBI()   # gives information about the default links for the Negative Binomial type I distribution  
# plotting the distribution
plot(function(y) dNBI(y, mu = 10, sigma = 0.5 ), from=0, to=40, n=40+1, type="h")
# creating random variables and plot them 
tN <- table(Ni <- rNBI(1000, mu=5, sigma=0.5))
r <- barplot(tN, col='lightblue')
# library(gamlss)
# data(aids)
# h<-gamlss(y~cs(x,df=7)+qrt, family=NBI, data=aids) # fits the model 
# plot(h)
# pdf.plot(family=NBI, mu=10, sigma=0.5, min=0, max=40, step=1)
}
\keyword{distribution}
\keyword{regression}% 
