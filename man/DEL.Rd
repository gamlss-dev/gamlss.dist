\name{DEL}
\alias{DEL}
\alias{dDEL}
\alias{pDEL}
\alias{qDEL}
\alias{rDEL}


\title{The Delaporte distribution for fitting a GAMLSS model}
\description{
The \code{DEL()} function defines the Delaporte distribution, a three parameter discrete distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dDEL}, \code{pDEL}, \code{qDEL} and \code{rDEL} define the density, distribution function, quantile function and random
generation for the Delaporte \code{DEL()}, distribution. 
}
\usage{
DEL(mu.link = "log", sigma.link = "log", nu.link = "logit")
dDEL(x, mu=1, sigma=1, nu=0.5, log=FALSE)
pDEL(q, mu=1, sigma=1, nu=0.5, lower.tail = TRUE, 
        log.p = FALSE)
qDEL(p, mu=1, sigma=1, nu=0.5,  lower.tail = TRUE, 
     log.p = FALSE,  max.value = 10000)        
rDEL(n, mu=1, sigma=1, nu=0.5, max.value = 10000)
}


\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link}, with "logit" link as the default for the nu parameter }
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive mu}
  \item{sigma}{vector of positive dispersion parameter}
   \item{nu}{vector of nu}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm  should look for q}
}
\details{
The probability function of the Delaporte distribution is given by
\deqn{f(y|\mu,\sigma,\nu)= \frac{e^{-\mu \nu}}{\Gamma(1/\sigma)}\left[ 1+\mu \sigma (1-\nu)\right]^{-1/\sigma} S }{f(y|mu,sigma,nu)=(exp(-mu*nu)/Gamma(1/sigma))*[1+mu*sigma*(1-nu)]^(-1/sigma) S} 
where  
\deqn{S=  \sum_{j=0}^{y} \left( \matrix{ y \cr j } \right) \frac{\mu^y \nu^{y-j}}{y!}\left[\mu + \frac{1}{\sigma(1-\nu)}\right]^{-j} \Gamma\left(\frac{1}{\sigma}+j\right)}{S=Sum(Per(y,j))*((mu^y)*(nu^{y-j})/y!) *[1+(1/(sigma*(1-nu)))]^j Gamma((1/sigma)*j)}
for \eqn{y=0,1,2,...,\infty}{y=0,1,2,...} 
where  \eqn{\mu>0}{mu>0} , \eqn{\sigma>0}{\sigma>0} and \eqn{0 <
\nu<1}{0<nu<1}.
 This distribution is a parametrization of the distribution given by Wimmer and Altmann (1999) p 515-516 where 
 \eqn{\alpha=\mu \nu}{a=mu*nu}, \eqn{k=1/\sigma}{1/sigma} and \eqn{\rho=[1+\mu\sigma(1-\nu)]^{-1}}{p=[1+mu*nu*(1-nu)]^(-1)} 
  
}
\value{
  Returns a \code{gamlss.family} object which can be used to fit a Delaporte distribution in the \code{gamlss()} function. 
 }
\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos D. M. and Akantziliotou, C. (2006) Modelling the parameters of a family of mixed Poisson distributions including the Sichel and Delaptorte.
Submitted for publication.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Wimmer, G. and Altmann, G (1999). \emph{Thesaurus of univariate discrete probability distributions} . Stamn Verlag, Essen, Germany 

}
\author{Rigby, R. A. and  Stasinopoulos D. M. }
\note{ The
mean of \eqn{Y} is given by \eqn{E(Y)=\mu}{E(Y)=mu} and the variance by \eqn{V(Y)=\mu+\mu^2 \sigma \left( 1-\nu\right)^2}{V(Y)=mu+mu^2*sigma*(1-nu)^2}.
 }
 

\seealso{ \code{\link{gamlss.family}}, \code{\link{SI}} , \code{\link{SICHEL}}}
\examples{
 DEL()# gives information about the default links for the  Delaporte distribution 
#plot the pdf using plot 
plot(function(y) dDEL(y, mu=10, sigma=1, nu=.5), from=0, to=100, n=100+1, type="h") # pdf
# plot the cdf
plot(seq(from=0,to=100),pDEL(seq(from=0,to=100), mu=10, sigma=1, nu=0.5), type="h")   # cdf
# generate random sample
tN <- table(Ni <- rDEL(100, mu=10, sigma=1, nu=0.5))
r <- barplot(tN, col='lightblue')
# fit a model to the data 
# libary(gamlss)
# gamlss(Ni~1,family=DEL, control=gamlss.control(n.cyc=50))
}
\keyword{distribution}
\keyword{regression}% 
