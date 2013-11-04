\name{SICHEL}
\alias{SICHEL}
\alias{dSICHEL}
\alias{pSICHEL}
\alias{qSICHEL}
\alias{rSICHEL}
\alias{VSICHEL}


\title{The Sichel distribution for fitting a GAMLSS model}
\description{
The \code{SICHEL()} function defines the Sichel distribution, a three parameter discrete distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dSICHEL}, \code{pSICHEL}, \code{qSICHEL} and \code{rSICHEL} define the density, distribution function, quantile function and random
generation for the Sichel \code{SICHEL()}, distribution. The function \code{VSICHEL} gives the variance of a fitted Sichel model.  
}
\usage{
SICHEL(mu.link = "log", sigma.link = "log", nu.link = "identity")
dSICHEL(x, mu=1, sigma=1, nu=-0.5, log=FALSE)
pSICHEL(q, mu=1, sigma=1, nu=-0.5, lower.tail = TRUE, 
         log.p = FALSE)
qSICHEL(p, mu=1, sigma=1, nu=-0.5,  lower.tail = TRUE, 
        log.p = FALSE, max.value = 10000)        
rSICHEL(n, mu=1, sigma=1, nu=-0.5, max.value = 10000)
VSICHEL(obj)
}


\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{ Defines the \code{nu.link}, with "identity" link as the default for the nu parameter }
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive mu}
  \item{sigma}{vector of positive despersion parameter}
   \item{nu}{vector of nu}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm  should look for q}
  \item{obj}{a fitted Sichel gamlss model}
}
\details{
The probability function of the Sichel distribution is given by
\deqn{f(y|\mu,\sigma,\nu)= \frac{\mu^y K_{y+\nu}(\alpha)}{c^y(\alpha \sigma)^{y+\nu} y! K_\nu(\frac{1}{\sigma})}}{f(y|mu,sigma,nu)=mu^y Ky+n(alpha)/(alpha sigma)^(y+v) y! Knu(1/sigma)} 
where  \eqn{\alpha^2=\frac{1}{\sigma^2}+\frac{2\mu}{c \sigma}}{alpha^2=1/sigma^2 +2*mu/c*sigma},
and \eqn{c=R_{\nu}(1/\sigma)=K_{\nu+1}(1/\sigma) \left[K_{\nu}(1/\sigma) \right]^{-1}}{c=Rv(1/sigma)=Kv+1(1/sigma)/Kv(1/sigma)} 
for \eqn{y=0,1,2,...,\infty}{y=0,1,2,...} 
where  \eqn{\mu>0}{mu>0} , \eqn{\sigma>0}{\sigma>0} and \eqn{-\infty <
\nu<\infty}{-Inf<nu<Inf} and \eqn{K_{\lambda}(t)=\frac{1}{2}\int_0^{\infty} x^{\lambda-1} \exp\{-\frac{1}{2}t(x+x^{-1})\}dx} is the 
modified Bessel function of the third kind.
Note that the above parameterization is different from Stein, Zucchini and Juritz (1988) who use the above probability function 
but treat
\eqn{\mu}{mu}, \eqn{\alpha}{alpha}  and \eqn{\nu}{nu} as the parameters. 
 
}
\value{
  Returns a \code{gamlss.family} object which can be used to fit a Sichel distribution in the \code{gamlss()} function. 
 }
\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos D. M. and Akantziliotou, C. (2006) Modelling the parameters of a family of mixed Poisson distributions including the Sichel and Delaptorte.
Submitted for publication.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stein, G. Z., Zucchini, W. and Juritz, J. M. (1987). Parameter
Estimation of the Sichel Distribution and its Multivariate Extension.
\emph{Journal of American Statistical Association}, \bold{82}, 938-944.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

}
\author{Rigby, R. A.,  Stasinopoulos D. M. and Akantziliotou C. }
\note{The mean of the above Sichel distribution is \eqn{\mu}{mu} and the variance is 
\eqn{\mu^2 \left[\frac{2\sigma (\nu+1)}{c} + \frac{1}{c^2}-1\right]}{mu^2 *( 2*sigma*(nu+1)/c + (1/c^2)-1 )}
}  
 

\seealso{ \code{\link{gamlss.family}}, \code{\link{PIG}} , \code{\link{SI}}}
\examples{
SICHEL()# gives information about the default links for the  Sichel distribution 
#plot the pdf using plot 
plot(function(y) dSICHEL(y, mu=10, sigma=1, nu=1), from=0, to=100, n=100+1, type="h") # pdf
# plot the cdf
plot(seq(from=0,to=100),pSICHEL(seq(from=0,to=100), mu=10, sigma=1, nu=1), type="h")   # cdf
# generate random sample
tN <- table(Ni <- rSICHEL(100, mu=5, sigma=1, nu=1))
r <- barplot(tN, col='lightblue')
# fit a model to the data 
# library(gamlss)
# gamlss(Ni~1,family=SICHEL, control=gamlss.control(n.cyc=50))
}
\keyword{distribution}
\keyword{regression}% 
