\name{SICHEL}
\alias{SICHEL}
\alias{dSICHEL}
\alias{pSICHEL}
\alias{qSICHEL}
\alias{rSICHEL}
\alias{VSICHEL}
\alias{tofySICHEL}
\alias{ZASICHEL}
\alias{dZASICHEL}
\alias{pZASICHEL}
\alias{qZASICHEL}
\alias{rZASICHEL}
\alias{ZISICHEL}
\alias{dZISICHEL}
\alias{pZISICHEL}
\alias{qZISICHEL}
\alias{rZISICHEL}

\title{The Sichel distribution for fitting a GAMLSS model}
\description{
The \code{SICHEL()} function defines the Sichel distribution, a three parameter discrete distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dSICHEL}, \code{pSICHEL}, \code{qSICHEL} and \code{rSICHEL} define the density, distribution function, quantile function and random
generation for the Sichel \code{SICHEL()}, distribution. The function \code{VSICHEL} gives the variance of a fitted Sichel model.  

The functions \code{ZASICHEL()} and \code{ZISICHEL()} are the zero adjusted (hurdle) and zero inflated versions of the Sichel distribution, respectively. That is four  parameter distributions. 

The functions \code{dZASICHEL}, \code{dZISICHEL}, \code{pZASICHEL},\code{pZISICHEL}, \code{qZASICHEL} \code{qZISICHEL}  \code{rZASICHEL} and  \code{rZISICHEL} define the probability,  cumulative, quantile  and random
generation functions for the zero adjusted and zero inflated  Sichel distributions, \code{ZASICHEL()}, \code{ZISICHEL()}, respectively.

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
tofySICHEL(y, mu, sigma, nu)

ZASICHEL(mu.link = "log", sigma.link = "log", nu.link = "identity", 
         tau.link = "logit")
dZASICHEL(x, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, log = FALSE)
pZASICHEL(q, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, 
          lower.tail = TRUE, log.p = FALSE)
qZASICHEL(p, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, 
          lower.tail = TRUE, log.p = FALSE, max.value = 10000)
rZASICHEL(n, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, 
          max.value = 10000)

ZISICHEL(mu.link = "log", sigma.link = "log", nu.link = "identity", 
          tau.link = "logit")
dZISICHEL(x, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, log = FALSE)
pZISICHEL(q, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, 
         lower.tail = TRUE, log.p = FALSE)
qZISICHEL(p, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, 
         lower.tail = TRUE, log.p = FALSE, max.value = 10000)
rZISICHEL(n, mu = 1, sigma = 1, nu = -0.5, tau = 0.1, 
         max.value = 10000)
}


\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the \code{mu} parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter}
  \item{nu.link}{ Defines the \code{nu.link}, with "identity" link as the default for the \code{nu} parameter }
    \item{tau.link}{Defines the \code{tau.link}, with "logit" link as the default for the \code{tau} parameter }
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive \code{mu}}
  \item{sigma}{vector of positive dispersion parameter \code{sigma}}
   \item{nu}{vector of \code{nu}}
  \item{tau}{vector of probabilities \code{tau}}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm  should look for q}
  \item{obj}{a fitted Sichel gamlss model}
  \item{y}{the y variable,  the \code{tofySICHEL()} should not be used on its own.}
}
\details{
The probability function of the Sichel distribution \code{SICHEL} is given by
\deqn{f(y|\mu,\sigma,\nu)= \frac{(\mu/b)^y K_{y+\nu}(\alpha)}{y!(\alpha \sigma)^{y+\nu} K_\nu(\frac{1}{\sigma})}}{f(y|\mu,\sigma,\nu)=[\mu^y K_{y+n}(\alpha)] / [y! (\alpha \sigma)^(y+\nu)  K_\nu(1/\sigma)]} 

for \eqn{y=0,1,2,...,\infty}{y=0,1,2,...},   \eqn{\mu>0}{mu>0} , \eqn{\sigma>0}{\sigma>0} and \eqn{-\infty <\nu<\infty}{-Inf<\nu<Inf} where 

 \deqn{\alpha^2=\frac{1}{\sigma^2}+\frac{2\mu}{\sigma}}{\alpha^2= 1/\sigma^2 +2*\mu/\sigma} 
 
\deqn{c=K_{\nu+1}(1/\sigma) / K_{\nu}(1/\sigma)}{c=K_{\nu+1} (1/\sigma)/K_{\nu}(1/\sigma)}
and \eqn{K_{\lambda}(t)}{K_{lambda}(t)} 
is the modified Bessel function of the third kind see pp 508-510 of Rigby \emph{et al.} (2019).
Note that the above parametrization is different from Stein, Zucchini and Juritz (1988) who use the above probability function 
but treat
\eqn{\mu}{mu}, \eqn{\alpha}{alpha}  and \eqn{\nu}{nu} as the parameters. 


The  definition of the zero adjusted Sichel distribution, \code{ZASICHEL} and  the the zero inflated Sichel  distribution, \code{ZISICHEL}, are given in pp. 517-518 and pp. 519-520 of  of Rigby \emph{et al.} (2019), respectively.

}
\value{
  Returns a \code{gamlss.family} object which can be used to fit a Sichel distribution in the \code{gamlss()} function. 
 }
\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Rigby, R. A., Stasinopoulos, D. M., & Akantziliotou, C. (2008). A framework for modelling overdispersed count data, including the Poisson-shifted generalized inverse Gaussian distribution. \emph{Computational Statistics & Data Analysis}, 53(2), 381-393.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  


Stein, G. Z., Zucchini, W. and Juritz, J. M. (1987). Parameter
Estimation of the Sichel Distribution and its Multivariate Extension.
\emph{Journal of American Statistical Association}, \bold{82}, 938-944.

(see also \url{https://www.gamlss.com/}).
}
\author{Rigby, R. A.,  Stasinopoulos D. M.,  Akantziliotou C and Marco Enea. }
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
