\name{PIG}
\alias{PIG}
\alias{PIG2}
\alias{dPIG}
\alias{dPIG2}
\alias{pPIG}
\alias{pPIG2}
\alias{qPIG}
\alias{qPIG2}
\alias{rPIG}
\alias{rPIG2}
\alias{ZIPIG}
\alias{dZIPIG}
\alias{pZIPIG}
\alias{qZIPIG}
\alias{rZIPIG}
\alias{ZAPIG}
\alias{dZAPIG}
\alias{pZAPIG}
\alias{qZAPIG}
\alias{rZAPIG}
\alias{PIG2}

\title{The Poisson-inverse Gaussian distribution for fitting a GAMLSS model}
\description{
The \code{PIG()} function defines the Poisson-inverse Gaussian distribution, a two parameter distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.   The \code{PIG2()} function  is a repametrization of \code{PIG()} where \code{mu} and \code{sigma} are orthogonal see Heller \emph{et al.} (2018).  


The functions \code{dPIG}, \code{pPIG}, \code{qPIG} and \code{rPIG} define the density, distribution function, quantile function and random
generation for the Poisson-inverse Gaussian \code{PIG()}, distribution.  Also \code{dPIG2}, \code{pPIG2}, \code{qPIG2} and \code{rPIG2} are the equivalent functions for \code{PIG2()}

The functions \code{ZAPIG()} and \code{ZIPIG()} are the zero adjusted (hurdle) and zero inflated versions of the  Poisson-inverse Gaussian distribution, respectively. That is three  parameter distributions. 


The functions \code{dZAPIG}, \code{dZIPIG}, \code{pZAPIG},\code{pZIPIG}, \code{qZAPIG} \code{qZIPIG}  \code{rZAPIG} and  \code{rZIPIG} define the probability,  cumulative, quantile  and random
generation functions for the zero adjusted and zero inflated  Poisson Inverse Gaussian distributions, \code{ZAPIG()}, \code{ZIPIG()}, respectively.

}
\usage{
PIG(mu.link = "log", sigma.link = "log")
dPIG(x, mu = 1, sigma = 1, log = FALSE)
pPIG(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qPIG(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE, 
     max.value = 10000)
rPIG(n, mu = 1, sigma = 1, max.value = 10000)

PIG2(mu.link = "log", sigma.link = "log")
dPIG2(x, mu=1, sigma=1, log = FALSE)
pPIG2(q, mu=1, sigma=1, lower.tail = TRUE, log.p = FALSE)
qPIG2(p, mu=1, sigma=1, lower.tail = TRUE, log.p = FALSE, 
     max.value = 10000)
rPIG2(n, mu=1, sigma=1)

ZIPIG(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZIPIG(x, mu = 1, sigma = 1, nu = 0.3, log = FALSE)
pZIPIG(q, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
qZIPIG(p, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE, 
       max.value = 10000)
rZIPIG(n, mu = 1, sigma = 1, nu = 0.3, max.value = 10000)

ZAPIG(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZAPIG(x, mu = 1, sigma = 1, nu = 0.3, log = FALSE)
pZAPIG(q, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
qZAPIG(p, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE, 
      max.value = 10000)
rZAPIG(n, mu = 1, sigma = 1, nu = 0.3, max.value = 10000)
}


\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{Defines the \code{mu.link}, with "logit" link as the default for the nu parameter}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of positive dispersion parameter}
  \item{nu}{vector of zero probability parameter}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm  should look for q}
                    
}
\details{
 The probability function of the Poisson-inverse Gaussian distribution \code{PIG}, is given by
 
\deqn{f(y|\mu,\sigma)=\left( \frac{2 \alpha}{\pi} \right)^{\frac{1}{2}} \frac{\mu^y e^{\frac{1}{\sigma}} K_{y-\frac{1}{2}}(\alpha)}{(\alpha \sigma)^y y!}}{f(y|mu,sigma)=(2*alpha/pi)^.5  mu^y e^(1/sigma) K(alpha)/(alpha*sigma)^y y!} 
where  \eqn{\alpha^2=\frac{1}{\sigma^2}+\frac{2\mu}{\sigma}}, for  \eqn{y=0,1,2,...,\infty} where   \eqn{\mu>0} and  \eqn{\sigma>0} and \eqn{
K_{\lambda}(t)=\frac{1}{2}\int_0^{\infty} x^{\lambda-1} \exp\{-\frac{1}{2}t(x+x^{-1})\}dx} is the modified Bessel function of the third kind, see also pp. 487-489 of Rigby \emph{et al}. (2019).
[Note that the above parameterization was used by Dean, Lawless and Willmot(1989). It is also a special case of the Sichel distribution \code{SI()} when  \eqn{\nu=-\frac{1}{2}}.] In this parameterization \eqn{E(y)= \mu} and \eqn{Var(y)=\mu+\sigma \mu^2}.  


The probability function of the Poisson-inverse Gaussian distribution \code{PIG2}, see Heller, Couturier and Heritier (2018), is given by
 
\deqn{f(y|\mu,\sigma)=\left( \frac{2 \sigma}{\pi} \right)^{\frac{1}{2}} 
\frac{\mu^y e^{\frac{1}{\alpha}} K_{y-\frac{1}{2}}(\sigma)}{(\alpha \sigma)^y y!}}{f(y|mu,sigma)=(2*alpha/pi)^.5  mu^y e^(1/sigma) K(alpha)/(alpha*sigma)^y y!} 
for    \eqn{y=0,1,2,...,\infty},  \eqn{\mu>0} and  \eqn{\sigma>0}  and \eqn{\alpha = \left[(\mu^2+\sigma^2)^{0.5}-\mu \right]^{-1}}, \eqn{K_{\lambda}(t)=\frac{1}{2}\int_0^{\infty} x^{\lambda-1} \exp\{-\frac{1}{2}t(x+x^{-1})\}dx} is the modified Bessel function of the third kind, see  pp. 487-489 of Rigby \emph{et al}. (2019).  In this parameterization \eqn{E(y)= \mu} and \eqn{\sigma} is a despertion parameter.  


The  definition of the zero adjusted Poison inverse Gaussian  distribution, \code{ZAPIG} and  the the zero inflated Poison inverse Gaussian  distribution, \code{ZIPIG}, are given in p. 513 and pp. 514-515 of  of Rigby \emph{et al.} (2019), respectively.

}
\value{
  Returns a \code{gamlss.family} object which can be used to fit a  Poisson-inverse Gaussian distribution in the \code{gamlss()} function.   
}
\references{ 
Dean, C., Lawless,  J. F. and Willmot,  G. E., A mixed poisson-inverse-Gaussian regression model, \emph{Canadian J. Statist.},
\bold{17}, 2, pp 171-181

Heller, G. Z., Couturier, D.L. and Heritier, S. R. (2018) Beyond mean modelling: Bias due to misspecification of dispersion in Poisson-inverse Gaussian regression
\emph{Biometrical Journal}, \bold{2}, pp 333-342.

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
\author{Dominique-Laurent Couturier, Mikis Stasinopoulos, Bob Rigby and Marco Enea}


\seealso{ \code{\link{gamlss.family}},  \code{\link{NBI}}, \code{\link{NBII}}, 
\code{\link{SI}}, \code{\link{SICHEL}}}
\examples{
PIG()# gives information about the default links for the  Poisson-inverse Gaussian distribution 
#plot the pdf using plot 
plot(function(y) dPIG(y, mu=10, sigma = 1 ), from=0, to=50, n=50+1, type="h") # pdf
# plot the cdf
plot(seq(from=0,to=50),pPIG(seq(from=0,to=50), mu=10, sigma=1), type="h")   # cdf
# generate random sample
tN <- table(Ni <- rPIG(100, mu=5, sigma=1))
r <- barplot(tN, col='lightblue')
# fit a model to the data 
# library(gamlss)
# gamlss(Ni~1,family=PIG)
ZIPIG()
ZAPIG()
}
\keyword{distribution}
\keyword{regression}% 
