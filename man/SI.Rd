\name{SI}
\alias{SI}
\alias{dSI}
\alias{pSI}
\alias{qSI}
\alias{rSI}
\alias{tofyS}


\title{The Sichel dustribution for fitting a GAMLSS model}
\description{
The \code{SI()} function defines the Sichel distribution, a three parameter discrete distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dSI}, \code{pSI}, \code{qSI} and \code{rSI} define the density, distribution function, quantile function and random
generation for the Sichel \code{SI()}, distribution. 
}
\usage{
SI(mu.link = "log", sigma.link = "log", nu.link = "identity")
dSI(x, mu = 0.5, sigma = 0.02, nu = -0.5, log = FALSE)
pSI(q, mu = 0.5, sigma = 0.02, nu = -0.5, lower.tail = TRUE, 
       log.p = FALSE)
qSI(p, mu = 0.5, sigma = 0.02, nu = -0.5, lower.tail = TRUE, 
    log.p = FALSE, max.value = 10000)
rSI(n, mu = 0.5, sigma = 0.02, nu = -0.5)
tofyS(y, mu, sigma, nu, what = 1)
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
  \item{y}{the y variable. The function \code{tofyS()} should be not used on its own.}
   \item{what}{take values 1 or 2, for function \code{tofyS()}.}
}
\details{
The probability function of the Sichel distribution is given by
\deqn{f(y|\mu,\sigma,\nu)= \frac{\mu^y K_{y+\nu}(\alpha)}{y!  (\alpha \sigma)^{y+\nu} K_\nu(\frac{1}{\sigma})}}{f(y|mu,sigma,nu)=mu^y Ky+n(alpha)/(alpha sigma)^(y+v) y! Knu(1/sigma)} 
where  \eqn{\alpha^2=\frac{1}{\sigma^2}+\frac{2\mu}{\sigma}}{alpha^2=1/sigma^2 +2*mu/sigma}, for \eqn{y=0,1,2,...,\infty}{y=0,1,2,...} 
where  \eqn{\mu>0}{mu>0} , \eqn{\sigma>0}{\sigma>0} and \eqn{-\infty <
\nu<\infty}{-Inf<nu<Inf} and \eqn{K_{\lambda}(t)=\frac{1}{2}\int_0^{\infty} x^{\lambda-1} \exp\{-\frac{1}{2}t(x+x^{-1})\}dx} is the 
modified Bessel function of the third kind.
Note that the above parameterization is different from Stein, Zucchini and Juritz (1988) who use the above probability function but treat
  \eqn{\mu}{mu}, \eqn{\alpha}{alpha}  and \eqn{\nu}{nu} as the parameters. Note that \eqn{\sigma=[(\mu^2+\alpha^2)^{\frac{1}{2}} -\mu ]^{-1}}. See also pp 510-511 of Rigby \emph{et al.} (2019).
 
}
\value{
  Returns a \code{gamlss.family} object which can be used to fit a Sichel distribution in the \code{gamlss()} function. 
 }
\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}



Stein, G. Z., Zucchini, W. and Juritz, J. M. (1987). Parameter
Estimation of the Sichel Distribution and its Multivariate Extension.
\emph{Journal of American Statistical Association}, \bold{82}, 938-944.

(see also  \url{https://www.gamlss.com/}).
}
\author{Akantziliotou C., Rigby, R. A.,  Stasinopoulos D. M. and Marco Enea}

 

\seealso{\code{\link{gamlss.family}}, \code{\link{PIG}}, \code{\link{NBI}}, 
\code{\link{NBII}}}
\examples{
SI()# gives information about the default links for the  Sichel distribution 
#plot the pdf using plot 
plot(function(y) dSI(y, mu=10, sigma=1, nu=1), from=0, to=100, n=100+1, type="h") # pdf
# plot the cdf
plot(seq(from=0,to=100),pSI(seq(from=0,to=100), mu=10, sigma=1, nu=1), type="h")   # cdf
# generate random sample
tN <- table(Ni <- rSI(100, mu=5, sigma=1, nu=1))
r <- barplot(tN, col='lightblue')
# fit a model to the data 
# library(gamlss)
# gamlss(Ni~1,family=SI, control=gamlss.control(n.cyc=50))
}
\keyword{distribution}
\keyword{regression}% 
