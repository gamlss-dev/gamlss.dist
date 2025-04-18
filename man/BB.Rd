\name{BB}
\alias{BB}
\alias{dBB} 
\alias{pBB} 
\alias{qBB} 
\alias{rBB}


\title{Beta Binomial Distribution For Fitting a GAMLSS Model}
\description{
This function defines the beta binomial distribution, a two parameter distribution, for a \code{gamlss.family} object to be used in a
GAMLSS fitting using the function gamlss()   
}
\usage{
BB(mu.link = "logit", sigma.link = "log")
dBB(x, mu = 0.5, sigma = 1, bd = 10, log = FALSE)
pBB(q, mu = 0.5, sigma = 1, bd = 10, lower.tail = TRUE,
      log.p = FALSE)
qBB(p, mu = 0.5, sigma = 1, bd = 10, lower.tail = TRUE, 
       log.p = FALSE)
rBB(n, mu = 0.5, sigma = 1, bd = 10)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "logit" link as the default for the mu parameter. Other links are "probit" and "cloglog"'(complementary log-log)}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter. Other links are "inverse", "identity" and "sqrt" }
  \item{mu}{vector of positive probabilities}
  \item{sigma}{the dispersion parameter}
  \item{bd}{vector of binomial denominators}
  \item{p}{vector of probabilities}
  \item{x,q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}
}
\details{
  Definition file for beta binomial distribution. 
 \deqn{f(y|\mu,\sigma)=\frac{\Gamma(n+1)} {\Gamma(y+1)\Gamma(n-y+1)} \frac{\Gamma(\frac{1}{\sigma}) \Gamma(y+\frac{\mu}{\sigma}) \Gamma[n+\frac{(1-\mu)}{\sigma}-y]}{\Gamma(n+\frac{1}{\sigma}) \Gamma(\frac{\mu}{\sigma}) \Gamma(\frac{1-\mu}{\sigma})}}{f(y|mu,sigma)=[Gamma(n+1)/Gamma(y+1)/Gamma(n-y+1)]*[Gamma(y+mu/sigma)*Gamma(1/sigma)*Gamma[n+(1-mu)/sigma-y]]/[Gamma(n+(1/sigma)) * Gamma(mu/sigma) * Gamma((1-mu)/sigma)]}
 for \eqn{y=0,1,2,\ldots,n}, \eqn{0<\mu<1} and \eqn{\sigma>0}, see pp. 523-524   of Rigby \emph{et al}. (2019). . For \eqn{\mu=0.5} and \eqn{\sigma=0.5} the distribution is uniform. 
}

\value{
 Returns a \code{gamlss.family} object which can be used to fit a Beta Binomial distribution in the \code{gamlss()} function.   
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape, (with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R},
	Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).
}
\author{ Mikis Stasinopoulos, Bob Rigby and Kalliope Akantziliotou }
\note{The response variable should be a matrix containing two columns, the first with the count of successes and the second with the count of failures. 
      The parameter \code{mu} represents a probability parameter with limits \eqn{0 < \mu < 1}{0 < mu <1}.
       \eqn{n \mu}{n*mu} is the mean of the distribution where n is the binomial denominator. 
       \eqn{\{n \mu (1-\mu)[1+(n-1) \sigma/(\sigma+1)]\}^{0.5}}{[n mu (1-mu)[1+(n-1) sigma/(sigma+1)]\]^0.5} is the standard deviation of the 
       Beta Binomial distribution. Hence \eqn{\sigma}{sigma} is a dispersion type parameter}

\section{Warning }{The functions \code{pBB} and \code{qBB} are calculated using a laborious procedure so they are relatively slow.} 

\seealso{ \code{\link{gamlss.family}}, \code{\link{BI}}, }

\examples{
# BB()# gives information about the default links for the Beta Binomial distribution 
#plot the pdf
plot(function(y) dBB(y, mu = .5, sigma = 1, bd =40), from=0, to=40, n=40+1, type="h")
#calculate the cdf and plotting it
ppBB <- pBB(seq(from=0, to=40), mu=.2 , sigma=3, bd=40)
plot(0:40,ppBB, type="h")
#calculating quantiles and plotting them  
qqBB <- qBB(ppBB, mu=.2 , sigma=3, bd=40)
plot(qqBB~ ppBB)
# when the argument fast is useful
p <- pBB(c(0,1,2,3,4,5), mu=.01 , sigma=1, bd=5)
qBB(p, mu=.01 , sigma=1, bd=5)
#  0 1 1 2 3 5
qBB(p, mu=.01 , sigma=1, bd=5)
#  0 1 2 3 4 5
# generate random sample
tN <- table(Ni <- rBB(1000, mu=.2, sigma=1, bd=20))
r <- barplot(tN, col='lightblue')
# fitting a model 
# library(gamlss)
#data(aep)   
# fits a Beta-Binomial model
#h<-gamlss(y~ward+loglos+year, sigma.formula=~year+ward, family=BB, data=aep) 
}
\keyword{distribution}
\keyword{regression}% 
