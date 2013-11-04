\name{make.link.gamlss}
\alias{make.link.gamlss}
\alias{show.link}
\alias{own.linkfun}
\alias{own.linkinv}
\alias{own.mu.eta}
\alias{own.valideta}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a Link for GAMLSS families
}
\description{
  The function \code{make.link.gamlss()} is used with \code{gamlss.family} distributions in package \pkg{gamlss()}.
     Given a link, it returns a link function, an inverse link  function, the derivative dpar/deta where 'par' is 
     the appropriate 
     distribution parameter and a function for checking the domain. It differs from the usual \code{make.link} of \code{glm()} 
     by having extra links as the \code{logshifto1}, and the 
     \code{own}. For the use of the \code{own} link see the example bellow. 
     \code{show.link} provides a way in which the user can identify the link functions available for each gamlss distribution. 
     If your required link function is not available for any of the gamlss distributions you can add it in. 
}
\usage{
make.link.gamlss(link)
show.link(family = "NO")
}

\arguments{
  \item{link}{character or numeric; one of \code{"logit"}, \code{"probit"},
          \code{"cloglog"}, \code{"identity"}, \code{"log"},  \code{"sqrt"},  \code{"1/mu^2"},
          \code{"inverse"}, \code{"logshifted"}, \code{"logitshifted"}, or number, say lambda resulting in power link \eqn{\mu^\lambda}{mu^lambda}.}
\item{family}{a gamlss distribution  \code{family}}

}
\details{The \code{own} link function is added to allow the user greater flexibility. 
         In order to used the own link function for any of the parameters of the distribution the \code{own} link should 
         appear in the available links for this parameter. You can check this using the function \code{show.link}. 
         If the \code{own} do not appear in the list you can create a new function for the distribution in which    
         \code{own} is added in the list. For example the first line of the code  of the binomial distribution, 
         \code{BI}, has change 
         from 
         
         "mstats <- checklink("mu.link", "Binomial", substitute(mu.link), c("logit", "probit", "cloglog", "log")),
          in version 1.0-0 of gamlss,  to 
          
         "mstats <- checklink("mu.link", "Binomial", substitute(mu.link), c("logit", "probit", "cloglog", "log", "own")) 
         
         in version 1.0-1. Given that the parameter has \code{own} as an option the user needs also to define the following 
         four new functions in order to used an \code{own} link.  
         
          i) own.linkfun 
          
          ii) own.linkinv
          
          iii) own.mu.eta and 
          
          iv) own.valideta.
          
          An example is given below.  
          
          Only one parameter of the distribution at a time is allowed to have its \code{own} link,
          (unless the same four \code{own} functions above are suitable for more that one parameter of the distribution).
          
          
          Note that from \pkg{gamlss} version 1.9-0 the user can introduce its own link function by define an appropriate function, (see the example below).  
          }
\value{
     For the \code{make.link.gamlss} a list with components 

 linkfun: Link function \code{function(parameter)}

 linkinv: Inverse link function \code{function(eta)}

  mu.eta: Derivative \code{function(eta)} dparameter/deta

valideta: \code{function(eta)} TRUE if all of eta is in the domain of \code{linkinv}.

 For the \code{show.link} a list with components the available links for the distribution parameters
}
\references{ 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos and Bob Rigby }
\note{For the links  involving parameters as in \code{logshifted} and \code{logitshifted} the parameters 
       can be passed in the definition of the distribution by calling the  \code{ checklink} function, for example in the definition of 
       the \code{tau} parameter in BCPE distribution the following call is made: 
       \code{tstats <- checklink(  "tau.link", "Box Cox Power Exponential", substitute(tau.link),   
                           c("logshifted", "log", "identity"), par.link = c(1))  }}


\seealso{ \code{\link{gamlss.family}} }
\examples{
str(make.link.gamlss("logshiftto1"))
l2<-make.link.gamlss("logshiftto1")
l2$linkfun(2) # should close to zero (Note that 0.00001 is added)
l2$linkfun(1-0.00001) # should be -Inf but it is large negative
#---------------------------------------------------------
# now use the own link function
# first if the distribution allows you
show.link(BI)
# seems OK now define the four own functions
# First try the probit link using the own link function
# 1: the linkfun function
own.linkfun <- function(mu) { qNO(p=mu)}
# 2: the inverse link function 
own.linkinv <- function(eta) { 
              thresh <- -qNO(.Machine$double.eps)
               eta <- pmin(thresh, pmax(eta, -thresh))
              pNO(eta)}
# 3: the dmu/deta function 
own.mu.eta <- function(eta) pmax(dNO(eta), .Machine$double.eps)
# 4: the valideta function 
own.valideta <- function(eta) TRUE

## bring the data
# library(gamlss) 
#data(aep)
# fitting the model using "own"   
# h1<-gamlss(y~ward+loglos+year, family=BI(mu.link="own"), data=aep)  
# model h1 should be identical to the probit 
# h2<-gamlss(y~ward+loglos+year, family=BI(mu.link="probit"), data=aep)
# now using a function instead of "own" 
probittest <- function()
{
linkfun <- function(mu) { qNO(p=mu)}
linkinv <- function(eta) 
            { 
              thresh <- -qNO(.Machine$double.eps)
               eta <- pmin(thresh, pmax(eta, -thresh))
              pNO(eta)
            }
mu.eta <- function(eta) pmax(dNO(eta), .Machine$double.eps) 
valideta <- function(eta) TRUE
link <- "probitTest"
structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
        valideta = valideta, name = link), class = "link-gamlss")
}
# h3<-gamlss(y~ward+loglos+year, family=BI(mu.link=probittest()), data=aep)  
# Second try the complementary log-log 
# using the Gumbel distribution  
own.linkfun <- function(mu) { qGU(p=mu)} 
own.linkinv <- function(eta) { 
              thresh <- -qGU(.Machine$double.eps)
               eta <- pmin(thresh, pmax(eta, -thresh))
              pGU(eta)} 
own.mu.eta <- function(eta) pmax(dGU(eta), .Machine$double.eps)
own.valideta <- function(eta) TRUE
# h1 and h2 should be identical to cloglog
# h1<-gamlss(y~ward+loglos+year, family=BI(mu.link="own"), data=aep)  
# h2<-gamlss(y~ward+loglos+year, family=BI(mu.link="cloglog"), data=aep)
# note that the Gumbel distribution is negatively skew
# for a positively skew link function we can used the Reverse Gumbel 
revloglog  <- function()
{
linkfun <- function(mu) { qRG(p=mu)} 
linkinv <- function(eta) { 
              thresh <- -qRG(.Machine$double.eps)
               eta <- pmin(thresh, pmax(eta, -thresh))
              pRG(eta)}
mu.eta <- function(eta) pmax(dRG(eta), .Machine$double.eps)
valideta <- function(eta) TRUE
link <- "revloglog"
structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
        valideta = valideta, name = link), class = "link-gamlss")
}
# h1<-gamlss(y~ward+loglos+year, family=BI(mu.link=revloglog()), data=aep)  
# a considerable improvement in the deviance
# try a shifted logit link function from -1, 1 
own.linkfun <- function(mu)
             { shift = c(-1,1)           
               log((mu-shift[1])/(shift[2]-mu))
             }
own.linkinv <- function(eta) 
            {
            shift = c(-1,1)  
            thresh <- -log(.Machine$double.eps)
               eta <- pmin(thresh, pmax(eta, -thresh))
                      shift[2]-(shift[2]-shift[1])/(1 + exp(eta))
            } 
own.mu.eta <- function(eta) 
            {
        shift = c(-1,1)  
            thresh <- -log(.Machine$double.eps)
               res <- rep(.Machine$double.eps, length(eta))
            res[abs(eta) < thresh] <- ((shift[2]-shift[1])*exp(eta)/(1 + exp(eta))^2)[abs(eta) < thresh]
            res
            }
own.valideta <- function(eta) TRUE       
#----------
str(make.link.gamlss("own"))
l2<-make.link.gamlss("own")
l2$linkfun(0) # should be zero
l2$linkfun(1) # should be Inf
l2$linkinv(-5:5)
}
\keyword{regression}% 
