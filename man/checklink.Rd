\name{checklink}
\alias{checklink}

\title{Set the Right Link Function for Specified Parameter and Distribution}
\description{
 This function is used within the distribution family specification of a GAMLSS model to define the right link for each of the parameters 
 of the distribution. 
 This function should not be called by the user unless he/she specify a new distribution family or wishes to change existing link 
 functions in the parameters.  
}
\usage{
checklink(which.link = NULL, which.dist = NULL, link = NULL, link.List = NULL)
}

\arguments{
  \item{which.link}{which parameter link e.g. \code{which.link="mu.link"} }
  \item{which.dist}{which distribution family e.g. \code{which.dist="Cole.Green"} }
  \item{link}{a repetition of \code{which.link} e.g. \code{link=substitute(mu.link)} }
  \item{link.List}{what link function are required e.g. \code{link.List=c("inverse", "log", "identity")} }
}

\value{
Defines the right link for each parameter
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  
 
Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}. 
 }
\author{Calliope Akantziliotou}

\seealso{ \code{\link{gamlss.family}} }

\keyword{regression}% 
