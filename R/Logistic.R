################################################################################
################################################################################
################################################################################
################################################################################
LO <- function (mu.link ="identity", sigma.link="log") 
{
    mstats <- checklink("mu.link", "Logistic", substitute(mu.link), c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Logistic", substitute(sigma.link), c("inverse", "log", "identity", "own"))
    
    structure(
          list(family = c("LO", "Logistic"),
           parameters = list(mu=TRUE, sigma=TRUE), 
                nopar = 2, 
                 type = "Continuous",
              mu.link = as.character(substitute(mu.link)), 
           sigma.link = as.character(substitute(sigma.link)), 
           mu.linkfun = mstats$linkfun, 
        sigma.linkfun = dstats$linkfun, 
           mu.linkinv = mstats$linkinv, 
        sigma.linkinv = dstats$linkinv,
                mu.dr = mstats$mu.eta, 
             sigma.dr = dstats$mu.eta, 
                 dldm = function(y,mu,sigma) (1/sigma)*(exp((y-mu)/sigma)-1)/(1+exp((y-mu)/sigma)),
               d2ldm2 = function(sigma) -1/(3*sigma^2),    
                 dldd = function(y,mu,sigma)  -(1/sigma)-(y-mu)/sigma^2 +2*(((y-mu)/sigma^2)*exp((y-mu)/sigma))/(1+exp((y-mu)/sigma)),
               d2ldd2 = function(sigma) -(1/(3*sigma^2))*(1+(pi^2/3)),
              d2ldmdd = function(y) rep(0,length(y)),
          G.dev.incr  = function(y,mu,sigma,...) -2*dLO(y,mu,sigma,log=TRUE), 
                rqres = expression(rqres(pfun="pLO", type="Continuous", y=y, mu=mu, sigma=sigma)),
            mu.initial = expression(mu <- (y+mean(y))/2), 
         sigma.initial = expression(sigma <- rep( (sqrt(3)*sd(y))/sqrt(pi) ,length(y) )),
              mu.valid = function(mu) TRUE, 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  TRUE,
                  mean = function(mu, sigma) mu,
              variance = function(mu, sigma) (pi^2 * sigma^2) / 3
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dLO<-function(x, mu=0, sigma=1, log=FALSE)
 { 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    ly <- max(length(x),length(mu),length(sigma)) 
     x <- rep(x, length = ly) 
 sigma <- rep(sigma, length = ly) 
    mu <- rep(mu, length = ly) 
    fy <- dlogis(x, location=mu, scale=sigma, log=log)
    fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pLO <- function(q, mu=0, sigma=1, lower.tail = TRUE, log.p = FALSE)
  {     
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
     ly <- max(length(q),length(mu),length(sigma)) 
      q <- rep(q, length = ly) 
  sigma <- rep(sigma, length = ly) 
     mu <- rep(mu, length = ly) 
    cdf <- plogis(q, location=mu, scale=sigma, lower.tail = lower.tail, log.p = log.p)
    cdf
   }
#----------------------------------------------------------------------------------------
qLO <- function(p, mu=0, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
if (log.p==TRUE) p <- exp(p) 
#if (lower.tail==FALSE) p <- 1-p
   ly <- max(length(p),length(mu),length(sigma)) 
    p <- rep(p, length = ly) 
sigma <- rep(sigma, length = ly) 
   mu <- rep(mu, length = ly)     
    q <- qlogis(p, location=mu, scale=sigma, lower.tail = lower.tail )
    q[p == 0] <- -Inf
    q[p == 1] <- Inf
    q[p <  0] <- NaN
    q[p >  1] <- NaN
    return(q)
   }
################################################################################
################################################################################
################################################################################
################################################################################
rLO <- function(n, mu=0, sigma=1)
  { 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    r <- rlogis(n, location=mu, scale=sigma)
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################
