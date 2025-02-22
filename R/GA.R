# MS Saturday, April 6, 2002 at 14:43
# last change MS Tuesday, September 9, 2003 at 10:21
# Gamma distribution with E(y)=mu Var(y)=sigma^2*mu^2
GA <-function (mu.link ="log", sigma.link="log") 
{
    mstats <- checklink("mu.link", "Gamma", substitute(mu.link), c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Gamma", substitute(sigma.link),  c("inverse", "log", "identity", "own"))
    
    structure(
          list(family = c("GA", "Gamma"),
           parameters = list(mu=TRUE,sigma=TRUE), 
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
                 dldm = function(y,mu,sigma) (y-mu)/((sigma^2)*(mu^2)),
               d2ldm2 = function(mu,sigma) -1/((sigma^2)*(mu^2)), 
                 dldd = function(y,mu,sigma)  (2/sigma^3)*((y/mu)-log(y)+log(mu)+log(sigma^2)-1+digamma(1/(sigma^2))),
               d2ldd2 = function(sigma) (4/sigma^4)-(4/sigma^6)*trigamma((1/sigma^2)), 
              d2ldmdd = function(y)  rep(0,length(y)),
          G.dev.incr  = function(y,mu,sigma,w,...) -2*dGA(y,mu,sigma,log=TRUE),                                        
                rqres = expression( rqres(pfun="pGA", type="Continuous", y=y, mu=mu, sigma=sigma)), 
           mu.initial = expression({mu <- (y+mean(y))/2}),
        sigma.initial = expression({sigma <- rep(1,length(y))}) ,
             mu.valid = function(mu) all(mu > 0) , 
          sigma.valid = function(sigma)  all(sigma > 0), 
              y.valid = function(y)  all(y > 0),
                 mean = function(mu, sigma) mu,
             variance = function(mu, sigma) sigma^2 * mu^2
          ),
                class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dGA<-function(x, mu=1, sigma=1, log=FALSE)
 { 
          if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
          if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    #      if (any(x < 0))  stop(paste("x must be positive", "\n", ""))  
       n <- max(length(x), length(mu), length(sigma))
       x <- rep_len(x, n)
      mu <- rep_len(mu, n)
   sigma <- rep_len(sigma, n)
 log.lik <- (1/sigma^2)*log(x/(mu*sigma^2))-x/(mu*sigma^2)-log(x)-lgamma(1/sigma^2)
     if(log==FALSE) fy  <- exp(log.lik) else fy <- log.lik
       fy[x <= 0] <- 0
       fy[x == Inf] <- 0
       fy 
  }
################################################################################
################################################################################
################################################################################
################################################################################
pGA <- function(q, mu=1, sigma=1, lower.tail = TRUE, log.p = FALSE)
  {     
          if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
          if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
      #    if (any(q < 0))  stop(paste("y must be positive", "\n", ""))  
      n <- max(length(q), length(mu), length(sigma))
      q <- rep_len(q, n)
     mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)
    cdf <- pgamma(q,shape=1/sigma^2,scale=mu*sigma^2, lower.tail = lower.tail, log.p = log.p)
   }
################################################################################
################################################################################
################################################################################
################################################################################
qGA <- function(p, mu=1, sigma=1,  lower.tail = TRUE, log.p = FALSE)
  { 
if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
if (log.p==TRUE) p <- exp(p) 
      n <- max(length(p), length(mu), length(sigma))
      p <- rep_len(p, n)
     mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)  
      q <- qgamma(p,shape=1/sigma^2,scale=mu*sigma^2, lower.tail = lower.tail)
      q[p == 0] <- 0
      q[p == 1] <- Inf
      q[p <  0] <- NaN
      q[p >  1] <- NaN
      return(q)
   }
################################################################################
################################################################################
################################################################################
################################################################################
rGA <- function(n, mu=1, sigma=1)
  { if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
    n <- ceiling(n)
    p <- runif(n)
    r <- qGA(p,mu=mu,sigma=sigma)
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################
