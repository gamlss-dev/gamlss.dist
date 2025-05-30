################################################################################
################################################################################
################################################################################
################################################################################
# MS Saturday, April 6, 2002 at 18:38
GU <-function (mu.link ="identity", sigma.link="log") 
{
    mstats <- checklink(   "mu.link", "Gumbel", substitute(mu.link), c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Gumbel", substitute(sigma.link), c("inverse", "log", "identity", "own"))        
    structure(
          list(family = c("GU", "Gumbel"),
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
                 dldm = function(y, mu,sigma) (exp((y-mu)/sigma)-1)/sigma,
               d2ldm2 = function(sigma)  -1/sigma^2 ,
                 dldd = function(y,mu,sigma) -(1/sigma)+  ((y-mu)/sigma^2)*(exp((y-mu)/sigma)-1),  
               d2ldd2 = function(sigma) -1.82368/sigma^2,
              d2ldmdd = function(sigma)  -0.422784/sigma^2,
          G.dev.incr  = function(y,mu,sigma,...)-2*dGU(y,mu,sigma,log=TRUE), 
                rqres = expression(rqres(pfun="pGU", type="Continuous", y=y, mu=mu, sigma=sigma)),
            mu.initial = expression(mu <- (y+mean(y))/2 ),
         sigma.initial = expression(sigma <-  rep((sqrt(6)*sd(y))/pi,length(y))), 
              mu.valid = function(mu) TRUE , 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  TRUE,
                  mean = function(mu, sigma) mu-0.5772157*sigma,
              variance = function(mu, sigma) 1.64493*sigma^2
          ),
            class = c("gamlss.family","family"))
}

################################################################################
################################################################################
################################################################################
################################################################################
dGU<-function(x, mu=0, sigma=1, log=FALSE)
 {  
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
        ly <- max(length(x),length(mu),length(sigma)) 
         x <- rep(x, length = ly) 
      sigma <- rep(sigma, length = ly) 
         mu <- rep(mu, length = ly) 
    log.lik <- -log(sigma)+((x-mu)/sigma)-exp((x-mu)/sigma)
if(log==FALSE) fy  <- exp(log.lik) else fy <- log.lik
    fy[x<=0] <- fy[ x == Inf ] <-  0
    fy 
}
################################################################################
################################################################################
################################################################################
################################################################################ 
pGU <- function(q, mu=0, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
      ly <- max(length(q),length(mu),length(sigma)) 
       q <- rep(q, length = ly) 
   sigma <- rep(sigma, length = ly) 
     mu <- rep(mu, length = ly) 
    cdf <- 1-exp(-exp((q-mu)/sigma))
if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
    cdf[q>=-Inf] <- 0
    cdf[ q == Inf ] <-  1
    cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qGU <- function(p, mu=0, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { 
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
if (log.p==TRUE) p <- exp(p) 
if (lower.tail==FALSE) p <- 1-p
      ly <- max(length(p),length(mu),length(sigma)) 
      p <- rep(p, length = ly) 
  sigma <- rep(sigma, length = ly) 
     mu <- rep(mu, length = ly) 
      q <- mu+sigma*log(-log(1-p))
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
rGU <- function(n, mu=0, sigma=1)
  { if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))   
    n <- ceiling(n)
    p <- runif(n)
    r <- qGU(p,mu=mu,sigma=sigma)
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################
