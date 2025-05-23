################################################################################
################################################################################
################################################################################
################################################################################
RG <-function (mu.link ="identity", sigma.link="log") 
    {
    mstats <- checklink("mu.link", "Reverse Gumbel", substitute(mu.link), c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Reverse Gumbel", substitute(sigma.link), c("inverse", "log", "identity", "own"))
    
    structure(
          list(family = c("RG", "Reverse Gumbel"),
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
                 dldm = function(y,mu,sigma) (1-exp(-((y-mu)/sigma)))/sigma ,
               d2ldm2 = function(sigma) -1/sigma^2,
                 dldd = function(y,mu,sigma) -(1/sigma)*(1-(y-mu)/sigma)-((y-mu)/sigma^2)*exp(-(y-mu)/sigma),
               d2ldd2 = function(sigma) -1.82368/sigma^2,
              d2ldmdd = function(sigma) -0.422784/sigma^2,
          G.dev.incr  = function(y,mu,sigma,...)-2*dRG(y,mu,sigma,log=TRUE),
                rqres = expression(rqres(pfun="pRG", type="Continuous", y=y, mu=mu, sigma=sigma)),
            mu.initial = expression(mu <- (y+mean(y))/2),
         sigma.initial = expression(sigma <-  rep((sqrt(6)*sd(y)/pi),length(y))), 
              mu.valid = function(mu) TRUE , 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  TRUE,
                  mean = function(mu, sigma) mu-digamma(1)*sigma,
              variance = function(mu, sigma) (pi^2*sigma^2)/6
          ),
            class = c("gamlss.family","family"))
   }
################################################################################
################################################################################
################################################################################
################################################################################
dRG<-function(x, mu=0, sigma=1, log=FALSE)
  { 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
      ly <- max(length(x),length(mu),length(sigma)) 
       x <- rep(x, length = ly) 
   sigma <- rep(sigma, length = ly) 
      mu <- rep(mu, length = ly)   
 log.lik <- (-log(sigma) -((x-mu)/sigma)  -exp(-(x-mu)/sigma))
     if(log==FALSE) fy  <- exp(log.lik) else fy <- log.lik
  fy[x==-Inf] <- fy[ x == Inf ] <-  0
      fy 
  }
################################################################################
################################################################################
################################################################################
################################################################################
pRG <- function(q, mu=0, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
     ly <- max(length(q),length(mu),length(sigma)) 
      q <- rep(q, length = ly) 
  sigma <- rep(sigma, length = ly) 
     mu <- rep(mu, length = ly) 
    cdf <- exp(-exp(-(q-mu)/sigma))
    if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
    if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
    cdf[q==-Inf] <- 0
    cdf[ q == Inf ] <-  1
    cdf
  }
################################################################################
################################################################################
################################################################################
################################################################################  
qRG <- function(p, mu=0, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
if (log.p==TRUE) p <- exp(p) else p <- p
if (lower.tail==TRUE) p <- p else p <- 1-p
    ly <- max(length(p),length(mu),length(sigma)) 
    p <- rep(p, length = ly) 
sigma <- rep(sigma, length = ly) 
   mu <- rep(mu, length = ly)   
    q <- mu-sigma*log(-log(p))
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
rRG <- function(n, mu=0, sigma=1)
  { if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
    n <- ceiling(n)
    p <- runif(n)
    r <- qRG(p,mu=mu,sigma=sigma)
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################
