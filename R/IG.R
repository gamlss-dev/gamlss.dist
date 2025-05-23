################################################################################
################################################################################
################################################################################
################################################################################
# amended 27_11_2007
IG <-function (mu.link = "log", sigma.link = "log") 
{   
    mstats <- checklink("mu.link", "Inverse Gaussian", substitute(mu.link), c("1/mu^2", "inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Inverse Gaussian", substitute(sigma.link),  c("inverse", "log", "identity", "own"))
    structure(
          list(family = c("IG", "Inverse Gaussian"),
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
                 dldm = function(y, mu, sigma) (y-mu)/((sigma^2)*(mu^3)),
               d2ldm2 = function(mu,sigma) -1/((mu^3)*(sigma^2)),
                 dldd = function(y,mu,sigma) (-1/sigma) +((y-mu)^2)/(y*(sigma^3)*(mu^2)),
               d2ldd2 = function(sigma) -2/(sigma^2),
              d2ldmdd = function(y)  rep(0,length(y)),
          G.dev.incr  = function(y,mu,sigma,...) 
                               {  -2*dIG(y,mu,sigma,log=TRUE)},
                rqres = expression(rqres(pfun="pIG", type="Continuous", y=y, mu=mu, sigma=sigma)),
            mu.initial = expression( mu <- (y+mean(y))/2) ,
         sigma.initial = expression(sigma <- sd(y)/(mean(y))^1.5 ), 
              mu.valid = function(mu) all(mu > 0), 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  all(y > 0),
                  mean = function(mu, sigma) mu,
              variance = function(mu, sigma) sigma^2 * mu^3
            ),
                 class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################ 
dIG<-function(x, mu = 1, sigma = 1, log=FALSE)
 {  
if (any(mu < 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
        n <- max(length(x), length(mu), length(sigma))
        x <- rep_len(x, n)
       mu <- rep_len(mu, n)
    sigma <- rep_len(sigma, n)
  log.lik <- (-0.5*log(2*pi)-log(sigma)-(3/2)*log(x)-((x-mu)^2)/(2*sigma^2*(mu^2)*x) )
if(log==FALSE) fy  <- exp(log.lik) else fy <- log.lik
      fy <-ifelse(x <= 0, 0, fy)
      fy[x <= 0] <- 0
      fy[x == Inf] <- 0
      fy 
  }
################################################################################
################################################################################
################################################################################
################################################################################ 
pIG <- function(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
  {    #  browser() 
if (any(mu < 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
       n <- max(length(q), length(mu), length(sigma))
       q <- rep_len(q, n)
      mu <- rep_len(mu, n)
   sigma <- rep_len(sigma, n)
    cdf1 <- pnorm(((q/mu)-1)/(sigma*sqrt(q))) 
   lcdf2 <- (2/(mu*sigma^2))+pnorm((-((q/mu)+1))/(sigma*sqrt(q)),log.p=TRUE)
     cdf <- cdf1+ exp(lcdf2)
if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
     cdf[q<=0] <- 0
     cdf[q>=Inf] <- 1
     cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################  
qIG <- function(p, mu=1, sigma=1,  lower.tail = TRUE, log.p = FALSE)
 {
################################################################################  
# local functions
  h1 <- function(q)
  { 
    pIG(q , mu = mu[i], sigma = sigma[i])-pp[i]   
  }
################################################################################  
  h <- function(q)
  { 
    pIG(q , mu = mu[i], sigma = sigma[i])   
  }
################################################################################
if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", ""))    
if (log.p==TRUE) p <- exp(p) 
if (lower.tail==FALSE) p <- 1-p
         lp <-  max(length(p),length(mu),length(sigma))
         pp <- rep(p, length = lp) 
 pp[p == 1] <- 0.5
pp[p == 0]  <- 0.5
      sigma <- rep(sigma, length = lp)
         mu <- rep(mu, length = lp)
          q <- rep(0,lp)      
for (i in  seq(along=pp)) 
{
if (h(mu[i])<pp[i]) 
  { 
  interval <- c(mu[i], mu[i]+sigma[i])
         j <- 2
while (h(interval[2]) < pp[i]) 
  {
interval[2] <- mu[i]+j*sigma[i]
          j <- j+1 
  }
  } 
else  
  {
  interval <-  interval <- c(.Machine$double.xmin, mu[i])
  }
            q[i] <- uniroot(h1, interval)$root
}
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
rIG <- function(n, mu=1, sigma=1, ...)
  { 
  if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
  if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
    n <- ceiling(n)
    p <- runif(n)
    r <- qIG(p,mu=mu,sigma=sigma, ...)
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################