# MS
################################################################################
################################################################################
################################################################################
################################################################################ 
# zero altered PIG (with probability y=0 is nu) 01/03/10
################################################################################
################################################################################
################################################################################
################################################################################ 
ZAPIG = function (mu.link = "log", sigma.link = "log", nu.link = "logit") 
{
  mstats <- checklink("mu.link", "ZAPIG", substitute(mu.link), 
                      c("inverse", "log", "identity"))
  dstats <- checklink("sigma.link", "ZAPIG", substitute(sigma.link), 
                      c("inverse", "log", "identity"))
  vstats <- checklink("nu.link", "ZAPIG", substitute(nu.link), 
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))
  
  structure(list(family = c("ZAPIG", "Zero altered Poisson inv. Gaussian"),
                 parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE),
                 nopar = 3,
                 type = "Discrete",
                 mu.link = as.character(substitute(mu.link)),
                 sigma.link = as.character(substitute(sigma.link)), 
                 nu.link = as.character(substitute(nu.link)),
                 mu.linkfun = mstats$linkfun, 
                 sigma.linkfun = dstats$linkfun,
                 nu.linkfun = vstats$linkfun, 
                 mu.linkinv = mstats$linkinv,
                 sigma.linkinv = dstats$linkinv, 
                 nu.linkinv = vstats$linkinv,
                 mu.dr = mstats$mu.eta,
                 sigma.dr = dstats$mu.eta, 
                 nu.dr = vstats$mu.eta,
                 dldm = function(y,mu,sigma,nu) 
                 {
                   dldm0 <- PIG()$dldm(y,mu,sigma) + dPIG(0,mu,sigma)*PIG()$dldm(0,mu,sigma)/(1-dPIG(0,mu,sigma))
                   dldm <- ifelse(y==0, 0 , dldm0)
                   dldm
                 }, 
                 d2ldm2 = function(y,mu,sigma,nu) {
                   dldm0 <- PIG()$dldm(y,mu,sigma) + dPIG(0,mu,sigma)*PIG()$dldm(0,mu,sigma)/(1-dPIG(0,mu,sigma))
                   dldm <- ifelse(y==0, 0 , dldm0)
                   d2ldm2 <- -dldm*dldm
                   d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)    
                   d2ldm2},
                 dldd = function(y,mu,sigma,nu) 
                 {
                   sigma <- ifelse(sigma<0.000001, 0.000001, sigma)
                   #  dldd0 <- NBI()$dldd(y,mu,sigma) + dNBI(0,mu,sigma)*NBI()$dldd(0,mu,sigma)/(1-dNBI(0,mu,sigma))
                   dldd0 <- PIG()$dldd(y,mu,sigma) + dPIG(0,mu,sigma)*PIG()$dldd(0,mu,sigma)/(1-dPIG(0,mu,sigma))
                   dldd <- ifelse(y==0, 0, dldd0)
                   dldd
                 }, 
                 d2ldd2 = function(y,mu,sigma,nu) 
                 {
                   sigma <- ifelse(sigma<0.000001, 0.000001, sigma )
                   #  dldd0 <- NBI()$dldd(y,mu,sigma) + dNBI(0,mu,sigma)*NBI()$dldd(0,mu,sigma)/(1-dNBI(0,mu,sigma))
                   dldd0 <- PIG()$dldd(y,mu,sigma) + dPIG(0,mu,sigma)*PIG()$dldd(0,mu,sigma)/(1-dPIG(0,mu,sigma))
                   dldd <- ifelse(y==0, 0 , dldd0)
                   d2ldd2 <- -dldd*dldd  
                   d2ldd2 <- ifelse(d2ldd2 < -1e-10, d2ldd2,-1e-10) 
                   d2ldd2
                 }, 
                 dldv = function(y,mu,sigma,nu) {
                   dldv <- ifelse(y==0, 1/nu, -1/(1-nu))
                   dldv}, 
                 d2ldv2 = function(y,mu,sigma,nu) {
                   d2ldv2 <- -1/(nu*(1-nu))
                   d2ldv2 <- ifelse(d2ldv2 < -1e-15, d2ldv2,-1e-15)  
                   d2ldv2},
                 d2ldmdd = function(y,mu,sigma,nu) {
                   sigma <- ifelse(sigma<0.000001, 0.000001, sigma )
                   dldm0 <- PIG()$dldm(y,mu,sigma) + dPIG(0,mu,sigma)*PIG()$dldm(0,mu,sigma)/(1-dPIG(0,mu,sigma))
                   dldm <- ifelse(y==0, 0 , dldm0)
                   dldd0 <- PIG()$dldd(y,mu,sigma) + dPIG(0,mu,sigma)*PIG()$dldd(0,mu,sigma)/(1-dPIG(0,mu,sigma))
                   dldd <- ifelse(y==0, 0 , dldd0)
                   d2ldm2<--dldm*dldd
                   d2ldm2}, 
                 d2ldmdv = function(y) 
                 {
                   d2ldmdv=0  
                   d2ldmdv
                 },
                 d2ldddv = function(y) 
                 {
                   d2ldddv=0
                   d2ldddv
                 },        
                 G.dev.incr  = function(y,mu,sigma,nu,...) -2*dZAPIG(y, mu = mu, sigma = sigma, nu=nu, log = TRUE), 
                 rqres = expression(
                   rqres(pfun="pZAPIG", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma, nu=nu)
                 ), 
                 mu.initial = expression(mu <- (y + mean(y))/2),           
                 ##           mu.initial = expression(mu <-  y+0.5),
                 sigma.initial =  expression(
                   sigma <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))),
                 nu.initial = expression(nu <- rep(0.1, length(y))),
                 mu.valid = function(mu) all(mu > 0) , 
                 sigma.valid = function(sigma)  all(sigma > 0), 
                 nu.valid = function(nu) all(nu > 0 & nu < 1),           
                 y.valid = function(y)  all(y >= 0),
                 mean = function(mu, sigma, nu) 
                 {
                   alpha2 <- 1 / sigma^2 + 2* mu / sigma  # see page 94 in Using GAMLSS in R by Rigby et al.
                   alpha  <- sqrt(alpha2)
                   c     <- (1 - nu) / (1 - exp(1 / sigma - alpha ))
                   return(mu * c)
                 },
                 variance = function(mu, sigma, nu) 
                 {
                   alpha2 <- 1 / sigma^2 + 2* mu / sigma  # see page 94 in Using GAMLSS in R by Rigby et al.
                   alpha  <- sqrt(alpha2)
                   c      <- (1 - nu) / (1 - exp(1 / sigma - alpha ))
                   return(mu * c + c * mu^2 * (1 + sigma - c))
                 }
  ),
  class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZAPIG<-function(x, mu = 1, sigma = 1, nu = 0.3, log = FALSE)
{ 
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
if (any(nu <= 0)|any(nu >= 1))  stop(paste("nu must be between 0 and 1 ", "\n", ""))
         ly <- max(length(x),length(mu),length(sigma),length(nu)) 
          x <- xx <- rep(x, length = ly)
    xx[x<0] <- 0
 xx[x>=Inf] <- 0
      sigma <- rep(sigma, length = ly)
         mu <- rep(mu, length = ly)   
         nu <- rep(nu, length = ly) 
        fy0 <- dPIG(0, mu = mu, sigma=sigma, log = T)
         fy <- dPIG(x, mu = mu, sigma=sigma, log = T) 
      logfy <- rep(0, length(x))
logfy[x!=0] <- log(1-nu) + fy - log(1-exp(fy0))
logfy[x==0] <- log(nu)
 # logfy <- ifelse((x==0), log(nu), log(1-nu) + fy - log(1-exp(fy0))) 
if(log == FALSE) fy2 <- exp(logfy) else fy2 <- logfy
fy2[x<0] <- 0
fy2[x>=Inf] <- 0
#  fy2 <- ifelse(x < 0, 0, fy2) 
  fy2
}
################################################################################
################################################################################
################################################################################
################################################################################
pZAPIG <- function(q, mu = 1, sigma = 1,  nu = 0.3, lower.tail = TRUE, log.p = FALSE)
{     
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0)|any(nu >= 1))  #In this parametrization  nu = alpha
    stop(paste("nu must be between 0 and 1 ", "\n", ""))
#  if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))
  cdf0 <- pPIG(0, mu = mu, sigma=sigma)
  cdf1 <- pPIG(q, mu = mu, sigma=sigma)                   
  cdf3 <- nu+((1-nu)*(cdf1-cdf0)/(1-cdf0))
  cdf <- ifelse((q==0),nu,  cdf3)
  if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
  if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf)  
  cdf <-ifelse(q < 0, 0, cdf) 
  cdf
}
################################################################################
################################################################################
################################################################################
################################################################################
qZAPIG <- function(p, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, 
                   log.p = FALSE, max.value = 10000)
{      
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
  if (log.p == TRUE) p <- exp(p)   else p <- p
  if (lower.tail == TRUE)  p <- p  else p <- 1 - p
  pnew <- (p-nu)/(1-nu)-1e-10
  cdf0 <- pPIG(0, mu = mu, sigma=sigma )                   
  pnew2 <- cdf0*(1-pnew) + pnew           
  pnew2 <- ifelse((pnew2 > 0 ),pnew2, 0)
  q <- qPIG(pnew2, mu = mu, sigma=sigma, max.value = max.value)                   
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
rZAPIG <- function(n, mu = 1, sigma = 1, nu = 0.3, max.value = 10000)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0)|any(nu >= 1))  #In this parametrization  nu = alpha
    stop(paste("nu must be between 0 and 1 ", "\n", ""))
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qZAPIG(p, mu=mu, sigma=sigma, nu=nu, max.value = max.value)
  as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################