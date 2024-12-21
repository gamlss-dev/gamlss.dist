# Bob Rigby and Mikis Stasinopoulos
# created April 2017
# beta negative binomial
################################################################################
################################################################################
################################################################################
################################################################################
BNB <-function (mu.link ="log", sigma.link="log", nu.link="log") 
{
     mstats <- checklink("mu.link", "BNB", substitute(mu.link), 
                         c("1/mu^2", "log", "identity"))
    dstats <- checklink("sigma.link", "BNB", substitute(sigma.link), 
                         c("inverse", "log", "identity"))
    vstats <- checklink("nu.link", "BNB",substitute(nu.link), 
                         c("1/nu^2", "log", "identity"))    
    structure(
          list(family = c("BNB", "Beta Negative Binomial"),
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
                 dldm = function(y,mu,sigma,nu) #---------------------------
                           {
                     m <- (1/sigma) + 1
                     n <- (mu*nu)/sigma
                     k <- 1/nu
                  dldm <- (nu/sigma)*digamma(y+n) +  (nu/sigma)*digamma(n+m)-
                          (nu/sigma)*digamma(n) -  (nu/sigma)*digamma(y+n+m+k)
                       dldm}, 
               d2ldm2 = function(y,mu,sigma,nu) #--------------------------
                           {
                     m <- (1/sigma) + 1
                     n <- (mu*nu)/sigma
                     k <- 1/nu
                  dldm <- (nu/sigma)*digamma(y+n) +  (nu/sigma)*digamma(n+m)-
                          (nu/sigma)*digamma(n) -  (nu/sigma)*digamma(y+n+m+k)
                d2ldm2 <- - dldm * dldm
                d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)  
                d2ldm2
                           },
                 dldd = function(y,mu,sigma,nu) #------------------------------
                           {
                    m <- (1/sigma) + 1
                    n <- (mu*nu)/sigma
                    k <- 1/nu
                 dldd <- -(mu*nu/sigma^2)*digamma(y+n) - (1/sigma^2)*digamma(m+k) -
                   (1/sigma^2)*(mu*nu+1)*digamma(n+m)+(mu*nu/sigma^2)*digamma(n)+
                   (1/sigma^2)*digamma(m)+(1/sigma^2)*(mu*nu+1)*digamma(y+n+m+k)
                      dldd},
               d2ldd2 = function(y,mu,sigma,nu)#--------------------------------
                            {
                    m <- (1/sigma) + 1
                    n <- (mu*nu)/sigma
                    k <- 1/nu
                 dldd <- -(mu*nu/sigma^2)*digamma(y+n) - (1/sigma^2)*digamma(m+k) -
                      (1/sigma^2)*(mu*nu+1)*digamma(n+m)+(mu*nu/sigma^2)*digamma(n)+
                      (1/sigma^2)*digamma(m)+(1/sigma^2)*(mu*nu+1)*digamma(y+n+m+k)
                d2ldd2 <- -dldd*dldd
                d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
                d2ldd2},
                 dldv = function(y,mu,sigma,nu)# -------------------------------
                   {                           
                    m <- (1/sigma) + 1
                    n <- (mu*nu)/sigma
                    k <- 1/nu
                 dldv <- -(1/nu^2)*digamma(y+k) + (mu/sigma)*digamma(y+n) -
                         (1/nu^2)*digamma(m+k)+(mu/sigma)*digamma(n+m)+
                         (1/nu^2)*digamma(k)-(mu/sigma)*digamma(n) -
                         ((mu/sigma)-(1/nu^2))*digamma(y+n+m+k)
                 dldv},
        d2ldv2 = function(y,mu,sigma,nu)#------------------------------------
                   {
                    m <- (1/sigma) + 1
                    n <- (mu*nu)/sigma
                    k <- 1/nu
                 dldv <- -(1/nu^2)*digamma(y+k) + (mu/sigma)*digamma(y+n) -
                      (1/nu^2)*digamma(m+k)+(mu/sigma)*digamma(n+m)+
                      (1/nu^2)*digamma(k)-(mu/sigma)*digamma(n) -
                      ((mu/sigma)-(1/nu^2))*digamma(y+n+m+k)
               d2ldv2 <- -dldv*dldv
               d2ldv2 <- ifelse(d2ldv2 < -1e-15, d2ldv2,-1e-15)  
                    d2ldv2},
        d2ldmdd = function(y,mu,sigma,nu) #---------------------------------
                    {
                   m <- (1/sigma) + 1
                   n <- (mu*nu)/sigma
                   k <- 1/nu
                dldm <- (nu/sigma)*digamma(y+n) +  (nu/sigma)*digamma(n+m)-
                        (nu/sigma)*digamma(n) -  (nu/sigma)*digamma(y+n+m+k) 
                dldd <- -(mu*nu/sigma^2)*digamma(y+n) - (1/sigma^2)*digamma(m+k) -
                         (1/sigma^2)*(mu*nu+1)*digamma(n+m)+(mu*nu/sigma^2)*digamma(n)+
                         (1/sigma^2)*digamma(m)+(1/sigma^2)*(mu*nu+1)*digamma(y+n+m+k)
                    d2ldmdd <- -dldm *dldd
                    d2ldmdd}, 
        d2ldmdv = function(y,mu,sigma,nu) 
                     {
                m <- (1/sigma) + 1
                n <- (mu*nu)/sigma
                k <- 1/nu
             dldm <- (nu/sigma)*digamma(y+n) +  (nu/sigma)*digamma(n+m)-
                      (nu/sigma)*digamma(n) -  (nu/sigma)*digamma(y+n+m+k) 
             dldv <- -(1/nu^2)*digamma(y+k) + (mu/sigma)*digamma(y+n) -
                     (1/nu^2)*digamma(m+k)+(mu/sigma)*digamma(n+m)+
                     (1/nu^2)*digamma(k)-(mu/sigma)*digamma(n) -
                      ((mu/sigma)-(1/nu^2))*digamma(y+n+m+k)
          d2ldmdv <- -dldm *dldv
          d2ldmdv}, 
          d2ldddv = function(y,mu,sigma,nu)#------------------------------- 
                    {
               m <- (1/sigma) + 1
               n <- (mu*nu)/sigma
               k <- 1/nu
            dldd <- -(mu*nu/sigma^2)*digamma(y+n) - (1/sigma^2)*digamma(m+k) -
                    (1/sigma^2)*(mu*nu+1)*digamma(n+m)+(mu*nu/sigma^2)*digamma(n)+
                    (1/sigma^2)*digamma(m)+(1/sigma^2)*(mu*nu+1)*digamma(y+n+m+k)  
            dldv <- -(1/nu^2)*digamma(y+k) + (mu/sigma)*digamma(y+n) -
                     (1/nu^2)*digamma(m+k)+(mu/sigma)*digamma(n+m)+
                      (1/nu^2)*digamma(k)-(mu/sigma)*digamma(n) -
                      ((mu/sigma)-(1/nu^2))*digamma(y+n+m+k)     
              d2ldddv <- -dldd *dldv
              d2ldddv
                                 },               
                
          G.dev.incr  = function(y,mu,sigma,nu,...) -2*dBNB(y, mu, sigma, nu, log=TRUE),
                rqres = expression(    
                        rqres(pfun="pBNB", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma, nu=nu)
                                  ), 
            mu.initial = expression(mu<- (y+mean(y))/2 ),
         sigma.initial = expression(
                      sigma <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))),
            nu.initial = expression({  nu <- rep(.1,length(y)) }), 
              mu.valid = function(mu) all(mu > 0) , 
           sigma.valid = function(sigma)  all(sigma > 0), 
              nu.valid = function(nu) TRUE,  
               y.valid = function(y)  all(y >= 0),
                  mean = function(mu, sigma, nu) mu,
              variance = function(mu, sigma, nu) ifelse(sigma < 1, mu * (1 + mu * nu) * (1 + sigma / nu) / (1 - sigma), Inf)
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dBNB<-function(x, mu=1, sigma=1, nu=1, log=FALSE)
  { 
   if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
   if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
   if (any(nu <= 0) )  stop(paste("nu must be greater than 0 ", "\n", "")) 
  # if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))  
    ly <- max(length(x),length(mu),length(sigma),length(nu)) 
     x <- rep(x, length = ly)      
 sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)   
    nu <- rep(nu, length = ly) 
    m <- (1/sigma) + 1
    n <- (mu*nu)/sigma
    k <- 1/nu
  logL <- lbeta(x+n, m+k)-lbeta(n,m)-lgamma(x+1)-lgamma(k)+lgamma(x+k)
  lik <- if (log) logL else exp(logL)
  lik[x < 0] <- 0
  lik[x == Inf] <- 0
  return(lik)
  }
################################################################################
################################################################################
################################################################################
################################################################################
pBNB <- function(q, mu = 1, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE)
{     
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0) )  stop(paste("nu must be greater than 0 ", "\n", "")) 
 # if (any(q < 0) )  stop(paste("q must be >=0", "\n", ""))
     ly <- max(length(q),length(mu),length(sigma),length(nu)) 
     qq <- rep(q, length = ly)      
  sigma <- rep(sigma, length = ly)
     mu <- rep(mu, length = ly)   
     nu <- rep(nu, length = ly) 
  qq[q==Inf] <- 1
     fn <- function(q, mu, sigma, nu) sum(dBNB(0:qq, mu=mu, sigma=sigma, nu=nu))
   Vcdf <- Vectorize(fn)
    cdf <- Vcdf(q=qq, mu=mu, sigma=sigma, nu=nu)
    cdf <- if(lower.tail==TRUE) cdf else 1-cdf
    cdf <- if(log.p==FALSE) cdf else log(cdf)    
cdf[q < 0] <- 0 
cdf[q == Inf] <- 1     
    return(cdf)
}
################################################################################
################################################################################
################################################################################
################################################################################
qBNB <- function(p, mu=1, sigma=1, nu=1,  lower.tail = TRUE, log.p = FALSE,  
                 max.value = 10000)
  {      
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 

if (log.p==TRUE) p <- exp(p) else p <- p
if (lower.tail==TRUE) p <- p else p <- 1-p  
           ly <- max(length(p),length(mu),length(sigma),length(nu)) 
            p <- rep(p, length = ly)                                                         
          QQQ <- rep(0,length = ly)                         
       nsigma <- rep(sigma, length = ly)
          nmu <- rep(mu, length = ly)                
          nnu <- rep(nu, length = ly)    
for (i in seq(along=p))                                                          
      {
      cumpro <- 0                                                                         
     if (p[i]+0.000000001 >= 1) QQQ[i] <- Inf
     else  
        {  
            for (j in seq(from = 0, to = max.value))
            {
            cumpro <-  pBNB(j, mu = nmu[i], sigma = nsigma[i], nu = nnu[i], log.p = FALSE) 
                       # else  cumpro+dBNB(j, mu = nmu[i], sigma = nsigma[i], nu = nnu[i], log = FALSE)# the above is faster 
           QQQ[i] <- j 
       if  (p[i] <= cumpro ) break 
            } 
        }
}  
          QQQ[p == 0] <- 0
          QQQ[p == 1] <- Inf
          QQQ[p <  0] <- NaN
          QQQ[p >  1] <- NaN
          return(QQQ)
   }
################################################################################
################################################################################
################################################################################
################################################################################
rBNB <- function(n, mu=1, sigma=1, nu=1, max.value = 10000)
  { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qBNB(p, mu=mu, sigma=sigma, nu=nu, max.value = max.value )
          return(as.integer(r))
  }
################################################################################
################################################################################
################################################################################
################################################################################
