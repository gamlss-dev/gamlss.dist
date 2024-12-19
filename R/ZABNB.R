################################################################################
################################################################################
################################################################################
################################################################################
# created 17_04_2017
################################################################################
################################################################################
################################################################################
################################################################################
ZABNB <-function (mu.link ="log", sigma.link="log", nu.link="log", tau.link = "logit") 
{
  mstats <- checklink("mu.link", "ZABNB", substitute(mu.link), 
                      c("1/mu^2", "log", "identity"))
  dstats <- checklink("sigma.link", "ZABNB", substitute(sigma.link), 
                      c("inverse", "log", "identity"))
  vstats <- checklink("nu.link", "ZABNB",substitute(nu.link), 
                      c("1/nu^2", "log", "identity"))  
  tstats <- checklink("tau.link", "ZABNB", substitute(tau.link), 
                      c("logit", "probit", "cloglog", "log", "own"))
  structure(
    list(family = c("ZABNB", "Zero Adjusted BNB"),
         parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE, tau=TRUE), 
         nopar = 4, 
         type = "Discrete",
         mu.link = as.character(substitute(mu.link)),  
         sigma.link = as.character(substitute(sigma.link)), 
         nu.link = as.character(substitute(nu.link)), 
         tau.link = as.character(substitute(tau.link)), 
         mu.linkfun = mstats$linkfun, 
         sigma.linkfun = dstats$linkfun, 
         nu.linkfun = vstats$linkfun,
         tau.linkfun = tstats$linkfun,  
         mu.linkinv = mstats$linkinv, 
         sigma.linkinv = dstats$linkinv,
         nu.linkinv = vstats$linkinv,
         tau.linkinv = tstats$linkinv, 
         mu.dr = mstats$mu.eta, 
         sigma.dr = dstats$mu.eta, 
         nu.dr = vstats$mu.eta,
         tau.dr = tstats$mu.eta, 
         dldm = function(y,mu,sigma,nu,tau) 
         {
           # dldm0 <- SICHEL()$dldm(y,mu,sigma,nu) + dSICHEL(0, mu,sigma,nu)*SICHEL()$dldm(0,mu,sigma,nu)/(1-dSICHEL(0,mu,sigma,nu))
           dldm0 <-    BNB()$dldm(y,mu,sigma,nu) +  dBNB(0, mu,sigma,nu)* BNB()$dldm(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldm <- ifelse(y==0, 0 , dldm0)
           dldm }, 
         d2ldm2 = function(y,mu,sigma,nu,tau) 
         {
           dldm0 <- BNB()$dldm(y,mu,sigma,nu) + dBNB(0, mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldm <- ifelse(y==0, 0 , dldm0)
           d2ldm2 <- -dldm*dldm
           d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)    
           d2ldm2},
         dldd = function(y,mu,sigma,nu,tau) 
         { 
           sigma <- ifelse(sigma<0.000001, 0.000001, sigma )
           dldd0 <- BNB()$dldd(y,mu,sigma, nu) + dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma, nu)/(1-dBNB(0,mu,sigma, nu))
           dldd <- ifelse(y==0, 0 , dldd0)
           dldd },
         d2ldd2 = function(y,mu,sigma,nu, tau)
         {
           sigma <- ifelse(sigma<0.000001, 0.000001, sigma )
           dldd0 <- BNB()$dldd(y,mu,sigma, nu) + dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma, nu)/(1-dBNB(0,mu,sigma, nu))
           dldd <- ifelse(y==0, 0 , dldd0)
           d2ldd2 <- -dldd*dldd
           d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
           d2ldd2 },
         dldv = function(y,mu,sigma,nu, tau) 
         {
           dldv0 <- BNB()$dldv(y,mu,sigma,nu) + dBNB(0, mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldv <- ifelse(y==0, 0 , dldv0)
           dldv },
         
         d2ldv2 = function(y,mu,sigma,nu, tau){
           dldv0 <- BNB()$dldv(y,mu,sigma,nu) + dBNB(0, mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldv <- ifelse(y==0, 0 , dldv0)
           d2ldv2 <- -dldv*dldv
           d2ldv2 <- ifelse(d2ldv2 < -1e-15, d2ldv2,-1e-15)  
           d2ldv2 },
         dldt = function(y,mu,sigma,nu, tau) 
         {
           dldt <- ifelse(y==0, 1/tau, -1/(1-tau))
           dldt }, 
         d2ldt2 = function(y,mu,sigma,nu, tau){
           d2ldt2 <- -1/(tau*(1-tau))# or -1/((1-tau)^2)
           d2ldt2 <- ifelse(d2ldt2 < -1e-15, d2ldt2,-1e-15)  
           d2ldt2},
         d2ldmdd = function(y,mu,sigma,nu, tau) #1
         {
           dldm0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))             
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           d2ldmdd <- -dldm *dldd
           d2ldmdd <- ifelse( d2ldmdd < -1e-15,  d2ldmdd,-1e-15)  
           d2ldmdd }, 
         d2ldmdv = function(y, mu, sigma, nu, tau) # 2
         {
           dldm0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))          
           dldv0 <- BNB()$dldv(y,mu,sigma,nu) + dBNB(0, mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldv <- ifelse(y==0, 0 , dldv0)
           d2ldmdv <- -dldm *dldv
           d2ldmdv <- ifelse(d2ldmdv < -1e-15, d2ldmdv,-1e-15)  
           d2ldmdv }, 
         d2ldmdt = function(y, mu, sigma, nu, tau) # 3
         {
           dldm0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))          
           dldt <- ifelse(y==0, 1/tau, -1/(1-tau))
           d2ldmdt <- -dldm *dldt
           d2ldmdt <- ifelse(d2ldmdt < -1e-15, d2ldmdt,-1e-15)  
           d2ldmdt }, 
         d2ldddv = function(y,mu,sigma,nu,tau) # 4
         {
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           dldv0 <- BNB()$dldv(y,mu,sigma,nu) + dBNB(0, mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldv <- ifelse(y==0, 0 , dldv0)
           d2ldddv <- -dldd *dldv
           d2ldddv <- ifelse(d2ldddv < -1e-15, d2ldddv,-1e-15) 
           d2ldddv },               
         d2ldddt = function(y,mu,sigma,nu,tau) # 5
         {
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           dldt <- ifelse(y==0, 1/tau, -1/(1-tau))
           d2ldddt <- -dldd *dldt
           d2ldddt <- ifelse(d2ldddt < -1e-15, d2ldddt,-1e-15) 
           d2ldddt },   
         d2ldvdt = function(y,mu,sigma,nu,tau) # 6
         {
           dldv0 <- BNB()$dldv(y,mu,sigma,nu) + dBNB(0, mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)/(1-dBNB(0,mu,sigma,nu))
           dldv <- ifelse(y==0, 0 , dldv0)
           dldt <- ifelse(y==0, 1/tau, -1/(1-tau))
           d2ldvdt <- -dldv *dldt
           d2ldvdt <- ifelse(d2ldvdt < -1e-15, d2ldvdt,-1e-15) 
           d2ldvdt },      
         G.dev.incr  = function(y,mu,sigma,nu,tau,...) -2*dZABNB(y, mu, sigma, nu, tau, log=TRUE),
         rqres = expression(    
           rqres(pfun="pZABNB", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma, nu=nu)
         ), 
         mu.initial = expression(mu<- (y+mean(y))/2 ),
         sigma.initial = expression(
           sigma <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))),
         nu.initial = expression({  nu <- rep(.1,length(y)) }), 
         tau.initial = expression({  tau <- rep(sum(y==0)/length(y),length(y))}), 
         mu.valid = function(mu) all(mu > 0) , 
         sigma.valid = function(sigma)  all(sigma > 0), 
         nu.valid = function(nu) all(nu > 0), 
         tau.valid = function(tau) all(tau > 0 & tau < 1),  
         y.valid = function(y)  all(y >= 0),
         mean = function(mu, sigma, nu, tau) 
         {
           p0 <- dBNB(0, mu, sigma, nu) 
           c  <- (1 - tau) / (1 - p0)
           return(mu * c)
         },
         variance = function(mu, sigma, nu, tau) 
         {
           p0 <- dBNB(0, mu, sigma, nu) 
           c  <- (1 - tau) / (1 - p0)
           return(ifelse(sigma < 1, c * mu * (1 + mu * nu) * (1 + sigma / nu) / (1 - sigma) + c * (1 - c) * mu^2, Inf))
         }
    ),
    class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZABNB<-function(x, mu=1, sigma=1, nu=1, tau=0.1, log=FALSE)
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(tau <= 0)|any(tau >= 1))  stop(paste("tau must be between 0 and 1 ", "\n", ""))
#  if (any(x < 0) )  stop(paste("x must be >=0", "\n", "")) 
  ly <- max(length(x),length(mu),length(sigma),length(nu),length(tau)) 
  x <- rep(x, length = ly)      
  sigma <- rep(sigma, length = ly)
  mu <- rep(mu, length = ly)   
  nu <- rep(nu, length = ly) 
  tau <- rep(tau, length = ly)
  fy0 <- dBNB(0, mu = mu, sigma=sigma,  nu=nu)
  fy <- dBNB(x, mu = mu, sigma=sigma, nu=nu, log = TRUE)                   
  logfy <- rep(0, length(x))
  logfy <- ifelse((x==0), log(tau), log(1-tau) + fy - log(1-fy0))          
  if(log == FALSE) fy2 <- exp(logfy) else fy2 <- logfy
  fy2[x < 0] <- 0
  fy2[x == Inf] <- 0
  fy2
}
################################################################################
################################################################################
################################################################################
################################################################################
pZABNB <- function(q, mu=1, sigma=1, nu=1, tau=0.1, lower.tail = TRUE, log.p = FALSE)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(tau <= 0)|any(tau >= 1))  #In this parametrization  nu = alpha
    stop(paste("tau must be between 0 and 1 ", "\n", ""))
#  if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))
   ly <- max(length(q),length(mu),length(sigma),length(nu),length(tau)) 
   qq <- rep(q, length = ly)      
sigma <- rep(sigma, length = ly)
   mu <- rep(mu, length = ly)   
   nu <- rep(nu, length = ly) 
   tau <- rep(tau, length = ly)
   qq[q==Inf] <- 1000
  cdf0 <- pBNB(0, mu = mu, sigma=sigma, nu=nu)
  cdf1 <- pBNB(qq, mu = mu, sigma=sigma, nu=nu)                   
  cdf3 <- tau+((1-tau)*(cdf1-cdf0)/(1-cdf0))
  cdf <- ifelse((q==0), tau,  cdf3)
  if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
  if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf)  
  cdf[q < 0] <- 0 
  cdf[q == Inf] <- 1    
  cdf
}
################################################################################
################################################################################
################################################################################
################################################################################
qZABNB <- function(p, mu=1, sigma=1, nu=1, tau=0.1, lower.tail = TRUE, 
                   log.p = FALSE, max.value = 10000)
{     
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
  if (any(tau <= 0)|any(tau >= 1))  #In this parametrization  nu = alpha
    stop(paste("tau must be between 0 and 1 ", "\n", ""))
  if (log.p == TRUE) p <- exp(p)   else p <- p
  if (lower.tail == TRUE)  p <- p  else p <- 1 - p
  pnew <- (p-tau)/(1-tau)-1e-10
  cdf0 <- pBNB(0, mu = mu, sigma=sigma, nu=nu)                   
  pnew2 <- cdf0*(1-pnew) + pnew           
  pnew2 <- ifelse((pnew2 > 0 ),pnew2, 0)
  q <- qBNB(pnew2, mu = mu, sigma=sigma, nu=nu, , max.value = max.value)                
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
rZABNB <- function(n, mu=1, sigma=1, nu=1, tau=0.1, max.value = 10000)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(tau <= 0)|any(tau >= 1))  #In this parametrization  nu = alpha
    stop(paste("tau must be between 0 and 1 ", "\n", ""))
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qZABNB(p, mu=mu, sigma=sigma, nu=nu, tau=tau, , max.value = max.value )
  as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################
