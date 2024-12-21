################################################################################
################################################################################
################################################################################
################################################################################
#  created on 17_04_2017
################################################################################
################################################################################
################################################################################
################################################################################
ZIBNB <-function (mu.link ="log", sigma.link="log", nu.link="log", tau.link = "logit") 
{
  mstats <- checklink("mu.link", "ZIBNB", substitute(mu.link), 
                      c("1/mu^2", "log", "identity"))
  dstats <- checklink("sigma.link", "ZIBNB", substitute(sigma.link), 
                      c("inverse", "log", "identity"))
  vstats <- checklink("nu.link", "ZIBNB",substitute(nu.link), 
                      c("1/nu^2", "log", "identity"))  
  tstats <- checklink("tau.link", "ZIBNB", substitute(tau.link), 
                      c("logit", "probit", "cloglog", "log", "own"))
  structure(
    list(family = c("ZIBNB"),
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
           dldm0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))
           dldm}, 
         d2ldm2 = function(y,mu,sigma,nu,tau) 
         {
           dldm0 <-  (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))
           d2ldm2 <- -dldm*dldm
           d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)    
           d2ldm2},
         dldd = function(y,mu,sigma,nu,tau) 
         { 
           #cat("sigma",sigma[1], "\n")
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           dldd },
         d2ldd2 = function(y,mu,sigma,nu, tau)
         {
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           d2ldd2 <- -dldd*dldd
           d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
           d2ldd2 },
         dldv = function(y,mu,sigma,nu, tau) 
         { 
           dldv0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)
           dldv <- ifelse(y==0, dldv0, BNB()$dldv(y,mu,sigma,nu))
           dldv },
         d2ldv2 = function(y,mu,sigma,nu, tau){
           dldv0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)
           dldv <- ifelse(y==0, dldv0, BNB()$dldv(y,mu,sigma,nu))
           d2ldv2 <- -dldv*dldv
           d2ldv2 <- ifelse(d2ldv2 < -1e-15, d2ldv2,-1e-15)  
           d2ldv2 },
         dldt = function(y,mu,sigma,nu, tau) {
           dldt0 <- ((tau+(1-tau)*dBNB(0,mu,sigma, nu))^(-1))*(1-dBNB(0,mu,sigma, nu))
           dldt <- ifelse(y==0, dldt0, -1/(1-tau))
           dldt }, 
         d2ldt2 = function(y,mu,sigma,nu, tau){
           dldt0 <- ((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*(1-dBNB(0,mu,sigma, nu))
           dldt <- ifelse(y==0, dldt0, -1/(1-tau))      
           d2ldt2 <- -dldt^2
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
           #  cat("mu-sigma", "\n")
           d2ldmdd }, 
         d2ldmdv = function(y, mu, sigma, nu, tau) # 2
         {
           dldm0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))       
           dldv0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)
           dldv <- ifelse(y==0, dldv0, BNB()$dldv(y,mu,sigma,nu))
           d2ldmdv <- -dldm *dldv
           d2ldmdv <- ifelse(d2ldmdv < -1e-15, d2ldmdv,-1e-15)  
           #cat("mu-nu", "\n")
           d2ldmdv }, 
         d2ldmdt = function(y, mu, sigma, nu, tau) # 3
         {
           dldm0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldm(0,mu,sigma,nu)
           dldm <- ifelse(y==0, dldm0, BNB()$dldm(y,mu,sigma,nu))             
           dldt0 <- ((tau+(1-tau)*dBNB(0,mu,sigma, nu))^(-1))*(1-dBNB(0,mu,sigma, nu))
           dldt <- ifelse(y==0, dldt0, -1/(1-tau))
           d2ldmdt <- -dldm *dldt
           d2ldmdt <- ifelse(d2ldmdt < -1e-15, d2ldmdt,-1e-15)  
           #cat("mu-tau", "\n")
           d2ldmdt }, 
         d2ldddv = function(y,mu,sigma,nu,tau) # 4
         {
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           dldv0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)
           dldv <- ifelse(y==0, dldv0, BNB()$dldv(y,mu,sigma,nu))
           d2ldddv <- -dldd *dldv
           d2ldddv <- ifelse(d2ldddv < -1e-15, d2ldddv,-1e-15) 
           #cat("sigma-nu", "\n")
           d2ldddv },               
         d2ldddt = function(y,mu,sigma,nu,tau) # 5
         {
           dldd0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldd(0,mu,sigma,nu)
           dldd <- ifelse(y==0, dldd0, BNB()$dldd(y,mu,sigma,nu))
           dldt0 <- ((tau+(1-tau)*dBNB(0,mu,sigma, nu))^(-1))*(1-dBNB(0,mu,sigma, nu))
           dldt <- ifelse(y==0, dldt0, -1/(1-tau))
           d2ldddt <- -dldd *dldt
           d2ldddt <- ifelse(d2ldddt < -1e-15, d2ldddt,-1e-15) 
           #cat("sigma-tau", "\n")
           d2ldddt },   
         d2ldvdt = function(y,mu,sigma,nu,tau) # 6
         {
           dldv0 <- (1-tau)*((tau+(1-tau)*dBNB(0,mu,sigma,nu))^(-1))*dBNB(0,mu,sigma,nu)*BNB()$dldv(0,mu,sigma,nu)
           dldv <- ifelse(y==0, dldv0, BNB()$dldv(y,mu,sigma,nu))
           dldt0 <- ((tau+(1-tau)*dBNB(0,mu,sigma, nu))^(-1))*(1-dBNB(0,mu,sigma, nu))
           dldt <- ifelse(y==0, dldt0, -1/(1-tau))
           d2ldvdt <- -dldv *dldt
           d2ldvdt <- ifelse(d2ldvdt < -1e-15, d2ldvdt,-1e-15) 
           #cat("nu-tau", "\n")
           d2ldvdt },      
         G.dev.incr  = function(y,mu,sigma,nu,tau, ...) -2*dZIBNB(y, mu, sigma, nu, tau, log=TRUE),
         rqres = expression(    
           rqres(pfun="pZIBNB", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma, nu=nu, tau=tau)
         ), 
         mu.initial = expression(mu<- (y+mean(y))/2 ),
         sigma.initial = expression(
           sigma <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))),
         nu.initial = expression({  nu <- rep(.1,length(y)) }), 
         tau.initial = expression({  tau <- rep(.1, length(y)) }), 
         mu.valid = function(mu) all(mu > 0) , 
         sigma.valid = function(sigma)  all(sigma > 0), 
         nu.valid = function(nu) all(nu > 0), 
         tau.valid = function(tau) all(tau > 0 & tau < 1),  
         y.valid = function(y)  all(y >= 0),
         mean = function(mu, sigma, nu, tau) (1 - tau) * mu,
         variance = function(mu, sigma, nu, tau) (1 - tau) * mu * (1 + mu * nu) * (1 + sigma / nu) *(1/(1-sigma))+tau*(1-tau)*mu^2
    ),
    class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZIBNB<-function(x, mu=1, sigma=1, nu=1, tau=0.1, log=FALSE)
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
  fy <- dBNB(x, mu = mu, sigma=sigma, nu=nu, log = T)
  logfy <- rep(0, length(x))
  logfy[x==0]  <- log(tau+(1-tau)*exp(fy))
  logfy[x!=0]  <- log(1-tau) + fy 
  #logfy <- ifelse((x==0), log(tau+(1-tau)*exp(fy)), (log(1-tau) + fy ))          
  if(log == FALSE) fy2 <- exp(logfy) else fy2 <- logfy
  fy2[x < 0] <- 0
  fy2[x == Inf] <- 0
  fy2  
}
################################################################################
################################################################################
################################################################################
################################################################################
pZIBNB <- function(q, mu=1, sigma=1, nu=1, tau=0.1, lower.tail = TRUE, log.p = FALSE)
{ 
  
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(tau <= 0)|any(tau >= 1)) #In this parametrization  nu = alpha
    stop(paste("tau must be between 0 and 1 ", "\n", ""))
#  if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))
  ly <- max(length(q),length(mu),length(sigma),length(nu),length(tau)) 
  qq <- rep(q, length = ly)
  qq[q==Inf] <- 1
  sigma <- rep(sigma, length = ly)
  mu <- rep(mu, length = ly)   
  nu <- rep(nu, length = ly) 
  cdf <- pBNB(qq, mu = mu, sigma=sigma, nu=nu)
  cdf <- tau + (1-tau)*cdf
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
qZIBNB <- function(p, mu=1, sigma=1, nu=1, tau=0.1, lower.tail = TRUE, log.p = FALSE,  
                   max.value = 10000)
{   
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
if (any(tau <= 0)|any(tau >= 1))
                   stop(paste("tau must be between 0 and 1 ", "\n", ""))
if (log.p == TRUE) p <- exp(p)   else p <- p
if (lower.tail == TRUE)  p <- p  else p <- 1 - p
   ly <- max(length(p),length(mu),length(sigma),length(nu),length(tau)) 
    p <- rep(p, length = ly)      
sigma <- rep(sigma, length = ly)
   mu <- rep(mu, length = ly)   
   nu <- rep(nu, length = ly) 
  tau <- rep(tau, length = ly)
 pnew <- (p-tau)/(1-tau)
 pnew[pnew > 0] <- pnew
 pnew[pnew < 0] <- 0
 #pnew <- ifelse((pnew > 0 ),pnew, 0)
  q <- qBNB(pnew, mu = mu, sigma=sigma, nu, , max.value = max.value)           
  q[p == 0] <- 0
  q[p == 1] <- Inf
  q[p <  0] <- NaN
  q[p >  1] <- NaN
  return(q)  
}
#----------------------------------------------------------------------------------------
rZIBNB <- function(n, mu=1, sigma=1, nu=1, tau=0.1, max.value = 10000)
{ 
  if (any(mu <= 0))  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0))  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(tau <= 0)|any(tau >= 1))  #In this parametrization  nu = alpha
    stop(paste("tau must be between 0 and 1 ", "\n", ""))
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qZIBNB(p, mu=mu, sigma=sigma, nu=nu,, max.value = max.value)
  as.integer(r)
}
#----------------------------------------------------------------------------------------
