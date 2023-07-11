#----------------------------------------------------------------------------------------
# link funtions had added for nu and tau to make sure that gamlssML() works 31-1-18
#---------------------------------------------------------------------------------------
NET <- function (mu.link ="identity", sigma.link="log", nu.link ="identity", tau.link ="identity") 
{
    mstats <- checklink("mu.link",    "NET", substitute(   mu.link), c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "NET", substitute(sigma.link), c("inverse", "log", "identity", "own"))
    vstats <- checklink("nu.link", "NET", substitute(nu.link), c("identity"))
    tstats <- checklink("tau.link", "NET", substitute(tau.link), c("identity"))
    structure(
          list(family = c("NET", "Normal Exponential t"),
           parameters = list(mu=TRUE, sigma=TRUE, nu=FALSE, tau=FALSE), 
                nopar = 4, 
                 type = "Continuous",  
              mu.link = as.character(substitute(mu.link)), 
           sigma.link = as.character(substitute(sigma.link)),
              nu.link = as.character(substitute(mu.link)), 
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
                 dldm = function(y,mu,sigma,nu,tau){ 
                                  k1 <- nu
                                  k2 <- tau
                                  tc <- (y-mu)/sigma
                               dldtc <- (abs(tc)<= k1)*(-tc) + 
                                        ((abs(tc)>k1)&(abs(tc)<= k2))*(-k1*sign(tc)) +
                                        (abs(tc) > k2)*(-k1*k2/tc)
                                dldm <- -dldtc/sigma
                                dldm 
                                    },
               d2ldm2 = function(y,mu,sigma,nu,tau) {
                                  k1 <- nu
                                  k2 <- tau
                                  c1 <- (1-2*pnorm(-k1))*sqrt(2*pi)
                                  c2 <- (2/k1)*exp(-((k1^2)/2))
                                  c3 <- 2*exp(-k1*k2+((k1^2)/2))/((k1*k2-1)*k1)
                                 ct  <- 1/(c1+c2+c3)
                                # tc  <- (y-mu)/sigma
                              d2ldm2 <- -ct*sqrt(2*pi)*(1-2*pnorm(-k1))+
                                        ((2*ct*k1)/(k1*k2+1))*exp(-k1*k2+(k1^2)/2)  
                              d2ldm2 <- d2ldm2/sigma^2},
                 dldd = function(y,mu,sigma,nu,tau) {
                                 k1 <- nu
                                 k2 <- tau
                                 tc <- (y-mu)/sigma
                              dldtc <- (abs(tc)<= k1)*(-tc) + 
                                        ((abs(tc)>k1)&(abs(tc)<= k2))*(-k1*sign(tc)) +
                                        (abs(tc) > k2)*(-k1*k2/tc)
                               dldd <- -(1+tc*dldtc)/sigma
                               dldd
                                    },
               d2ldd2 = function(y,mu,sigma,nu,tau) {
                                   k1 <- nu
                                   k2 <- tau
                                   c1 <- (1-2*pnorm(-k1))*sqrt(2*pi)
                                   c2 <- (2/k1)*exp(-((k1^2)/2))
                                   c3 <- 2*exp(-k1*k2+((k1^2)/2))/((k1*k2-1)*k1)
                                   ct <- 1/(c1+c2+c3)
                                  #tc  <- (y-mu)/sigma
                               d2ldd2 <- 2*ct*k1*exp(-(k1^2)/2)-
                                           ct*sqrt(2*pi)*(1-2*pnorm(-k1))+
                                           (2*ct*k1*(k2^2)/(k1*k2-1))*exp(-k1*k2+(k1^2)/2)
                               d2ldd2 <- (d2ldd2-1)/(sigma^2)
                               },
               d2ldmdd = function(y)  rep(0,length(y)), 
                  dldv = function(y)  rep(0,length(y)), 
                  dldt = function(y)  rep(0,length(y)),
                d2ldv2 = function(y)  rep(0,length(y)),
                d2ldt2 = function(y)  rep(0,length(y)),
               d2ldmdv = function(y)  rep(0,length(y)), 
               d2ldmdt = function(y)  rep(0,length(y)), 
               d2ldddv = function(y)  rep(0,length(y)), 
               d2ldddt = function(y)  rep(0,length(y)),
               d2ldvdt = function(y)  rep(0,length(y)), 
           G.dev.incr  = function(y,mu,sigma,nu,tau,...)  -2*dNET(y,mu,sigma,nu,tau, log=TRUE),  
                 rqres = expression(rqres(pfun="pNET", type="Continuous", y=y, mu=mu, sigma=sigma, nu=nu, tau=tau)),
            mu.initial = expression(mu <- (y-mean(y))/2 ),
         sigma.initial = expression(sigma <- rep((sd(y)+0.001),length(y))),
            nu.initial = expression(nu <- rep(1.5,length(y))), # 1.5  default
           tau.initial = expression(tau <- rep(2,length(y))),  # 2   default
              mu.valid = function(mu) TRUE,  
           sigma.valid = function(sigma)  all(sigma > 0),
              nu.valid = function(nu) all(nu > 0), 
             tau.valid = function(tau, nu) all(tau > nu),
               y.valid = function(y) TRUE,
                  mean = function(mu, sigma, nu, tau) {
                                  if (nu * tau > 2) {mu} else{NaN}
                         },
              variance = function(mu, sigma, nu, tau) {
                                  if (nu * tau > 3) {
                                    c1 <- sqrt(2*pi) *  (2*pnorm(nu) - 1)
                                    c2 <- 2/nu * exp(-nu^2/2) 
                                    c3 <- 2/(nu*(nu*tau-1)) * exp(-nu*tau + nu^2/2)
                                    c <- 1/(c1 + c2 + c3) 
                                    return(2 * sigma^2 * c * (sqrt(2*pi) * (pnorm(nu) - 1/2) + 
                                           (2/nu + 2/nu^3) * exp(-nu^2/2) +
                                           (nu^2*tau^2 + 4*nu*tau + 6) / (nu^3 * (nu*tau - 3)) * exp(-nu*tau + nu^2/2)))
                                  } else {
                                    return(Inf)
                                  }
                         }
          ),
            class = c("gamlss.family","family"))
}
#----------------------------------------------------------------------------------------
dNET <- function(x, mu=0, sigma=1, nu=1.5, tau=2, log=FALSE)
 {
          if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
          if (any(nu <= 0))  stop(paste("nu must be positive", "\n", ""))  
          if (any(tau <= 0))  stop(paste("tau must be positive", "\n", ""))  
          if (any(tau < nu))  stop(paste(" tau must greater or equal than  nu", "\n", ""))  
        k1 <- nu
        k2 <- tau
        c1 <- (1-2*pnorm(-k1))*sqrt(2*pi)
        c2 <- (2/k1)*exp(-((k1^2)/2))
        c3 <- 2*exp(-k1*k2+((k1^2)/2))/((k1*k2-1)*k1)
       ct  <- 1/(c1+c2+c3)
       tc  <- (x-mu)/sigma
        d1 <- (abs(tc) <= k1)*(-(tc^2)/2)
        d2 <- ((abs(tc) > k1) & (abs(tc) <= k2))*(-k1*abs(tc)+((k1^2)/2))
        d3 <- ifelse(x==0, 0, (abs(tc) > k2)*(-k1*k2*log(abs(tc)/k2)-k1*k2+((k1^2)/2)))
    loglik <- log(ct)-log(sigma)+d1+d2+d3 
      if(log==FALSE) ft  <- exp(loglik) else ft <- loglik 
       ft
  }      
#----------------------------------------------------------------------------------------
pNET <- function(q,  mu=0, sigma=1, nu=1.5, tau=2,  lower.tail = TRUE, log.p = FALSE)
 {  
          if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
          if (any(nu <= 0))  stop(paste("nu must be positive", "\n", ""))  
          if (any(tau <= 0))  stop(paste("tau must be positive", "\n", ""))  
          if (any(tau < nu))  stop(paste(" tau must greater or equal than  nu", "\n", ""))  
        k1 <- nu
        k2 <- tau
        c1 <- (1-2*pnorm(-k1))*sqrt(2*pi)
        c2 <- (2/k1)*exp(-((k1^2)/2))
        c3 <- 2*exp(-k1*k2+((k1^2)/2))/((k1*k2-1)*k1)
        ct <- 1/(c1+c2+c3)
        tc <- (q-mu)/sigma
                                    #Fk2 is cdf up to the point -k2
       Fk2 <- (ct*(k2^(k1*k2))/(k1*k2-1)) * exp(-k1*k2+((k1^2)/2))* 
               ifelse(tc==0,1, abs(tc)^(-k1*k2+1)) 
                                    #cf1 is cdf value up to the point -k2
      cdf1 <- (ct/(k1*(k1*k2-1)))*exp(-k1*k2+(k1^2)/2)
                                    #Fk1 is cdf up to the point -k1
       Fk1 <- cdf1 + (ct/k1)*exp(-k1*abs(tc)+(k1^2)/2)
                                    #cf2 is cdf value up to the point -k1
      cdf2 <- cdf1 +(ct/k1)*exp(-(k1^2)/2)
                                    #F0 is cdf up to the point 0 
        F0 <- cdf2 + ct * sqrt(2*pi) * (pnorm(-abs(tc)) - pnorm(-k1))
                                    #calclulate the cdf0=F(-tc) to get the cdf of
                                    #the positive tc as 1-F(-tc)
      #cdf0 <- cdf2 + ct * sqrt(2*pi) * (pnorm(-tc) - pnorm(-k1))
                                    #cdf.tc is the cdf function of all the points
       cdf <- (tc <= -k2) * Fk2 + #negative tc
                ((tc > -k2) & (tc <= -k1)) * Fk1 + #negative tc
                ((tc > -k1) & (tc <= 0)) * F0 + #negative tc
                ((tc > 0) & (tc <= k1)) * (1-F0)+
                ((tc > k1) & (tc <= k2)) * (1-Fk1)+
                (tc > k2)* (1-Fk2)
               #positive tc
  if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
  if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
       cdf                                       
 }
#----------------------------------------------------------------------------------------
qNET <- function(p,  mu=0, sigma=1, nu=1.5, tau=2,  lower.tail = TRUE, log.p = FALSE )
{ 
  #---functions--------------------------------------------   
  #-----------------------------------------------------------------
  #if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
  if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
  if (any(nu <= 0))  stop(paste("nu must be positive", "\n", ""))  
  if (any(tau <= 0))  stop(paste("tau must be positive", "\n", ""))  
  if (any(tau < nu))  stop(paste(" tau must greater or equal than  nu", "\n", ""))     
  if (log.p==TRUE) p <- exp(p) else p <- p
  if (lower.tail==TRUE) p <- p else p <- 1-p
  if (any(p < 0)|any(p > 1))  stop(paste("p must be between 0 and 1", "\n", ""))
  #if (any(any(p > 0.993))  stop(paste("p must be between 0 and 1", "\n", "")) 
     lp <-  max(length(p),length(mu),length(sigma),length(nu), length(tau))
      p <- rep(p, length = lp)                                                                      
  sigma <- rep(sigma, length = lp)
     mu <- rep(mu, length = lp)
     nu <- rep(nu, length = lp)
    tau <- rep(tau, length = lp)
     p1 <- p2 <-  ifelse(p<=0.5, p,(1-p)) 
     c1 <- (1-2*pnorm(-nu))*sqrt(2*pi)
     c2 <- (2/nu)*exp(-((nu^2)/2))
     c3 <- 2*exp(-nu*tau+((nu^2)/2))/((nu*tau-1)*nu)
     ct <- 1/(c1+c2+c3)
      b <- (ct/(nu*(nu*tau-1)))*exp(-nu*tau+(nu^2)/2)
     z1 <- -(b*nu*(tau^(nu*tau))/p1)^(1/(nu*tau-1))
     p1 <- ifelse(p1>=(nu*tau*b), p1, 0.5)
     z2 <- -(nu/2)+(1/nu)*log((nu/ct)*(p1-b))
     z3 <- qnorm(pnorm(-nu)+(1/(sqrt(2*pi)))*((p1-b)/ct - (1/nu) *exp(-(nu^2)/2)))
     q1 <- (p2<=(nu*tau*b))*z1 + ( (p1>(nu*tau*b)) & (p1<=(b+(ct/nu)*exp(-(nu^2)/2))))*z2+
           ((p1>b+(ct/nu)*exp(-(nu^2)/2)) & (p1<0.5))*z3
     q <- ifelse(p<=0.5,q1,-q1) 
     q <- mu+sigma*q
q
}
#----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
rNET <- function(n, mu=0, sigma=1, nu=1.5, tau=2)
{
  if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
  n <- ceiling(n)
  p <- runif(n)
  r <- qNET(p, mu = mu, sigma = sigma, nu = nu, tau = tau)
  r
}
#----------------------------------------------------------------- 


