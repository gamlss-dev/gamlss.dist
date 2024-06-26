################################################################################
################################################################################
################################################################################
################################################################################
# Linear quadratic family that assumes the following relation for the *variance*
# of the normal distribution Var = mu*(1+s*mu)
# regression on mu and on the sigma (log and identity links)
LQNO <-function (mu.link ="log", sigma.link="log") 
{
    mstats <- checklink(  "mu.link", "Normal", substitute(mu.link),    
                                              c("log","identity"))
    dstats <- checklink("sigma.link", "Normal", substitute(sigma.link), 
                                              c("log","identity"))        
    structure(
  list(family = c("LQNO", "Normal with Linear Quadratic relationship between
                  mean and variance"),
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
                 dldm = function(y,mu,sigma)   
                    {
                      c0 <- (1+mu*sigma)
                      c1 <- sigma/c0
                      c2 <- (y-mu)/(mu*c0)
                      -0.5*(1/mu+c1)+c2+0.5*(y-mu)*c1*c2+0.5*c2*c2*c0
                    },
               d2ldm2 = function(mu,sigma) 
                  {
                  -(1 + 2*mu*(1 + 2*sigma)*(1 + mu*sigma))/(2.*(mu*(1 + mu*sigma))^2)
                  },
                 dldd = function(y,mu,sigma)  
                   {
                      c0 <- (1+mu*sigma) 
                      0.5*( ((y-mu)/c0)^2-mu/c0)
                   },
               d2ldd2 = function(mu,sigma) 
                  {
                  -mu^2/(2.*(1 + mu*sigma)^2)
                 
                  },
              d2ldmdd = function(mu,sigma) 
                  {
                    -(1 + 2*mu*sigma)/(2.*(1 + mu*sigma)^2)
                  },
          G.dev.incr  = function(y,mu,sigma,...) -2*dLQNO(y,mu,sigma,log=TRUE),                         
                rqres = expression(rqres(pfun="pLQNO", type="Continuous", y=y, mu=mu, sigma=sigma)),
           mu.initial = expression({ mu <- abs((y+mean(y))/2) }),
        sigma.initial = expression({sigma <- rep(abs((var(y)/mean(y)-1))/mean(y),length(y))}), 
             mu.valid = function(mu) TRUE , 
          sigma.valid = function(sigma) all(sigma > 0), 
              y.valid = function(y)  TRUE
          ),
            class = c("gamlss.family","family"))
}

################################################################################
################################################################################
################################################################################
################################################################################
dLQNO<-function(x, mu=1, sigma=1, log=FALSE)
 { 
    if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    fy <- dnorm(x, mean=mu, sd=sqrt(mu*(1+sigma*mu)), log=log)
    fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pLQNO <- function(q, mu=1, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { 
    if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    cdf <- pnorm(q, mean=mu, sd=sqrt(mu*(1+sigma*mu)), lower.tail = lower.tail, log.p = log.p)
    cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qLQNO <- function(p, mu=1, sigma=1, lower.tail = TRUE, log.p = FALSE)
  { 
    if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (log.p==TRUE) p <- exp(p) else p <- p
#    if (any(p < 0)|any(p > 1))  stop(paste("p must be between 0 and 1", "\n", "")) 
    q <- qnorm(p, mean=mu, sd=sqrt(mu*(1+sigma*mu)), lower.tail = lower.tail )
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
rLQNO <- function(n, mu=1, sigma=1)
  { 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", "")) 
    r <- rnorm(n, mean=mu, sd=sqrt(mu*(1+sigma*mu)))
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################
