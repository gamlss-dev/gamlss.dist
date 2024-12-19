#-------------------------------------------------------------
# The discretised Burr XII distribution 
# Authors
# Robert Rigby, Mikis Stasinopoulos, Fernanda De Bastiani  
# ------------------------------------------------------------
DBURR12 <- function (mu.link = 'log', sigma.link = 'log', nu.link = 'log') {
  mstats <- checklink('mu.link', 'Discrete Burr XII', substitute(mu.link),
             c('1/mu^2', 'log', 'identity'))
  dstats <- checklink('sigma.link', 'Discrete Burr XII', substitute(sigma.link),
             c('inverse', 'log', 'identity'))
  vstats <- checklink('nu.link', 'Discrete Burr XII', substitute(nu.link),
             c('1/nu^2', 'log', 'identity'))
  structure(
    list( family = c('DBURR12', 'Discrete Burr XII'),
      parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE),
           nopar = 3,
            type = 'Discrete',
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
            dldm = function (y, mu, sigma, nu) {
              Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
            dSdmu <- function(y) ifelse(y >=1, sigma*nu*((y^sigma)/(mu^(sigma+1)))*
                                          (1+(y/mu)^sigma)^(-nu-1), 0)
            dldm  <- (dSdmu(y)-dSdmu(y+1))/(Sur(y)-Sur(y+1)) 
            dldm                                    
            },
      d2ldm2 = function (y, mu, sigma, nu) {
              Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
            dSdmu <- function(y) ifelse(y >=1, sigma*nu*((y^sigma)/(mu^(sigma+1)))*
                                      (1+(y/mu)^sigma)^(-nu-1) ,0)
            dldm  <-   (dSdmu(y)- dSdmu(y+1))/(Sur(y)-Sur(y+1)) 
          -(dldm^2)
      },
      dldd = function (y, mu, sigma, nu){
        Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
   dSdsigma <- function(y) ifelse(y >=1, -nu*log(y/mu)*(y/mu)^sigma*
                                    (1+(y/mu)^sigma)^(-nu-1),0)
      dldd  <-   (dSdsigma(y)- dSdsigma(y+1))/(Sur(y)-Sur(y+1)) 
      dldd
      } ,
      d2ldd2 = function (y, mu, sigma, nu) {
             Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
        dSdsigma <- function(y) ifelse(y >=1, -nu*log(y/mu)*(y/mu)^sigma*
                                         (1+(y/mu)^sigma)^(-nu-1),0)
            dldd <- (dSdsigma(y)- dSdsigma(y+1))/(Sur(y)-Sur(y+1)) 
      -(dldd^2)
      },
      d2ldmdd = function (y, mu, sigma, nu) {
            Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
          dSdmu <- function(y) ifelse(y >=1, sigma*nu*((y^sigma)/(mu^(sigma+1)))*
                                      (1+(y/mu)^sigma)^(-nu-1) ,0)
          dldm  <-   (dSdmu(y)- dSdmu(y+1))/(Sur(y)-Sur(y+1)) 
       dSdsigma <- function(y) ifelse(y >=1, -nu*log(y/mu)*(y/mu)^sigma*
                                         (1+(y/mu)^sigma)^(-nu-1),0)
           dldd <- (dSdsigma(y)- dSdsigma(y+1))/(Sur(y)-Sur(y+1)) 
        -(dldm * dldd)
      },
      dldv = function (y, mu, sigma, nu){
           Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
         dSdnu <- function(y) ifelse(y >=1, -log(1+(y/mu)^sigma)*
                                       (1+(y/mu)^sigma)^{-nu}, 0)
         dldv  <-   (dSdnu(y)- dSdnu(y+1))/(Sur(y)-Sur(y+1)) 
         dldv
      },
      d2ldv2 = function (y, mu, sigma, nu) {
           Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
         dSdnu <- function(y) ifelse(y >=1, -log(1+(y/mu)^sigma)*
                                      (1+(y/mu)^sigma)^{-nu}, 0)
         dldv  <-   (dSdnu(y)- dSdnu(y+1))/(Sur(y)-Sur(y+1)) 
        -(dldv * dldv)
      },
   d2ldmdv = function (y, mu, sigma, nu) {
          Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
        dSdmu <- function(y) ifelse(y >=1, sigma*nu*((y^sigma)/(mu^(sigma+1)))*
                                   (1+(y/mu)^sigma)^(-nu-1) ,0)
        dldm  <-   (dSdmu(y)- dSdmu(y+1))/(Sur(y)-Sur(y+1)) 
        dSdnu <- function(y) ifelse(y >=1, -log(1+(y/mu)^sigma)*
                                      (1+(y/mu)^sigma)^{-nu}, 0)
        dldv  <-   (dSdnu(y)- dSdnu(y+1))/(Sur(y)-Sur(y+1)) 
     -(dldm * dldv)
      },
   d2ldddv = function (y, mu, sigma, nu) {
       Sur <- function(y) (1+(y/mu)^sigma)^(-nu)
  dSdsigma <- function(y) ifelse(y >=1, -nu*log(y/mu)*(y/mu)^sigma*
                                      (1+(y/mu)^sigma)^(-nu-1),0)
     dldd  <-   (dSdsigma(y)- dSdsigma(y+1))/(Sur(y)-Sur(y+1)) 
     dSdnu <- function(y) ifelse(y >=1, -log(1+(y/mu)^sigma)*
                                   (1+(y/mu)^sigma)^{-nu}, 0)
     dldv  <-   (dSdnu(y)- dSdnu(y+1))/(Sur(y)-Sur(y+1)) 
     -(dldd * dldv)
     },
      G.dev.incr = function (y, mu, sigma, nu,...)  -2 * dDBURR12(y, mu,
        sigma, nu, log = TRUE),
           rqres = expression(rqres(pfun = 'pDBURR12', type = 'Discrete', ymin = 0, y = y, mu = mu,
                               sigma = sigma, nu = nu)),
      mu.initial = expression(mu <- (y + (mean(y)) / 2)),
   sigma.initial = expression(sigma <- rep(2, length(y))),
      nu.initial = expression(nu <- rep(2, length(y))), 
        mu.valid = function(mu) all(mu > 0), 
     sigma.valid = function(sigma) all(sigma > 0), 
        nu.valid = function(nu) all(nu > 0),    
         y.valid = function(y) all(y >= 0)
    ),
    class = c('gamlss.family', 'family')
  )
}
#-----------------------------------------------------------------------
dDBURR12 <- function(x, mu = 5, sigma = 2, nu = 2, log = FALSE) 
  {
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0))  stop(paste("nu must be greater than 0 ", "\n", "")) 
 # if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))  
     ly <- max(length(x),length(mu),length(sigma),length(nu)) 
      x <- rep(x, length = ly)      
  sigma <- rep(sigma, length = ly)
     mu <- rep(mu, length = ly)   
     nu <- rep(nu, length = ly) 
     Sur <- function(x) (1+(x/mu)^sigma)^(-nu)
  prob <- Sur(x)-Sur(x+1) #(1+(x/mu)^sigma)^(-nu)-(1+((x+1)/mu)^sigma)^(-nu)
  prob <- if(log ) log(prob) else prob
  prob <- ifelse(x < 0, 0, prob) 
  prob[x < 0] <- 0
  prob[x == Inf] <- 0
  return(prob)
}


pDBURR12 <- function(q, mu = 5, sigma = 2, nu = 2,  lower.tail = TRUE, log.p = FALSE) 
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0))  stop(paste("nu must be greater than 0 ", "\n", "")) 
 # if (any(q < 0) )  stop(paste("x must be >=0", "\n", ""))  
   ly <- max(length(q),length(mu),length(sigma),length(nu)) 
    q <- rep(q, length = ly)      
sigma <- rep(sigma, length = ly)
   mu <- rep(mu, length = ly)   
   nu <- rep(nu, length = ly) 
  cdf <- 1 - (1+((q+1)/mu)^sigma)^(-nu) 
 if(lower.tail==TRUE) cdf <- cdf else cdf=1-cdf
 if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf) 
  cdf[q < 0] <- 0 
  cdf[q == Inf] <- 1     
  cdf
}

# qDBURR12 <- function(p, mu = 5, sigma = 2, nu = 2,  lower.tail = TRUE, log.p = FALSE) 
# {
#   if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
#   if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
#   if (any(nu <= 0))  stop(paste("nu must be greater than 0 ", "\n", ""))  
#   if (any(p <= 0) | any(p >= 1))  stop(paste("p must be between 0 and 1", "\n", "")) 
#   if (log.p==TRUE) p <- exp(p) else p <- p
#   if (lower.tail==TRUE) p <- p else p <- 1-p 
#       ly <- max(length(p),length(mu),length(sigma),length(nu)) 
#        p <- rep(p, length = ly)      
#    sigma <- rep(sigma, length = ly)
#       mu <- rep(mu, length = ly)   
#       nu <- rep(nu, length = ly) 
#        q <- mu*(exp(-(log(1-p))/nu)-1)^(1/sigma)-1 
#     ceiling(q)
# }
 
qDBURR12 <- function(p, mu = 5, sigma = 2, nu = 2,  lower.tail = TRUE, log.p = FALSE) 
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0))  stop(paste("nu must be greater than 0 ", "\n", ""))  
  # if (any(p <= 0) | any(p >= 1))  stop(paste("p must be between 0 and 1", "\n", "")) 
  if (log.p==TRUE) p <- exp(p) else p <- p
  if (lower.tail==TRUE) p <- p else p <- 1-p 
     ly <- max(length(p),length(mu),length(sigma),length(nu)) 
      p <- rep(p, length = ly)      
  sigma <- rep(sigma, length = ly)
     mu <- rep(mu, length = ly)   
     nu <- rep(nu, length = ly) 
      q <- round(mu*(exp(-(log(1-p))/nu)-1)^(1/sigma)-1L)
      q[p == 0] <- 0
      q[p == 1] <- Inf
      q[p <  0] <- NaN
      q[p >  1] <- NaN
      return(q)
}
 
rDBURR12 <- function(n, mu = 5, sigma = 2, nu = 2) 
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(nu <= 0))  stop(paste("nu must be greater than 0 ", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qDBURR12(p, mu=mu, sigma=sigma, nu=nu )
  as.integer(r)
} 

