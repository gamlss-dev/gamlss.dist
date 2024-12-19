################################################################################
################################################################################
################################################################################
################################################################################
# the ZIPF distrbution 
# The pdf could  have used the zeta() function on package require(Rmpfr)
# here we have used the function zetaP() which is a 
# simplify version of Thomas Lee zeta()
################################################################################
################################################################################
################################################################################
################################################################################
# the fitting function
ZIPF <- function (mu.link = "log") 
{
  mstats <- checklink("mu.link", "ZIPF", substitute(mu.link),c("inverse", "log", "sqrt", "identity")) 
  structure(
    list(    family = c("ZIPF", "zipf distribution"),
         parameters = list(mu = TRUE),
              nopar = 1, 
               type = "Discrete", 
            mu.link = as.character(substitute(mu.link)), 
         mu.linkfun = mstats$linkfun, 
         mu.linkinv = mstats$linkinv, 
              mu.dr = mstats$mu.eta, 
               dldm = function(y,mu)
                 {
                 nd <- numeric.deriv(dZIPF(y, mu, log=TRUE), "mu", delta=0.001)
               dldm <- as.vector(attr(nd, "gradient"))
               dldm
                },
              d2ldm2 = function(y,mu)
                {
                 nd <- numeric.deriv(dZIPF(y, mu, log=TRUE), "mu", delta=0.001)
               dldm <- as.vector(attr(nd, "gradient"))
             d2ldv2 <- -dldm*dldm
             d2ldv2
                },
         G.dev.incr  = function(y,mu,...) -2*dZIPF(x = y, mu = mu, log = TRUE),
         rqres = expression(rqres(pfun="pZIPF", type="Discrete", ymin=1, y=y, mu=mu)), 
         mu.initial =expression({mu <- rep(.1,length(y)) } ),
         mu.valid = function(mu) all(mu > 0), 
         y.valid = function(y)  all(y >= 1),
            mean = function(mu) ifelse(mu > 1, zetaP(mu) / zetaP(mu +1), Inf),
        variance = function(mu) ifelse(mu > 2, (zetaP(mu + 1) * zetaP(mu - 1) - (zetaP(mu))^2) / (zetaP(mu + 1))^2, Inf)
    ),
    class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZIPF<- function(x, mu = 1, log = FALSE)
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
 # if (any(x < 1) )  stop(paste("x must be >=1", "\n", ""))
     ly <- max(length(x),length(mu)) 
      x <- rep(x, length = ly)      
     mu <- rep(mu, length = ly)   
   logL <- -(mu+1)*log(x)-log(zetaP(mu+1)) # or zeta
    lik <- if (log) logL else exp(logL)
    lik[x <= 0] <- 0
  as.numeric(lik)
}
################################################################################
################################################################################
################################################################################
################################################################################
pZIPF <- function(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
{
#----------  
  Zeta.aux<- function (shape, qq) 
  {
    LLL <- max(length(shape), length(qq))
    if (length(shape) != LLL) 
      shape <- rep_len(shape, LLL)
    if (length(qq) != LLL) 
      qq <- rep_len(qq, LLL)
    if (any(qq < 12 - 1)) 
      warning("all values of argument 'q' should be 12 or more")
    aa <- qq
    B2 <- c(1/6, -1/30, 1/42, -1/30, 5/66, -691/2730, 7/6, -3617/510)
    kk <- length(B2)
    ans <- 1/((shape - 1) * (1 + aa)^(shape - 1)) + 0.5/(1 + aa)^shape
    term <- (shape/2)/(1 + aa)^(shape + 1)
    ans <- ans + term * B2[1]
    for (mm in 2:kk) 
    {
      term <- term * (shape + 2 * mm - 3) * (shape + 2 * mm - 
                                               2)/((2 * mm - 1) * 2 * mm * (1 + aa)^2)
      ans <- ans + term * B2[mm]
    }
    ifelse(aa - 1 <= qq, ans, rep(0, length(ans)))
  }
################################################################################
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
       ly <- max(length(q),length(mu)) 
        q <- rep(q, length = ly)      
       mu <- rep(mu, length = ly)
      ans <- rep_len(0, ly)
   qfloor <- floor(q)
  for (nn in 1:(12 - 1)) ans <- ans + as.numeric(nn <= qfloor)/nn^(mu + 1)
          vecTF <- (12 - 1 <= qfloor)
  if (lower.tail) 
       {
            if (any(vecTF)) 
              ans[vecTF] <- zetaP(mu[vecTF] + 1) - Zeta.aux(mu[vecTF] + 
                                                                1, qfloor[vecTF] + 1)
       }
       else 
       {
            ans <- zetaP(mu + 1) - ans
            if (any(vecTF)) 
              ans[vecTF] <- Zeta.aux(mu[vecTF] + 1, qfloor[vecTF] + 
                                       1)
       }
   cdf <- ans/zetaP(mu + 1)
   cdf[q <= 0] <- 0 
   cdf[q > Inf] <- 1 
   cdf
}
################################################################################
################################################################################
################################################################################
################################################################################
qZIPF <- function(p, mu = 1, lower.tail = TRUE, log.p = FALSE, max.value = 10000)
{      
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (log.p==TRUE) p <- exp(p) else p <- p
  if (lower.tail==TRUE) p <- p else p <- 1-p    
      ly <- length(p)                                                       
     QQQ <- rep(0,ly)                         
     nmu <- rep(mu, length = ly)     
      ly <- max(length(p),length(mu)) 
       p <- rep(p, length = ly)      
      mu <- rep(mu, length = ly) 
     
  for (i in seq(along=p))                                                          
  {
    cumpro <- 0                                                                         
    if (p[i]+0.000000001 >= 1) QQQ[i] <- Inf
    else  
    {  
      for (j in seq(from = 1, to = max.value))
      {
        cumpro <-  pZIPF(j, mu = nmu[i],  log.p = FALSE) 
        # else  cumpro+dSICHEL(j, mu = nmu[i], sigma = nsigma[i], nu = nnu[i], log = FALSE)# the above is faster 
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
rZIPF <- function(n, mu = 1, max.value = 10000)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", "")) 
  n <- ceiling(n)
  p <- runif(n)
  r <- qZIPF(p, mu = mu, max.value = max.value)
  as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################
# this is the function from VGAM
# it also need the function Zeta.derivative() which need C code
zetaP <-function (x) 
{
  Zeta.aux<- function (shape, qq) 
  {
    LLL <- max(length(shape), length(qq))
    if (length(shape) != LLL) 
      shape <- rep_len(shape, LLL)
    if (length(qq) != LLL) 
      qq <- rep_len(qq, LLL)
    if (any(qq < 12 - 1)) 
      warning("all values of argument 'q' should be 12 or more")
      aa <- qq
      B2 <- c(1/6, -1/30, 1/42, -1/30, 5/66, -691/2730, 7/6, -3617/510)
      kk <- length(B2)
     ans <- 1/((shape - 1) * (1 + aa)^(shape - 1)) + 0.5/(1 + aa)^shape
    term <- (shape/2)/(1 + aa)^(shape + 1)
     ans <- ans + term * B2[1]
    for (mm in 2:kk) 
      {
    term <- term * (shape + 2 * mm - 3) * (shape + 2 * mm - 
                       2)/((2 * mm - 1) * 2 * mm * (1 + aa)^2)
     ans <- ans + term * B2[mm]
      }
    ifelse(aa - 1 <= qq, ans, rep(0, length(ans)))
  }
   aa <- 12 
  ans <- 0
  for (ii in 0:(aa - 1)) ans <- ans + 1/(1 + ii)^x
   ans <- ans + Zeta.aux(shape = x, aa)    
  ans
}
################################################################################
################################################################################
################################################################################
################################################################################