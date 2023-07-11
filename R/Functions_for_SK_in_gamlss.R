#-----------------------------------------------------------------
#-----------------------------------------------------------------
#require(gamlss)
# functions for skewness and kurtosis 
# created by Mikis Stasinopoulos on the 3-2-19
#  1)        momentSK() : sample moment skewness and kurtosis (now includes Jarque-Bera test)
#  2)       centileSK() : sample centile skewness and kurtosis
#  3)     centileSkew() : sample centile skewness only
#  4)     centileKurt() : sample centile kurtosis only
#  5)   theoCentileSK() : theorericalcentile skewness and kurtosis
#  6)   plotCentileSK() : plotting theoretical  skewness and kurtosis as function of p
#  7)    SKmoment_col() : plotting moment SK in colour 
#  8)   SKmoment_gray() : plotting moment SK in gray  
#  9)    SKmomentBoth() : plotting moment SK in gray for S -1 to 1   
# 10)   SKcentile_col() : plotting centile SK in colour 
# 11)  SKcentile_gray() : plotting centile SK in gray
# 12)   SKcentileBoth() : plotting centile SK in gray for S -1 to 1
# 13)     checkMomentSK : data moments plot 
# 14)    checkCentileSK : data centile plot 
#-----------------------------------------------------------------
# FUNCTION 1 
#-----------------------------------------------------------------
# SAMPLE moment skewness + kurtosis
# moment skewness and kurtosis estimation   
momentSK <- function(x, weights=NULL)
{
if (any(is.na(x)))  warning("NA's will be removed from x")  
if (!length(weights)) weights <- rep(1, length(x))
       i <- is.na(weights) | weights == 0 | is.na(x) | is.infinite(x)
if (any(i)) 
{
       x <- x[!i]
 weights <- weights[!i]
}
       g <- weights/sum(weights) 
     m.1 <- sum(g*x)
     m.2 <- sum(g*(x-m.1)^2)
     m.3 <- sum(g*(x-m.1)^3)
     m.4 <- sum(g*(x-m.1)^4)      
   n.obs <- sum(weights)# length(x) 
    #m.1 <- mean(x)
    #m.2 <- sum((x-m.1)^2)/n.obs  
    #m.3 <- sum((x-m.1)^3)/n.obs 
    #m.4 <- sum((x-m.1)^4)/n.obs 
 gamma.1 <- m.3/(m.2^(1.5)) # skew = gamma_1 =sqrt(beta_1)
  beta.1 <- gamma.1^2
  beta.2 <- m.4/(m.2^2)     # kurt = beta_2
# excess kurtosis
gamma.2 <- (beta.2-3)         # ekurt = gamma_2  
# transformed skewness and kurtosis, see Jones and Pewey (2009), page 5, Figure 2
  tskew <- gamma.1/(1+abs(gamma.1))
  tkurt <- gamma.2/(1+abs(gamma.2)) 
  #Jarque_bera_test = gamma_1/(6/n)+gamma_2
   list(mom.skew = gamma.1, # moment .skew
  trans.mom.skew = tskew, 
        mom.kurt = beta.2,
 excess.mom.kurt = gamma.2, # excess kurtosis 
  trans.mom.kurt = tkurt,
jarque.bera.test = (n.obs/6)*beta.1+(n.obs/24)*gamma.2^2)
}
#-----------------------------------------------------------------
#  Y <- rSST(1000, nu=.5, tau=5)
#  momentSK(Y)
#  library(tseries)
# jarque.bera.test(Y)
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# FUNCTION 2 
#-----------------------------------------------------------------
# SAMPLE centile skewness + kurtosis
centileSK <- function(x, cent=c(1, 25), weights=NULL)
{
# local function -----------------------------------------
  quantileW <- function (y, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1)) 
  {
    if (!length(weights)) 
      return(quantile(y, probs = probs))
    if (any(probs < 0 | probs > 1)) 
      stop("Probabilities must be between 0 and 1 inclusive")
    i <- is.na(weights) | weights == 0 | is.na(x) | is.infinite(x)
    if (any(i)) 
    {
           y <- y[!i]
     weights <- weights[!i]
    }
       ysort <- unique(sort(y))
    weights1 <- tapply(weights, y, sum)
        cumu <- cumsum(weights1)
           x <- ysort
         wts <- weights1
           n <- sum(wts)
       order <- 1 + (n - 1) * probs
         low <- pmax(floor(order), 1)
        high <- pmin(low + 1, n)
       order <- order%%1
        allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant", 
                   f = 1, rule = 2)$y
           k <- length(probs)
   quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
        nams <- paste(format(round(probs * 100, 
                               if (length(probs) > 1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")
    names(quantiles) <- nams
    return(quantiles)  
  }
#-------------------------------------------------------------
# the main function starts here   
#-------------------------------------------------------------
if (any(is.na(x)))  warning("NA's will be removed from x")  
if (!length(weights)) weights <- rep(1, length(x))
       i <- is.na(weights) | weights == 0 | is.na(x) 
if (any(i)) 
  {   x <- x[!i]
weights <- weights[!i]
  }
# if (any(is.na(x)))  warning("NA's have been removed from x")
#                    x <- x[!is.na(x)]
#   centiles for
#       1%  cent[1]
#       25% cent[2]
#       50% cent[3]
#       75% cent[4] and
#       99% cent[5]
            Cent <- quantileW(x, weights=weights,
                         probs=c(cent[1]/100, cent[2]/100, 0.5, 1-(cent[2]/100),1-(cent[1]/100)))
           S0.01 <- ((Cent[1]+Cent[5])/2 -Cent[3])/((Cent[5]-Cent[1])/2)
    names(S0.01) <- ""
           S0.25 <- ((Cent[2]+Cent[4])/2 -Cent[3])/((Cent[4]-Cent[2])/2)
   names(S0.25 ) <- ""
   #        tS0.01 <- S0.01/(1+abs(S0.01))# transformed
   # names(tS0.01) <- ""
  #        tS0.25 <- S0.25/(1+abs(S0.25))# transformed
   # names(tS0.25) <- ""
           K0.01 <- (Cent[5]-Cent[1])/(Cent[4]-Cent[2])
    names(K0.01) <- ""
          sK0.01 <- K0.01/3.449 # standarised kurtosis
   names(sK0.01) <- ""
        ex.K0.01 <- K0.01-3.449 # excess kurtosis fro plots
 names(ex.K0.01) <- ""
        tr.K0.01 <- ex.K0.01/(1+abs(ex.K0.01))
 names(tr.K0.01) <- ""
  list(      S0.25 = S0.25,
             S0.01 = S0.01,
       # trans.S0.25 = tS0.25,
       # trans.S0.01 = tS0.01,
             K0.01 = K0.01,
        standK0.01 = sK0.01,
         exc.K0.01 = ex.K0.01,  # excess
       trans.K0.01 = tr.K0.01)
}
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
# FUNCTION 3 
#--------------------------------------------------------------
# SAMPLE centile skewness 
centileSkew <- function(x, cent=1, weights=NULL)
 {
# local function -----------------------------------------
quantileW <- function (y, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1)) 
 {
  if (!length(weights)) 
    return(quantile(y, probs = probs))
  if (any(probs < 0 | probs > 1)) 
    stop("Probabilities must be between 0 and 1 inclusive")
     i <- is.na(weights) | weights == 0
  if (any(i)) 
     {
         y <- y[!i]
   weights <- weights[!i]
     }
     ysort <- unique(sort(y))
  weights1 <- tapply(weights, y, sum)
      cumu <- cumsum(weights1)
         x <- ysort
       wts <- weights1
         n <- sum(wts)
     order <- 1 + (n - 1) * probs
       low <- pmax(floor(order), 1)
      high <- pmin(low + 1, n)
     order <- order%%1
      allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant", 
                    f = 1, rule = 2)$y
         k <- length(probs)
 quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
      nams <- paste(format(round(probs * 100, 
                                if (length(probs) > 1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")
     names(quantiles) <- nams
     return(quantiles)  
  }
#-------------------------------------------------------------
# the main function starts here   
#-------------------------------------------------------------   
  if (cent > 50) stop("cent should be less than 50") 
  if (!length(weights)) weights <- rep(1, length(x))
  if (any(is.na(x)))  warning("NA's will be removed from x")  
  if (!length(weights)) weights <- rep(1, length(x))
     i <- is.na(weights) | weights == 0 | is.na(x) 
  if (any(i)) 
   {    x <- x[!i]
  weights <- weights[!i]
   }
   quant <- quantileW(x,  probs=c(cent/100, 0.25, 0.5, 0.75, 1-(cent/100)),
                      weights=weights)
#   #eval(parse(text=eval(paste(paste0("S",cent[1]/100), "<-((quant[1]+quant[5])/2 -quant[3])/((quant[5]-quant[1])/2)"))))
   Sp<- ((quant[1]+quant[5])/2 -quant[3])/((quant[5]-quant[1])/2)
   names(Sp ) <- ""
   tSp <-  Sp/(1+abs(Sp))
   names(tSp) <- ""        
#   Kp <- (quant[5]-quant[1])/(quant[4]-quant[2])
#   names(Kp) <- ""       
#   sKp <- Kp/3.449 # standarised kurtosis
#   names(sKp) <- ""   
#   ex.Kp <- Kp-3.449 # excess kurtosis for plots
#   names(ex.Kp) <- ""   
#   tKp <- ex.Kp/(1+abs(ex.Kp))
#   names(tKp) <- ""           
   list(    p = cent/100,
           Sp = Sp) 
#          tSp = tSp)
#           Kp = Kp,
#          sKp = sKp,
#        ex.Kp = ex.Kp,  # excess 
#         teKp = tKp)
}
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
# FUNCTION 4 
#--------------------------------------------------------------
#--------------------------------------------------------------
# SAMPLE centile  kurtosis
centileKurt <- function(x, cent=1, weights=NULL)
 {
# local function -----------------------------------------
quantileW <- function (y, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1)) 
 {
    if (!length(weights)) 
      return(quantile(y, probs = probs))
    if (any(probs < 0 | probs > 1)) 
      stop("Probabilities must be between 0 and 1 inclusive")
    i <- is.na(weights) | weights == 0
    if (any(i)) 
    {
        y <- y[!i]
  weights <- weights[!i]
    }
    ysort <- unique(sort(y))
 weights1 <- tapply(weights, y, sum)
     cumu <- cumsum(weights1)
        x <- ysort
      wts <- weights1
        n <- sum(wts)
    order <- 1 + (n - 1) * probs
      low <- pmax(floor(order), 1)
     high <- pmin(low + 1, n)
    order <- order%%1
     allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant", 
                   f = 1, rule = 2)$y
        k <- length(probs)
quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
     nams <- paste(format(round(probs * 100, 
               if (length(probs) > 1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")
    names(quantiles) <- nams
    return(quantiles)  
}
#-------------------------------------------------------------
# the main function starts here   
#-------------------------------------------------------------   
if (cent > 50) stop("cent should be less than 50") 
if (!length(weights)) weights <- rep(1, length(x))
if (any(is.na(x)))  warning("NA's will be removed from x")  
if (!length(weights)) weights <- rep(1, length(x))
    i <- is.na(weights) | weights == 0 | is.na(x) 
if (any(i)) 
{      x <- x[!i]
nweights <- weights[!i]
}
   quant <- quantileW(x,  probs=c(cent/100, 0.25, 0.5, 0.75, 1-(cent/100)), 
                      weights=weights)
   #   #eval(parse(text=eval(paste(paste0("S",cent[1]/100), "<-((quant[1]+quant[5])/2 -quant[3])/((quant[5]-quant[1])/2)"))))
#   Sp<- ((quant[1]+quant[5])/2 -quant[3])/((quant[5]-quant[1])/2)
#   names(Sp ) <- ""
#   tSp <-  Sp/(1+abs(Sp))
#   names(tSp) <- ""        
     Kp <- (quant[5]-quant[1])/(quant[4]-quant[2])
      names(Kp) <- ""       
      sKp <- Kp/3.449 # standarised kurtosis
      names(sKp) <- ""   
      ex.Kp <- Kp-3.449 # excess kurtosis for plots
      names(ex.Kp) <- ""   
      tKp <- ex.Kp/(1+abs(ex.Kp))
      names(tKp) <- ""           
   list(    p = cent/100,
              Kp = Kp,
             sKp = sKp,
           ex.Kp = ex.Kp,  # excess 
            teKp = tKp)
 }
 #-------------------------------------------------------------- 
# Y <- rSST(1000, nu=.5, tau=5)
# centileSK(Y)
#  centileSkew(Y)
# centileKurt(Y,25 )
#--------------------------------------------------------------
#-----------------------------------------------------------------
# FUNCTION 5 
#-----------------------------------------------------------------
# Theoretical centile skewness + kurtosis
theoCentileSK <- function(fam="NO", p=0.01, ...)
{
    fam <- as.gamlss.family(fam)
  fname <- fam$family[[1]]
   qfun <- paste("q", fname, sep = "")
 invcdf <- eval(parse(text=qfun))
     IR <- invcdf(.75,...)-invcdf(.25,...)
    SIR <- IR/2
 Galton <- ((invcdf(.75,...)+invcdf(.25,...))/2-invcdf(.5,...))/SIR
     Sp <- ((invcdf(p,...)+invcdf(1-p,...))/2-invcdf(.5,...))/
             ((invcdf(1-p,...)-invcdf(p,...))/2)
     Kp <- (invcdf(1-p,...)-invcdf(p,...))/(invcdf(.75,...)-invcdf(.25,...))
    sKp <- Kp/((qNO(.99)-qNO(.01))/(qNO(.75)-qNO(.25)))
  list(IR=IR, SIR=SIR, S_0.25=Galton, S_0.01=Sp, K_0.01=Kp, sK_0.01=sKp)
}
#--------------------------------------------------------------  
# theoCentileSK("NO")
# theoCentileSK(TF, nu=5)
# theoCentileSK("EXP")
# theoCentileSK("ZAGA")
#-----------------------------------------------------------------
# FUNCTION 6 
#-----------------------------------------------------------------
# plotting skewness or kurtosis as function of p
#--------------------------------------------------------------
plotCentileSK<-function(fam="NO", 
                  plotting = c("skew", "kurt", "standKurt"),
                       add = FALSE, col = 1, lty = 1,  lwd = 1, 
                        ylim=NULL, ...)
{
  plotting <- match.arg(plotting) 
        p1 <- seq(0.001,.499, length=201)
       RES <- theoCentileSK(fam=fam, p=p1, ...) 
   if (plotting=="skew")
   {
     if (add)  lines(RES[["S_0.01"]]~p1, col=col, lty=lty,  lwd=lwd)   
       else
         plot(RES[["S_0.01"]]~p1, type="l", xlab="p", ylab=expression(s[p]), 
              col=col, lty=lty, ylim=ylim, lwd=lwd)   
   }
   
   if (plotting=="kurt")
   {
     if (add)  lines(RES[["K_0.01"]]~p1, col=col, lty=lty, lwd=lwd)   
     else
       plot(RES[["K_0.01"]]~p1, type="l", xlab="p", ylab=expression(k[p]),
            col=col, lty=lty, ylim=ylim, lwd=lwd)   
   }
   if (plotting=="standKurt")
   {
       if (add)  lines(RES[["K_0.01"]]~p1, col=col, lty=lty, lwd=lwd)   
       else
         plot(RES[["sK_0.01"]]~p1, type="l", xlab="p", ylab="kutrosis", 
              col=col, lty=lty, ylim=ylim, lwd=lwd)   
   }
}
#--------------------------------------------------------------
# plotCentileSK(EXP)
# plotCentileSK(IG, add=T, col=2)
# plotCentileSK(NO, "skew")
# plotCentileSK(NO, "kurt")
# plotCentileSK(TF, "kurt", nu=5, add=T, col=2)
# plotCentileSK(NO, "standKurt", ylim=c(0,3))
# plotCentileSK(TF, "standKurt", nu=5, add=T, col=2)
#--------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# FUNCTION 7 
#-----------------------------------------------------------------
# loading the functions
#    bountary() fEGB1_1 fEGB1_2 fJSU fSEP3 fSHASHo  fST3_1  fST3_2
# .onAttach <- function(...)
# {
#   load(system.file("doc", "MomentSkewKurt1.RData", package="gamlss.dist"))
# }

#load(system.file("doc", "MomentSkewKurt1.RData", package="gamlss.dist"))
# gamlssNews <- function() file.show(system.file("doc", "NEWS.txt", package="gamlss"))# Monday, September 5, 2005 MS
# #------------------------------------------------------------------
# potting moment SK
SKmoment_col <- function()
{
  boundary <- function(lty = 1, lwd = 2, col = 1){}
   fEGB2_1 <- function(x, deriv = 0L){}
   fEGB2_2 <- function(v){}
      fJSU <- function(x, deriv = 0L){}
     fSEP3 <- function(x, deriv = 0L){}
    fST3_1 <- function(x, deriv = 0L){}
    fST3_2 <- function(x, deriv = 0L){}
   fSHASHo <- function(x, deriv = 0L){}
 load(system.file("doc", "MomentSkewKurt1.RData", package="gamlss.dist"))
  curve(fJSU, ylim=c(-1,1), xlab="transformed moment skewness",
        ylab="transformed moment kurtosis",
                                 lty=2, col="red",  lwd=2)
  curve(fSHASHo, 0.001,1, add=T, lty=3, col="orange", lwd=2)
  curve(fSEP3, 0.01,1,    add=T, lty=4, col="brown", lwd=2)
  curve(fST3_2, 0.5,1,    add=T, lty=5, col="blue", lwd=2)
  curve(fST3_1, 0,.49,    add=T, lty=5, col="blue", lwd=2)
  curve(fEGB2_1, 0, .6666,  add=T, lty=6, col="olivedrab", lwd=2)
  curve(fEGB2_2 , 0,.6666,  add=T, lty=6, col="olivedrab", lwd=2)
  boundary()
  lines(c(0,1), c(1,1), lwd=2)
  lines(c(0,0), c(-0.6666,1), lwd=2)
  points(0,0, lwd=2)
  legend("bottomright",
                   legend=c("JSU","SHASHo","SEP3", "ST3", "EGB2",  "all distributions"),
                     col=c("red","orange", "brown", "blue","olivedrab","black"),
                     lty=c(2,3,4,5,6,1), lwd=2 )
}
# #------------------------------------------------------------------
# # 
# # gamlss.dist:::SKmoment_col()
# #------------------------------------------------------------------
# #------------------------------------------------------------------
# #-----------------------------------------------------------------
# # FUNCTION 8 
# #-----------------------------------------------------------------
SKmoment_gray <- function()
{
  boundary <- function(lty = 1, lwd = 2, col = 1){}
   fEGB2_1 <- function(x, deriv = 0L){}
   fEGB2_2 <- function(v){}
      fJSU <- function(x, deriv = 0L){}
     fSEP3 <- function(x, deriv = 0L){}
    fST3_1 <- function(x, deriv = 0L){}
    fST3_2 <- function(x, deriv = 0L){}
   fSHASHo <- function(x, deriv = 0L){}
load(system.file("doc", "MomentSkewKurt1.RData", package="gamlss.dist"))
  curve(fJSU, ylim=c(-1,1), xlab="transformed moment skewness",
        ylab="transformed moment kurtosis",
        lty=2, col=gray(.7),  lwd=2)
  curve(fSHASHo, 0.001,1, add=T, lty=3, col=gray(.4), lwd=2)
  curve(fSEP3, 0.01,1,    add=T, lty=4, col=gray(.6), lwd=2)
  curve(fST3_2, 0.5,1,    add=T, lty=5, col=gray(.8), lwd=2)
  curve(fST3_1, 0,.49,    add=T, lty=5, col=gray(.8), lwd=2)
  curve(fEGB2_1, 0, .6666,  add=T, lty=6, col=gray(.5), lwd=2)
  curve(fEGB2_2 , 0,.6666,  add=T, lty=6, col=gray(.5), lwd=2)
  boundary()
  lines(c(0,1), c(1,1), lwd=2)
  lines(c(0,0), c(-0.6666,1), lwd=2)
  points(0,0, lwd=2)
  legend("bottomright",
         legend=c("JSU","SHASHo","SEP3", "ST3", "EGB2",    "all distributions"),
         col=c(gray(.7), gray(.4), gray(.6), gray(.8), gray(.5), gray(0)),
         lty=c(2,3,4,5,6,1), lwd=2, cex=.9 )
}
# #------------------------------------------------------------------
# # use 
# gamlss.dist:::SKmoment_gray()
#-----------------------------------------------------------------
# FUNCTION 9
#-----------------------------------------------------------------
#------------------------------------------------------------------
SKmomentBoth <- function(...,show.legend=TRUE)
{
  boundary <- function(lty = 1, lwd = 2, col = 1){}
  fEGB2_1 <- function(x, deriv = 0L){}
  fEGB2_2 <- function(v){}
  fJSU <- function(x, deriv = 0L){}
  fSEP3 <- function(x, deriv = 0L){}
  fST3_1 <- function(x, deriv = 0L){}
  fST3_2 <- function(x, deriv = 0L){}
  fSHASHo <- function(x, deriv = 0L){}
  load(system.file("doc", "MomentSkewKurt1.RData", package="gamlss.dist"))  
  # logNO-JSU
  curve(fJSU, 0,1, ylim=c(-1,1), xlim=c(-1,1), xlab="transformed moment skewness",
        ylab="transformed moment kurtosis", col=gray(.7), lwd=2, lty=2)
  flipfJSU <- function(x) fJSU(-x)
  curve( flipfJSU, -1,0, add=TRUE, col=gray(.7), lwd=2, lty=2)
  # SHASHo
  curve(fSHASHo, 0.001,1, add=T, lty=3, col=gray(.4), lwd=2)
  flipfSHASHo <- function(x) fSHASHo(-x)
  curve( flipfSHASHo, -1,-0.001, add=TRUE, lty=3, col=gray(.4), lwd=2)
  # SEP3
  curve(fSEP3, 0.01,1, add=T, lty=4, col=gray(.6), lwd=2)
  flipfSEP3 <- function(x) fSEP3(-x)
  curve(flipfSEP3, -1,-0.01, add=T, lty=4, col=gray(.6), lwd=2)
  # ST3
  curve(fST3_2, 0.5,1, add=T, lty=5, col=gray(.8), lwd=2)
  flipfST3_2 <- function(x) fST3_2(-x)
  curve(flipfST3_2, -1,-0.5, add=T, lty=5, col=gray(.8), lwd=2)
  curve(fST3_1, 0,.49, add=T, lty=5, col=gray(.8), lwd=2)
  flipfST3_1 <- function(x) fST3_1(-x)
  curve(flipfST3_1, -0.49,0, add=T, lty=5, col=gray(.8), lwd=2)
  # EGB2
  curve(fEGB2_1, 0, .6666, add=T, lty=6, col=gray(.5), lwd=2)
  flipfEGB2_1 <- function(x) fEGB2_1(-x)
  curve(flipfEGB2_1, -0.6666, 0, add=T, lty=6, col=gray(.5), lwd=2)
  curve(fEGB2_2 , 0.0, .6666, add=T, lty=6, col=gray(.5), lwd=2)
  flipfEGB2_2 <- function(x) fEGB2_2(-x)
  curve(flipfEGB2_2, -0.6666, 0, add=T, lty=6, col=gray(.5), lwd=2)
  # local
  boundary()
  # local function
  boundaryR<-function()
  {
    tskew<-seq(0,0.99999,length=101)
   # tskew<-seq(-0.99999,0,length=1000)
    skew <- tskew/(1-tskew)
    kurt <- 1 + (skew^2)
    ekurt <- kurt - 3
    tkurt <- ekurt/(1+abs(ekurt))
    lines(tkurt~I(-tskew), type="l", lty=1, lwd=2, col=1)
  }
  boundaryR()
  grid()
  lines(c(-1,1), c(1,1), lwd=2)
  if (show.legend)
  {
    legend("bottomright",
           legend=c("JSU","SHASHo","SEP3", "ST3", "EGB2", "all distributions"),
           col=c(gray(.7), gray(.4), gray(.6), gray(.8), gray(.5), gray(0)),
           lty=c(2,3,4,5,6,1), lwd=2, cex=.8 )
  }

}
# #-------------------------------------------------------------------
# gamlss.dist:::SKmomentBoth()
# #------------------------------------------------------------------
# #-----------------------------------------------------------------
# # FUNCTION 10 
# #-----------------------------------------------------------------
#load(file="/Users/MikisStasinopoulos/Dropbox/gamlss/library/gamlss.dist1/inst/doc/CentileSkewKurt.RData")
# library(gamlss)

# #------------------------------------------------------------------
SKcentile_col <- function( type=c("central", "tail"))
{
# dummy functions
  cEGB2_1 <- cEGB2_2 <-tEGB2_1 <-tEGB2_2 <- cJSU <-  tJSU <- tST3_1 <- tST3_2 <-function (x, deriv = 0L) {}
  cSB <- tSB <- cSEP3  <- tSEP3 <- cSHASH <- tSHASH <- cST3_1 <- cST3_2 <- function (x, deriv = 0L) {}
  cEGB2_1_data <- cEGB2_2_data <- data.frame( cskew=0, ckurt=0)
load(system.file("doc", "CentileSkewKurt.RData", package="gamlss.dist"))   
  type <- match.arg(type)
  if (type=='central')
  {
    curve(cSEP3, 0.01,.99,  ylim=c(-1,1), xlab="transformed centile skewness",
          ylab="transformed centle kurtosis",
          lty=4, col="brown", lwd=2)
    curve(cST3_2, 0.1445, 1, add=T, lty=5, col="blue", lwd=2)
    curve(cST3_1, 0, 0.1441, add=T, lty=5, col="blue", lwd=2)
    curve(cJSU, 0.01, .99, add=T, lty=2, col="red",  lwd=2)
    curve(cSHASH, 0, .965, add=T, lty=3, col="orange", lwd=2)
    lines(cEGB2_1_data$cskew,cEGB2_1_data$ckurt,  lty=6, col="olivedrab", lwd=2)
    lines(cEGB2_2_data$cskew[c(-1,-2)],cEGB2_2_data$ckurt[c(-1,-2)],  lty=6, col="olivedrab", lwd=2) 
    #lines(cEGB2_1Data$cskew,cEGB2_1Data$ckurt,  lty=6, col="olivedrab", lwd=2)
    #curve(cEGB2_1, 0,  0.26, add=T,lty=6, col="olivedrab", lwd=2)
    #curve(cEGB2_2, 0,  0.258, add=T,lty=6, col="olivedrab", lwd=2)
    curve(cSB, 0, 0.99, add=T, lty=7, col="darkgreen", lwd=2)
    lines(c(0,1), c(1,1), lwd=2)
    lines(c(0,0), c(-0.56,1), lwd=2)
  } else
  {
  curve(tSEP3, 0.01,.99,  ylim=c(-1,1), xlab="transformed centile skewness",
        ylab="transformed centle kurtosis",
        lty=4, col="brown", lwd=2)
  curve(tST3_2, 0.484, 1, add=T, lty=5, col="blue", lwd=2)
  curve(tST3_1,  0, 0.482, add=T, lty=5, col="blue", lwd=2)
  curve(tJSU, 0.01, 0.99, add=T, lty=2, col="red",  lwd=2)
  curve(tSHASH, 0, .996, add=T, lty=3, col="orange", lwd=2)
  curve(tJSU, 0.01, 0.99, add=T, lty=2, col="red",  lwd=2)
  curve(tEGB2_1,  0,  .70,  add=T,lty=6, col="olivedrab", lwd=2)
  curve(tEGB2_2, 0,  0.70, add=T,lty=6, col="olivedrab", lwd=2)
  curve(tSB, 0,1, add=T, lty=7, col="darkgreen", lwd=2)
  lines(c(0,1), c(1,1), lwd=2)
  lines(c(0,0), c(-0.6,1), lwd=2)
  points(0,0, lwd=2)
  }
  legend("bottomright",
         legend=c("JSU","SHASHo","SEP3", "ST3", "EGB2",  "SB"),
         col=c("red","orange", "brown", "blue","olivedrab", "darkgreen"),
         lty=c(2,3,4,5,6,7), lwd=2 )
}
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# # gamlss.dist:::SKcentile_col("central")
# # gamlss.dist:::SKcentile_col("tail")
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# #-----------------------------------------------------------------
# # FUNCTION 11 
# #-----------------------------------------------------------------
SKcentile_gray <- function( type=c("central", "tail"))
{
# dummy functions
  cEGB2_1 <- cEGB2_2 <-tEGB2_1 <-tEGB2_2 <- cJSU <-  tJSU <- tST3_1 <- tST3_2 <-function (x, deriv = 0L) {}
  cSB <- tSB <- cSEP3  <- tSEP3 <- cSHASH <- tSHASH <- cST3_1 <- cST3_2 <- function (x, deriv = 0L) {}
  cEGB2_1_data <- cEGB2_2_data <- data.frame( cskew=0, ckurt=0)
load(system.file("doc", "CentileSkewKurt.RData", package="gamlss.dist"))   
  type <- match.arg(type)
  if (type=='central')
  {
    curve(cSEP3, 0.01,.99,  ylim=c(-1,1), xlab="central centile skewness",
          ylab="transformed centle kurtosis",
          lty=4, col=gray(.6), lwd=2)
    curve(cST3_2, 0.1445, 1, add=T, lty=5, col=gray(.8), lwd=2)
    curve(cST3_1, 0, 0.1441, add=T, lty=5, col=gray(.8), lwd=2)
    curve(cJSU, 0.01, .99, add=T, lty=2, col=gray(.7),  lwd=2)
    curve(cSHASH, 0, .965, add=T, lty=3, col=gray(.4), lwd=2)
    lines(cEGB2_1_data$cskew,cEGB2_1_data$ckurt,  lty=6, col=gray(.5), lwd=2)
    lines(cEGB2_2_data$cskew[c(-1,-2)],cEGB2_2_data$ckurt[c(-1,-2)],  lty=6, col=gray(.5), lwd=2) 
    curve(cSB, 0, 0.99, add=T, lty=7, col=gray(.9), lwd=2)
    lines(c(0,1), c(1,1), lwd=2)
    lines(c(0,0), c(-0.56,1), lwd=2)
  }  else
  {
  curve(tSEP3, 0.01,.99,  ylim=c(-1,1), xlab="tail centile skewness",
        ylab="transformed centle kurtosis",
        lty=4, col=gray(.6), lwd=2)
  curve(tST3_2, 0.484, 1, add=T, lty=5, col=gray(.8), lwd=2)
  curve(tST3_1,  0, 0.482, add=T, lty=5, col=gray(.8), lwd=2)
  curve(tJSU, 0.01 ,0.99, add=T, lty=2, col=gray(.7),  lwd=2)
  curve(tSHASH, 0, .996, add=T, lty=3, col=gray(.4), lwd=2)
  curve(tEGB2_1, 0,  .70, add=T, lty=6, col=gray(.5), lwd=2)
  curve(tEGB2_2, 0,  0.70, add=T, lty=6, col=gray(.5), lwd=2)
  curve(tSB, 0,1, add=T, lty=7, col=gray(.9), lwd=2)
  lines(c(0,1), c(1,1), lwd=2)
  lines(c(0,0), c(-0.6,1), lwd=2)
  points(0,0, lwd=2)
  }
  legend("bottomright",
         legend=c("JSU","SHASHo","SEP3", "ST3", "EGB2",    "SB"),
         col=c(gray(.7), gray(.4), gray(.6), gray(.8), gray(.5), gray(.9)),
         lty=c(2,3,4,5,6,7), lwd=2, cex=.9 )

}
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# # gamlss.dist:::SKcentile_gray("central")
# # gamlss.dist:::SKcentile_gray("tail")
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# # FUNCTION 12 
# #--------------------------------------------------------------
SKcentileBoth <- function(type=c("central", "tail"), show.legend=TRUE)
{
# dummy functions
  cEGB2_1 <- cEGB2_2 <-tEGB2_1 <-tEGB2_2 <- cJSU <-  tJSU <- tST3_1 <- tST3_2 <-function (x, deriv = 0L) {}
  cSB <- tSB <- cSEP3  <- tSEP3 <- cSHASH <- tSHASH <- cST3_1 <- cST3_2 <- function (x, deriv = 0L) {}
  cEGB2_1_data <- cEGB2_2_data <- data.frame( cskew=0, ckurt=0)
load(system.file("doc", "CentileSkewKurt.RData", package="gamlss.dist"))     
  type <- match.arg(type)
  if (type=='central')
  {
    curve(cSEP3, 0.01,.99,  ylim=c(-1,1),  xlim=c(-1,1), xlab="central centile skewness",
          ylab="transformed centle kurtosis",
          lty=4, col=gray(.6), lwd=2)
    flipcSEP3 <- function(x) cSEP3(-x)
    curve( flipcSEP3, -1,-0.01, add=TRUE, lty=4, col=gray(.6), lwd=2)
    curve(cST3_2, 0.1445, 1, add=T, lty=5, col=gray(.8), lwd=2)
    curve(cST3_1, 0, 0.1441, add=T, lty=5, col=gray(.8), lwd=2)
    flipcST3_2 <- function(x) cST3_2(-x)
    curve(flipcST3_2, -1, -0.144, add=T, lty=5, col=gray(.8), lwd=2)
    flipcST3_1 <- function(x) cST3_1(-x)
    curve(flipcST3_1, -0.1441,0,  add=T, lty=5, col=gray(.8), lwd=2)
    curve(cJSU, 0.01, .99, add=T, lty=2, col=gray(.7),  lwd=2)
    flipcJSU <- function(x) cJSU(-x)
    curve(flipcJSU, -.99, -0.01, add=T, lty=2, col=gray(.7),  lwd=2)
    curve(cSHASH, 0, .965, add=T, lty=3, col=gray(.4), lwd=2)
    flipcSHASH <- function(x) cSHASH(-x)
    curve(flipcSHASH, -.965, 0, add=T, lty=3, col=gray(.4), lwd=2)
    #lines(cEGB2_1Data$cskew,cEGB2_1Data$ckurt,  lty=6, col=gray(.5), lwd=2)
    #lines(-cEGB2_1Data$cskew,cEGB2_1Data$ckurt,  lty=6, col=gray(.5), lwd=2)
    lines(cEGB2_1_data$cskew,cEGB2_1_data$ckurt,  lty=6, col=gray(.5), lwd=2)
    lines(-cEGB2_1_data$cskew,cEGB2_1_data$ckurt,  lty=6, col=gray(.5), lwd=2)
    lines(cEGB2_2_data$cskew[c(-1,-2)],cEGB2_2_data$ckurt[c(-1,-2)],  lty=6, col=gray(.5), lwd=2) 
    lines(-cEGB2_2_data$cskew[c(-1,-2)],cEGB2_2_data$ckurt[c(-1,-2)],  lty=6, col=gray(.5), lwd=2)
   # curve(cEGB2_2, 0,  0.26, add=T,lty=6, col=gray(.5), lwd=2)
   # flipcEGB2_2 <- function(x) cEGB2_2(-x)
   # curve(flipcEGB2_2, -0.26, 0, add=T,lty=6, col=gray(.5), lwd=2)
   # lines(c(-0.26,-0.26), c(0.5078402, 0.5754391), col=gray(.5), lty=6,  lwd=2)
    curve(cSB, 0, 0.99, add=T, lty=7, col=gray(.9), lwd=2)
    flipcSB <- function(x) cSB(-x)
    curve(flipcSB, -0.99, 0, add=T, lty=7, col=gray(.9), lwd=2)
    lines(c(-1,1), c(1,1), lwd=2)
    grid()
  }  else
  {
  curve(tSEP3, 0.01,.99,  ylim=c(-1,1),  xlim=c(-1,1), xlab="tail centile skewness",
        ylab="transformed centle kurtosis",
        lty=4, col=gray(.6), lwd=2)
  fliptSEP3 <- function(x) tSEP3(-x)
  curve( fliptSEP3, -1,-0.01, add=TRUE, lty=4, col=gray(.6), lwd=2)
  curve(tSHASH, 0, .996, add=T, lty=3, col=gray(.4), lwd=2)
  fliptSHASH <- function(x) tSHASH(-x)
  curve(fliptSHASH, -.996,0, add=T, lty=3, col=gray(.4), lwd=2)
  curve(tST3_2, 0.484, 1, add=T, lty=5, col=gray(.8), lwd=2)
  fliptST3_2 <- function(x) tST3_2(-x)
  curve(fliptST3_2, -1,  -0.484,  add=T, lty=5, col=gray(.8), lwd=2)
  curve(tST3_1,  0, 0.482, add=T, lty=5, col=gray(.8), lwd=2)
  fliptST3_1 <- function(x) tST3_1(-x)
  curve(fliptST3_1, -0.482, 0,  add=T, lty=5, col=gray(.8), lwd=2)
  curve(tJSU, 0.01 ,0.99, add=T, lty=2, col=gray(.7),  lwd=2)
  fliptJSU <- function(x) tJSU(-x)
  curve(fliptJSU,  -0.99, -0.01, add=T, lty=2, col=gray(.7),  lwd=2)
  curve(tEGB2_1, 0,  0.70, add=T, lty=6, col=gray(.5), lwd=2)
  fliptEGB2_1 <- function(x) tEGB2_1(-x)
  curve(fliptEGB2_1, -0.70, 0, add=T, lty=6, col=gray(.5), lwd=2)
  curve(tEGB2_2, 0,  0.70, add=T, lty=6, col=gray(.5), lwd=2)
  fliptEGB2_2 <- function(x) tEGB2_2(-x)
  curve(fliptEGB2_2, -0.70, 0, add=T, lty=6, col=gray(.5), lwd=2)
  curve(tSB, 0,1, add=T, lty=7, col=gray(.9), lwd=2)
  fliptSB <- function(x) tSB(-x)
  curve(fliptSB, -1,0, add=T, lty=7, col=gray(.9), lwd=2)
  grid()
  lines(c(-1,1), c(1,1), lwd=2)
  }
  if (show.legend)
  {
    legend("bottomright",
           legend=c("JSU","SHASHo","SEP3", "ST3", "EGB2",    "SB"),
           col=c(gray(.7), gray(.4), gray(.6), gray(.8), gray(.5), gray(.9)),
           lty=c(2,3,4,5,6,7), lwd=2, cex=.9 )
  }

}
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# # gamlss.dist:::SKcentileBoth(type="central")
# # gamlss.dist:::SKcentileBoth(type="tail")
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# FUNCTION 13
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
checkMomentSK <- function(x,
                weights = NULL,          
                    add = FALSE,
              bootstrap = TRUE,
           no.bootstrap = 99,
          col.bootstrap = "lightblue",
          pch.bootstrap =  21,
            asCharacter = TRUE,
              col.point = "black",
              pch.point = 4,
              lwd.point = 2,
           text.to.show = NULL,
              cex.text  = 1.5,
               col.text = "black",
            show.legend = TRUE
            )
{
#############################################################
# local functions here
  CI95 <- function(x)#
  {
    if (any(abs(x)>=1)) stop(" x should be in (-1,1)")
    gamma.1 <- x/(1-x)
    gamma.2 <- ( (24/n)*(qchisq(.95, df=2)-(n/6)*gamma.1^2))^0.5
    gamma.2t <- gamma.2/(1+abs(gamma.2))
    gamma.2t
  }
  CI95.2 <- function(x) CI95(-x)
  CI95.3 <- function(x) -CI95(-x)
  CI95.4 <- function(x) -CI95(x)
##############################################################################
# function stats here
# checking whether model ro get the residual otherwise data
X <-  if (is(x,"gamlss")) resid(x)
  else x
n <- length(X)
if (add==FALSE)
  {
  SKmomentBoth(show.legend=show.legend)
  abline(h=0, col="lightgray"); abline(v=0, col="lightgray")
  xx <-seq(0, 0.99, length=101)
  y1 <- CI95(xx)
  y2 <- CI95.2(-xx)
  y3 <- CI95.3(-xx)
  y4 <- CI95.4(xx)
  xy<-na.omit(data.frame(x=c(xx,xx[101:1],-xx,-xx[101:1]), y=c(y1,y4[101:1],y3,y2[101:1])))
  polygon(xy$x, xy$y, lty=11, col=gray(.97))
  }
#---------------------------------------------------------
#---------------------------------------------------------
if (bootstrap)
  {
    for (i in 1:no.bootstrap)
    {
      ind <- sample(n, n,  replace = TRUE)
     # x1 <- sample(X, replace = TRUE)
      sk <-  momentSK(X[ind], weights=weights[ind])
      points(sk$trans.mom.skew, sk$trans.mom.kurt,  pch=pch.bootstrap,
             col=col.bootstrap)
    }
  }
   sk <-  momentSK(X, weights=weights)
   if (asCharacter)
   {
    if (is.null(text.to.show))
      text(sk$trans.mom.skew, sk$trans.mom.kurt, paste(substitute(x)),
           cex=cex.text, col=col.text)
     else 
       text(sk$trans.mom.skew, sk$trans.mom.kurt, text.to.show,
            cex=cex.text, col=col.text)
   }
   else points(sk$trans.mom.skew, sk$trans.mom.kurt, col=col.point, pch=pch.point,
               lwd=lwd.point)
invisible(list(skewness=sk$mom.skew, trans.skewness=sk$trans.mom.skew, 
               kurtosis=sk$mom.kurt, excess.kurt=sk$excess.mom.kurt,
               trans.kurtosis=sk$trans.mom.kurt, jarque.bera.test=sk$jarque.bera.test))
}
#--------------------------------------------------------------
#--------------------------------------------------------------
# # Y <- rSST(1000, nu=.5, tau=5)
# # checkMomentSK(Y)
# # Y <- rSST(1000, nu=1, tau=5)
# # checkMomentSK(Y)
# # Y <- rSST(1000, nu=1, tau=100)
# # checkMomentSK(Y)
# # Y <- rSST(1000, nu=1, tau=500)
# # checkMomentSK(Y)
# #--------------------------------------------------------------
# #--------------------------------------------------------------
# #-----------------------------------------------------------------
# # FUNCTION 14
# #-----------------------------------------------------------------
checkCentileSK <- function(x,
                weights = NULL,        
                   type = c("central", "tail"),
                    add = FALSE,
              bootstrap = TRUE,
           no.bootstrap = 99,
          col.bootstrap = "lightblue",
          pch.bootstrap =  21,
            asCharacter = TRUE,
              col.point = "black",
              pch.point = 4,
              lwd.point = 2,
           text.to.show = NULL,
              cex.text  = 1.5,
               col.text = "black",
            show.legend = TRUE
          )
{
  #############################################################
type <- match.arg(type)
   X <-  if (is(x,"gamlss")) resid(x)
        else x
   n <- length(X)
  if (add==FALSE) SKcentileBoth(type = type, show.legend=show.legend)
  abline(h=0, col="lightgray"); abline(v=0, col="lightgray")
  #---------------------------------------------------------
  #---------------------------------------------------------
  if (bootstrap)
  {
    for (i in 1:no.bootstrap)
    {
      ind <- sample(n, n,  replace = TRUE)
      # x1 <- sample(X, replace = TRUE)
      sk <-  centileSK(X[ind], weights=weights[ind])
     # x1 <- sample(X, replace = TRUE)
      #sk <-  centileSK(x1, weights=weights)
     if (type=="central")
     {
       points(sk$S0.25,  sk$trans.K0.01,  pch=pch.bootstrap, col=col.bootstrap)
     }
      else{
        points(sk$S0.01, sk$trans.K0.01,  pch=pch.bootstrap, col=col.bootstrap)
      }
    }
  }
    sk <-  centileSK(X, weights=weights)
  if (type=="central"){
    if (asCharacter)
     {
      if (is.null(text.to.show))
        text(sk$S0.25, sk$trans.K0.01, paste(substitute(x)),
             cex=cex.text, col=col.text)
      else 
        text(sk$S0.25, sk$trans.K0.01, text.to.show,
             cex=cex.text, col=col.text)
     }
    else points(sk$S0.25, sk$trans.K0.01, col=col.point, pch=pch.point,
                lwd=lwd.point)
  } else {
    
    if (asCharacter)
    {
      if (is.null(text.to.show))
        text(sk$S0.01, sk$trans.K0.01, paste(substitute(x)),
             cex=cex.text, col=col.text)
      else 
        text(sk$S0.01, sk$trans.K0.01, text.to.show,
             cex=cex.text, col=col.text)
    }
    else points(sk$S0.01,  sk$trans.K0.01, col=col.point, pch=pch.point,
                lwd=lwd.point)
  }
  invisible(list(skew0.25=sk$S0.25, skew0.01=sk$S0.01, tr.skew0.25=sk$S0.25, 
                 tr.skew0.01=sk$trans.S0.01, kurt0.01=sk$K0.01, tr.kurt0.01=sk$trans.K0.01))
}