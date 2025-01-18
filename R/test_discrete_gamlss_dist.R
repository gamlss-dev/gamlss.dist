################################################################################
# this functions test for discrete gamlss.family distributions
# the tests are as follows
# i) use the function checkUsage() to check is there are  undefined variables 
# ii) use the  the local test_range() function to check if the range od the function d.p.q is defined properly 
# iii) use the local function test_options_in_qfun() to check if the options in q-function are working 
# iv)  start a loop at different values of the distribution parameters (depending on the number of parameter)
#     and 
#    a) check whether the pdf's are summing up to 1
#    b) whether the cdf and the inverse cdf  (q-function) produce compatible results
#    c) whether the pdf and cdf are matching 
#    d) checking the tails of the distribution in in the `p` function  
#    e) creates data using the r function and fit constant models models using gamlss() and gamlss2() 
#    comparing the deviances 
#    g) if deferences occurred saves the different data for rechecking
################################################################################
################################################################################
################################################################################
################################################################################
# create by MS and BR at Tuesday, March 9, 2010 
# the for parameter is not tested
test_discrete_gamlss_dist<- function(family = "PO", 
                   y.range  = c(0,Inf), 
                  mu.range = c(0,Inf), 
               sigma.range = c(0,Inf),
                  nu.range = c(0,Inf),
                 tau.range = c(0,Inf), 
                    mu.val = c(.5,5,10,30), 
                 sigma.val = c(.5,5,10,30),
                    nu.val = c(.5,5,10,30),
                   tau.val = c(.5,5,10,30),
                         N = 100,
                        bd = NULL,
                      save = TRUE, 
                     trace = TRUE,
                      crit = 0.001,
                     ...  # for gamlss control parameters i.e. n.cyc=50 
                 )
{
################################################################################
## main function     
require(codetools)
require(gamlss)
################################################################################  
# local function
test_range <- function(family, lower, upper )
  {
    fam  <- as.gamlss.family(family)
    fname <- fam$family[[1]] 
    dfun <- paste0("d", fname)
    pfun <- paste0("p", fname)
    qfun <- paste0("q", fname)
    
    cat("-------------------------------------------", "\n")
    cat("-----------------START---------------------", "\n")
    cat("-------------------------------------------", "\n")
    cat("pdf", "\n")
    p <- rep(1,4)
    cat(eval(call(dfun, x=lower)),   "\n")
    cat(eval(call(dfun, x=lower-1)), "\n")
    cat(eval(call(dfun, x=upper)), "  \n")
    cat(eval(call(dfun, x=upper+1)), "\n")
    cat("------------------------------------------", "\n") 
    cat("cdf", "\n")
    cat(eval(call(pfun,q=lower)), "\n")
    cat(eval(call(pfun,q=lower-1)), "\n")
    cat(eval(call(pfun,q=upper)), "\n")
    cat(eval(call(pfun,q=upper+1)), "\n")
    cat("------------------------------------------", "\n")
    cat("q-function", "\n")
    cat(eval(call(qfun,p=0)), "\n")
    cat(eval(call(qfun,p=1)), "\n")
    cat(eval(call(qfun,p=0-1)), "\n")
    cat(eval(call(qfun,p=1+1)), "\n")  
    cat("-------------------------------------------", "\n")
    cat("--------------------END--------------------", "\n")
    cat("-------------------------------------------", "\n")
} 
################################################################################
test_range_bi <- function(family, lower, upper=bd, bd )
{
   fam  <- as.gamlss.family(family)
  fname <- fam$family[[1]] 
  dfun <- paste0("d", fname)
  pfun <- paste0("p", fname)
  qfun <- paste0("q", fname)
cat("-------------------------------------------", "\n")
cat("-----------------START---------------------", "\n")
cat("-------------------------------------------", "\n")
cat("pdf", "\n")
  p <- rep(1,4)
  cat(eval(call(dfun, x=lower, bd=bd)),   "\n")
  cat(eval(call(dfun, x=lower-1, bd=bd)), "\n")
  cat(eval(call(dfun, x=upper, bd=bd)), "  \n")
  cat(eval(call(dfun, x=upper+1, bd=bd)), "\n")
  cat("------------------------------------------", "\n") 
  cat("cdf", "\n")
  cat(eval(call(pfun,q=lower, bd=bd)), "\n")
  cat(eval(call(pfun,q=lower-1, bd=bd)), "\n")
  cat(eval(call(pfun,q=upper, bd=bd)), "\n")
  cat(eval(call(pfun,q=upper+1, bd=bd)), "\n")
  cat("------------------------------------------", "\n")
  cat("q-function", "\n")
  cat(eval(call(qfun,p=0, bd=bd)), "\n")
  cat(eval(call(qfun,p=1, bd=bd)), "\n")
  cat(eval(call(qfun,p=0-1, bd=bd)), "\n")
  cat(eval(call(qfun,p=1+1, bd=bd)), "\n")  
  cat("-------------------------------------------", "\n")
  cat("--------------------END--------------------", "\n")
  cat("-------------------------------------------", "\n")
}  
################################################################################  
test_options_in_qfun_bi <- function(family, bd)
{
   fam  <- as.gamlss.family(family)
  fname <- fam$family[[1]]
   qfun <- paste0("q", fname) 
  cat(eval(call(qfun,-1, log.p=TRUE, bd=bd)), "\n")
  cat(eval(call(qfun, exp(-1), bd=bd)), "\n")
  cat(eval(call(qfun, exp(-1), lower.tail=FALSE, bd=bd)), "\n")
}
################################################################################  
test_options_in_qfun <- function(family)
{
    fam  <- as.gamlss.family(family)
   fname <- fam$family[[1]]
    qfun <- paste0("q", fname) 
cat(eval(call(qfun,-1, log.p=TRUE)), "\n")
cat(eval(call(qfun, exp(-1))), "\n")
cat(eval(call(qfun, exp(-1), lower.tail=FALSE)), "\n")
}
################################################################################
################################################################################
################################################################################
################################################################################  
        mixed <- NULL
          fam <- as.gamlss.family(family) # family 
      distype <- fam$type 
if (distype!="Discrete") stop("The type of the distribution should be Discrete")
        fname <- fam$family[[1]] # family names
if (fname%in%gamlss:::.gamlss.bi.list)
          {
          if (is.null(bd)) stop("The family requires binomial denominator")
          bd <- bd
          }
no.active.par <- sum(as.vector(unlist(fam$parameters[1:fam$nopar])))
     family <- c("None", "None") 
     dorfun <- paste("d",fname,sep="") # say dNO
     porfun <- paste("p",fname,sep="") # say pNO
     qorfun <- paste("q",fname,sep="") # say qNO
     rorfun <- paste("r",fname,sep="") # say rN0
    thedata <- NULL #seq(1,length=N)
theParameters <- list()
theDiffdata <- NULL
     nfails <- 0
    nDfails <- 0
cat("****************************************************************************", "\n")
cat("************************* test for", paste(fname), "start here ***************************", "\n") 
cat("----- using the checkUsage() function ----", "\n")
checkUsage(eval(parse(text=fname)))
checkUsage(eval(parse(text=dorfun)))
checkUsage(eval(parse(text=porfun)))
checkUsage(eval(parse(text=qorfun)))
checkUsage(eval(parse(text=rorfun)))
cat("----- checkUsage() finished ----", "\n")
################################################################################
cat("****************************************************************************", "\n")
cat("----- start checking the range of the functions ----", "\n")
if (fname%in%gamlss:::.gamlss.bi.list) test_range_bi(BI, lower=y.range[1], bd=10)
else  test_range(fname, lower=y.range[1], upper=y.range[2])
cat("----- finish checking the range of the functions ----", "\n")
cat("****************************************************************************", "\n")
################################################################################
################################################################################
cat("----- start checking the option of the q-function ----", "\n")
if (fname%in%gamlss:::.gamlss.bi.list)  test_options_in_qfun_bi(fname, bd=bd)
else test_options_in_qfun(fname)
cat("----- finish checking the option of the q-function ----", "\n")
cat("****************************************************************************", "\n")
################################################################################
# the loop
################################################################################
################################################################################
################################################################################
## one parameters
################################################################################
################################################################################
if(no.active.par==1) # one parameter distributions
 {
  for (i in 1:length(mu.val))
  {
   if (trace==TRUE)
    { 
cat("****************************************************************************", "\n")
cat("testing at mu", mu.val[i], "\n")
    }
################################################################################
##  test whether it is a proper distribution summing up to 1
  intY <- if (fname%in%gamlss:::.gamlss.bi.list) 
              sum(eval(call(dorfun, x= y.range [1]:bd, mu=mu.val[i], bd=bd)))    
          else sum(eval(call(dorfun, x= y.range [1]:10000, mu=mu.val[i])))    
if (abs(intY-1)> 0.001) 
  warning( cat( "testing whether pdf sum up to 1: the summation do not add to 1", "at mu", mu.val[i], "\n"))
  else cat("The", fname,  "distribution at mu", mu.val[i], "sums up to one", "\n")   
################################################################################
## checking  whether the cdf and the inverse cdf  (q-function) are compatible 
   val <- if (fname%in%gamlss:::.gamlss.bi.list) eval(call(rorfun, n=100,   mu=mu.val[i], bd=bd))
          else eval(call(rorfun, n=100,   mu=mu.val[i])) # randomly generate a value
    pr <- if (fname%in%gamlss:::.gamlss.bi.list) eval(call(porfun, q=val, mu=mu.val[i], bd=bd))
          else eval(call(porfun, q=val, mu=mu.val[i])) #  get the cdf
    qq <-  if (fname%in%gamlss:::.gamlss.bi.list) eval(call(qorfun, p=pr,  mu=mu.val[i], bd=bd )) 
           else eval(call(qorfun, p=pr,  mu=mu.val[i])) 
if(any(abs(qq-val)>0.0001)) 
    warning( cat( "the p and q functions of", fnams, "are NOT all matching", "at mu", mu.val[i], "and y =",val,"\n"))  else  
      cat("The p and q function of distribution", fname, "at mu=", mu.val[i], "are compatible", "\n")
################################################################################
## checking whether pdf and cdf match 
  p1 <- if (fname%in%gamlss:::.gamlss.bi.list) 
           sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i], bd=bd)))  
        else sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i])))     
  aa <-  if (fname%in%gamlss:::.gamlss.bi.list) p1- eval(call(porfun, q=val[1], mu=mu.val[i], bd=bd))
          else p1-eval(call(porfun, q=val[1], mu=mu.val[i])) #pNO(val)
if(abs(aa)>0.0001)
   warning( cat( "the value of d and p functions do NOT match", "at mu", mu.val[i], "\n")) 
   else cat("The d and p values of distribution", fname, "at mu=" , mu.val[i], "are OK", "\n")  
################################################################################ 
## checking the tails in p function 
   tt <- if (fname%in%gamlss:::.gamlss.bi.list) 
        { 
        eval(call(porfun, q=val, mu=mu.val[i], bd=bd, lower.tail=TRUE))+
        eval(call(porfun, q=val, mu=mu.val[i], bd=bd, lower.tail=FALSE))
        }
         else
         { 
        eval(call(porfun, q=val, mu=mu.val[i], lower.tail=TRUE))+
        eval(call(porfun, q=val, mu=mu.val[i], lower.tail=FALSE))
        }
   if(any(abs(tt-1)>0.0001))
warning( cat( "for the value", val, "the tails of p functions do not add to 1", "at mu", mu.val[i], "\n")) 
  else
  cat("The d and p values of distribution", fname, "at mu=" , mu.val[i], "are OK", "\n") 
################################################################################
## fitting data 
## create data set in the  global environment 
   dat <<-dat <- if (fname%in%gamlss:::.gamlss.bi.list) eval(call(rorfun, N ,mu=mu.val[i], bd=bd))
                 else eval(call(rorfun, N ,mu=mu.val[i])) #create data
if (trace==TRUE) cat("--fitting models-- \n")
   m1 <- try(if (fname%in%gamlss:::.gamlss.bi.list)  
                   gamlss(cbind(dat, bd-dat)~1, family=fam, trace=FALSE, ...)
              else gamlss(dat~1, family=fam, trace=FALSE, n.cyc=100))  
    if (any(class(m1)%in%"try-error")||any(is.na(deviance(m1))))
         { 
         warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "\n"))
    }
cat("gamlss: ", deviance(m1), "\n")
   m2 <- try(if (fname%in%gamlss:::.gamlss.bi.list) 
         gamlss2(cbind(dat, bd-dat)~1, family=fam, trace=FALSE, ...)
    else gamlss2(dat~1, family=fam, trace=FALSE, ...))
cat("gamlss2:", deviance(m2), "\n")
if (any(class(m2)%in%"try-error")||any(is.na(deviance(m2))))
         { 
         warning(paste("gamlss2 failed", "at mu", mu.val[i], "\n"))
      }
if (any(class(m2)%in%"try-error")||any(is.na(deviance(m2)))||any(class(m1)%in%"try-error")||any(is.na(deviance(m1))))
         {
                thedata <- cbind(thedata,dat)  
                 nfails <- nfails+1
theParameters[[nfails]] <-  mu.val[i]  
  cat("Currrent No. of model fitting fails: ", nfails, "\n")
if (trace==FALSE) cat("---------------------------------------------------------- \n") 
         }
if (!any(class(m1)%in%"try-error") & !any(class(m2)%in%"try-error"))
         {
                     dd <- deviance(m1)-deviance(m2)
if(abs(dd)>crit)
        { 
warning( cat("The deviances for gamlss and gamlss2 do not agree at mu", mu.val[i], "with a difference", dd, "\n"))  
              theDiffdata <- cbind(theDiffdata,dat)
                  nDfails <- nDfails+1
             } 
         }
    }
 }
################################################################################
################################################################################
## two parameters
################################################################################
################################################################################
if(no.active.par==2) # two parameter distributions
 {
 for (i in 1:length(mu.val))
   {
   for (j in 1:length(sigma.val))
     {
     if (trace==TRUE)
      {
cat("-------------------------------------------------------------------------- \n")
cat("testing at mu", mu.val[i], "sigma", sigma.val[j], "\n")
      }
################################################################################
## test whether a proper distribution summing up to 1
intY <- if (fname%in%gamlss:::.gamlss.bi.list) 
      sum(eval(call(dorfun, x= c(y.range [1]:y.range[2]), mu=mu.val[i],  sigma=sigma.val[j], bd=bd))) 
        else sum(eval(call(dorfun, x= y.range [1]:10000, mu=mu.val[i],  sigma=sigma.val[j])))    
if (abs(intY-1)> 0.001) 
warning( cat( "The summation do NOT add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "\n"))
else 
  cat("The", fname,"distribution at mu", mu.val[i], "and sigma",  sigma.val[i],"sums up to one", "\n") 
################################################################################
## checking the whether the cdf and the inverse cdf match
val <- if (fname%in%gamlss:::.gamlss.bi.list) eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j], bd=bd)) 
       else eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j])) # randomly generate a value
 pr <- if (fname%in%gamlss:::.gamlss.bi.list) eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], bd=bd))
       else  eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j]))#  get the cdf
qq <-  if (fname%in%gamlss:::.gamlss.bi.list) eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], bd=bd)) 
       else eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j])) 
if(abs(qq-val)>0.0001) 
  {
   warning( cat( "the p and q functions are not matching", "at mu", mu.val[i], "and sigma", sigma.val[j], "and y =",val, "\n"))
  } else 
  cat("The p and q function of distribution", fname, "at mu=", mu.val[i],"and sigma=",  sigma.val[i], "are compatible", "\n")     
################################################################################
## checking whether pdf and cdf match 
    p1 <- if (fname%in%gamlss:::.gamlss.bi.list) 
                sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i], sigma=sigma.val[j], bd=bd))) 
           else sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i], sigma=sigma.val[j])))     
    aa <- if (fname%in%gamlss:::.gamlss.bi.list) 
                 p1-eval(call(porfun, q=val[1], mu=mu.val[i], sigma=sigma.val[j], bd=bd))
          else   p1- eval(call(porfun, q=val[1], mu=mu.val[i], sigma=sigma.val[j])) #pNO(val)
   if(abs(aa)>0.0001)
   warning( cat( "for the value", val, "the d and p functions do not match", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n")) else 
       cat("The d and p values of distribution", fname, "at mu=", mu.val[i],"and sigma=", sigma.val[i], "are OK", "\n")     
################################################################################ 
## checking whether tails in p function 
tt <- if (fname%in%gamlss:::.gamlss.bi.list) 
      {
      eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], bd=bd, lower.tail=TRUE))+
      eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], bd=bd, lower.tail=FALSE))
      }
      else 
      { 
     eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], lower.tail=TRUE))+
      eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], lower.tail=FALSE))
      }
if(abs(tt-1)>0.0001)
   warning( cat( "for the value", val, "the tails of p functions do not add to 1", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))   else
  cat("The tails of", fname, "at mu=", mu.val[i],"and sigma=",sigma.val[i], "are OK", "\n")
################################################################################
## fitting data 
## create data set in the  global environment  
dat <<- dat <- if (fname%in%gamlss:::.gamlss.bi.list) 
                  eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j], bd=bd))
             else eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j])) #create data
if (trace==TRUE) cat("--fitting models-- \n")
m1 <- try( if (fname%in%gamlss:::.gamlss.bi.list) gamlss(cbind(dat, bd-dat)~1, family=fam, trace=FALSE, ...)
          else gamlss(dat~1, family=fam, trace=FALSE, ...))  
if (any(class(m1)%in%"try-error")||any(is.na(deviance(m1))))
         { 
         warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))
}
cat("gamlss: ", deviance(m1), "\n")
m2 <- try( if (fname%in%gamlss:::.gamlss.bi.list) 
                gamlss(cbind(dat, bd-dat)~1, family=fam, trace=FALSE, method=mixed(2,100), ...)
           else gamlss(dat~1, family=fam, trace=FALSE, method=mixed(2,100),...))
cat("gamlss2:", deviance(m2), "\n")
if (any(class(m2)%in%"try-error")||any(is.na(deviance(m2))))
         { 
         warning(paste("gamlss method mixed()  failed", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))
        
         }
      if (any(class(m2)%in%"try-error")||any(is.na(deviance(m2)))||any(class(m1)%in%"try-error")||any(is.na(deviance(m1))))
         { 
         thedata<-cbind(thedata,dat) 
         nfails=nfails+1 
         theParameters[[nfails]] <-  c(mu.val[i], sigma.val[j])  
            cat("Currrent No. of model fitting fails: ", nfails, "\n")
if (trace==FALSE) 
  cat("-------------------------------------------------------------------------- \n") 
         }
  if (!any(class(m1)%in%"try-error") & !any(class(m2)%in%"try-error"))
         {
      dd <- deviance(m1)-deviance(m2)
   if(abs(dd)>0.001) # what level we want ?? 
      warning( cat("The deviances for methods RS() and mixed() do not agree at mu", mu.val[i], "and sigma", sigma.val[j], "with a difference", dd, "\n"))    
   if(abs(dd)>0.001)
          {
   theDiffdata <- cbind(theDiffdata,dat) 
       nDfails <- nDfails+1 
          }
         }
         
      }
    }
  }  
#---------------------------------------------------------------------------------------- 
################################################################################
################################################################################
## three parameters
################################################################################
################################################################################
if(no.active.par==3) # three parameter distributions
 {
 for (i in 1:length(mu.val))
   {
   for (j in 1:length(sigma.val))
     {
     for (k in 1:length(nu.val))
       {
#---------------------------------------------------------
if (trace==TRUE)
 {
cat("-------------------------------------- \n")
cat("testing at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "\n")
}
################################################################################       
## test whether a proper distribution summing up to 1
intY <- if (fname%in%gamlss:::.gamlss.bi.list)  
    sum(eval(call(dorfun, x= y.range [1]:bd, mu=mu.val[i],  sigma=sigma.val[j],  nu=nu.val[k], bd=bd)))   
        else  sum(eval(call(dorfun, x= y.range [1]:10000, mu=mu.val[i],  sigma=sigma.val[j],  nu=nu.val[k])))    
if (abs(intY-1)> 0.001) 
       warning( cat( "testing whether pdf sum up to 1: the summation do not add to 1", "at mu", mu.val[i],"sigma", sigma.val[j], "and nu", nu.val[k], "\n")) else 
      cat("The",fname,"distribution at mu", mu.val[i], "sigma", sigma.val[i], "and nu", nu.val[i], "sums up to one", "\n")     
################################################################################
## checking the whether the cdf and the inverse cdf are compatible
val <-  if (fname%in%gamlss:::.gamlss.bi.list) 
             eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], bd=bd))
        else eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k]))  
 pr <- if (fname%in%gamlss:::.gamlss.bi.list) 
             eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], bd=bd))
        else eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k])) #  get the cdf
qq <-  if (fname%in%gamlss:::.gamlss.bi.list) 
             eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], bd=bd)) 
        else eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k])) 
if(abs(qq-val)>0.0001) 
   warning( cat( "the p and q functions are not matching", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k], "and y =",val,  "\n")) else 
     cat("The p and q functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[i], "and nu", nu.val[i], "sums up to one", "\n")  
################################################################################
## checking whether pdf and cdf match 
  p1 <- if (fname%in%gamlss:::.gamlss.bi.list) 
              sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i],  sigma=sigma.val[j], nu=nu.val[k], bd=bd)))   
         else sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i], sigma=sigma.val[j],nu=nu.val[k])))     
  aa <- if (fname%in%gamlss:::.gamlss.bi.list) 
              p1-eval(call(porfun, q=val[1],  mu=mu.val[i],sigma=sigma.val[j],  nu=nu.val[k], bd=bd))
        else p1-eval(call(porfun, q=val[1],  mu=mu.val[i], sigma=sigma.val[j],  nu=nu.val[k])) #pNO(val)
if(abs(aa)>0.0001)
   warning( cat( "for the value", val, "the d and p functions do not match", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],"\n"))  else 
       cat( "The d and p functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "and nu",
            nu.val[k], "are OK", "\n")
################################################################################
## checking the tails in p function 
tt <-  if (fname%in%gamlss:::.gamlss.bi.list) 
       {
       eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], bd=bd, lower.tail=TRUE))+
       eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], bd=bd, lower.tail=FALSE))
       }
       else 
       {
       eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], lower.tail=TRUE))+
       eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], lower.tail=FALSE))
       }
if(abs(tt-1)>0.0001)
   warning( cat( "for the value", val, "the tails of p functions do not add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "\n"))   else 
     cat( "The tails of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "and nu",
          nu.val[k], "are OK", "\n") 
################################################################################
## fitting data 
## generate data in the global enviroment 
dat <<- dat<- if (fname%in%gamlss:::.gamlss.bi.list) 
                 eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k], bd=bd )) 
            else eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k])) #create data
if (trace==TRUE) cat("--fitting models-- \n")
m1 <- try( if (fname%in%gamlss:::.gamlss.bi.list) 
               gamlss(cbind(dat, bd-dat)~1, family=fam, trace=FALSE, ...)
          else gamlss(dat~1, family=fam, trace=FALSE)) 
cat("gamlss : ", deviance(m1), "\n")
      if (any(class(m1)%in%"try-error"||any(is.na(deviance(m1)))))
         { 
          warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "\n"))
      }
m2 <- try( if (fname%in%gamlss:::gamlss.bi.list) 
                gamlss2(cbind(dat, bd-dat)~1, family=fam, trace=FALSE, ...)
           else gamlss(dat~1, family=fam, trace=FALSE, method=mixed(2,100),...))
cat("gamlss2: ", deviance(m2), "\n")
      if (any(class(m2)%in%"try-error"||any(is.na(deviance(m2)))))
         { 
         warning(paste("gamlss method mixed()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k], "\n"))
         }
      if (any(class(m2)%in%"try-error"||any(is.na(deviance(m2))))||(any(class(m1)%in%"try-error"||any(is.na(deviance(m1))))))
         { 
         thedata<-cbind(thedata,dat) 
         nfails=nfails+1
         theParameters[[nfails]] <-  c(mu.val[i], sigma.val[j], nu.val[k])  
           cat("Currrent No. of model fitting fails: ", nfails, "\n")
         if (trace==FALSE) cat("---------------------------------------------------------- \n") 
         }
      if (!any(class(m1)%in%"try-error") & !any(class(m2)%in%"try-error"))
         {
         dd <- deviance(m1)-deviance(m2)
         if(abs(dd)>0.001) # what level we want ?? 
         warning( cat("The deviances for methods RS() and mixed() do not agree at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "with a difference", dd, "\n"))  
         if(abs(dd)>0.001)
          {
           theDiffdata<-cbind(theDiffdata,dat) 
          nDfails=nDfails+1 
          }
         }
       }
      }
    }
  }  
################################################################################
################################################################################
## four parameters
################################################################################
################################################################################
if(no.active.par==4) # four parameter distributions
 {
 for (i in 1:length(mu.val))
   {
   for (j in 1:length(sigma.val))
     {
     for (k in 1:length(nu.val))
       {
       for (l in 1:length(tau.val))
         {
################################################################################
if (trace==TRUE)
 {
cat("-------------------------------------------------------------------------- \n")
cat("testing at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l],"\n")
 }
################################################################################
## test whether a proper distribution summing up to 1
 intY <- if (fname%in%gamlss:::.gamlss.bi.list)  
   sum(eval(call(dorfun, x= y.range [1]:bd, mu=mu.val[i],  sigma=sigma.val[j],  nu=nu.val[k],  tau=tau.val[l], bd=bd))) else  
   sum(eval(call(dorfun, x= y.range [1]:10000, mu=mu.val[i],  sigma=sigma.val[j],  nu=nu.val[k], tau=tau.val[l])))    
if (abs(intY-1)> 0.001) 
       warning( cat( "testing whether pdf sum up to 1: the summation do not add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n")) else 
      cat("The", fname, "distribution at mu", mu.val[i], "sigma", sigma.val[i], "nu", nu.val[i],
             "and tau", tau.val[i], "sums up to one","\n")      
################################################################################
## checking the whether the cdf an inverse cdf are comparable 
val <- if (fname%in%gamlss:::.gamlss.bi.list)  
  eval(call(rorfun, n=1,   mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], bd=bd)) 
      else  
  eval(call(rorfun, n=1,   mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) 
 pr <- if (fname%in%gamlss:::.gamlss.bi.list) 
  eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], bd=bd))
      else 
  eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) #  get the cdf
 qq <-  if (fname%in%gamlss:::.gamlss.bi.list) 
  eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], bd=bd)) 
else eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) 
if(abs(qq-val)>0.0001) 
  warning(cat("the p and q functions are NOT matching at mu", mu.val[i], "sigma", sigma.val[j], 
               "nu", nu.val[k], "and tau", tau.val[l], "at y =", val, "\n")) else 
          cat("The p and q functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "and tau", tau.val[l], "sums up to one", "\n")  
################################################################################
## checking whether pdf and cdf match 
 p1 <- if (fname%in%gamlss:::.gamlss.bi.list) 
   sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i],  sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], bd=bd)))   
 else sum(eval(call(dorfun, x=y.range [1]:val[1],  mu=mu.val[i],  sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])))     
 aa <- if (fname%in%gamlss:::.gamlss.bi.list) 
   p1-eval(call(porfun, q=val[1],  mu=mu.val[i],  sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], bd=bd))
       else p1-eval(call(porfun, q=val[1],  mu=mu.val[i],   sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) 
if(abs(aa)>0.0001)
   warning( cat( "for the value", val, "the d and p functions do not match", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "and tau", tau.val[l],"\n"))  else
     cat( "The d and p functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "nu",
          nu.val[k], "and tau", tau.val[l], "are OK", "\n")
################################################################################
## checking the tail in p function 
 tt <-  if (fname%in%gamlss:::.gamlss.bi.list) 
 {
   eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k],  tau=tau.val[l], bd=bd, lower.tail=TRUE))+
     eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k],  tau=tau.val[l], bd=bd, lower.tail=FALSE))
 }
 else 
 {
   eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], lower.tail=TRUE))+
     eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], lower.tail=FALSE))
 }
 if(abs(tt-1)>0.0001)
   warning( cat( "for the value", val, "the tails of p functions do not add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "and tau", tau=tau.val[l], "\n"))   else 
     cat( "The tails of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "nu",
          nu.val[k], "and tau", tau=tau.val[l], "are OK", "\n") 
################################################################################
## fitting data  
dat<<-dat<-eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) #create data
if (trace==TRUE) cat("--fitting models-- \n")
 m1 <- try(gamlss(dat~1, family=fam, trace=FALSE, ...))  
if (any(class(m1)%in%"try-error")||any(is.na(deviance(m1))))
          { 
          warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n"))
      } else  cat("gamlss : ", deviance(m1), "\n")
m2 <- try(gamlss2(dat~1, family=fam, trace=FALSE, ...))
if (any(class(m2)%in%"try-error")||any(is.na(deviance(m2))))
          { 
          warning(paste("gamlss method mixed()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n"))
          } else cat("gamlss2: ", deviance(m2), "\n") 
if (any(class(m2)%in%"try-error")||any(is.na(deviance(m2)))||(any(class(m1)%in%"try-error"))||any(is.na(deviance(m1))))
          { 
                thedata <-cbind(thedata,dat) 
                 nfails <- nfails+1 
theParameters[[nfails]] <-  c(mu.val[i], sigma.val[j], nu.val[k], tau.val[l])  
cat("Currrent No. of model fitting fails: ", nfails, "\n")
if (trace==FALSE) cat("------------------------------------------------------------------- \n") 
          }
  if (!any(class(m1)%in%"try-error") & !any(class(m2)%in%"try-error"))
         {
      dd <- deviance(m1)-deviance(m2)
   if(abs(dd)>0.001) # what level we want ?? 
      warning( cat("The deviances for methods RS() and mixed() do not agree at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "with a difference", dd, "\n"))  
      if(abs(dd)>0.001)
          {
           theDiffdata <-cbind(theDiffdata,dat) 
            nDfails <- nDfails+1 
          }   
         }
        }
       }
      }
    }
  }  
################################################################################
################################################################################
# PLOTS
# 
if (fname%in%gamlss:::.gamlss.bi.list) 
{
if (no.active.par==1&&y.range[1]==0&&y.range[2]==bd)
    binom_1_31(fam)
if (no.active.par==2&&y.range[1]==0&&y.range[2]==bd)
    binom_2_33(fam)
if (no.active.par==3&&y.range[1]==0&&y.range[2]==bd)  
  binom_3_33(fam)
} else 
{
  if (no.active.par==1&&y.range[1]==0&&y.range[2]==Inf)
    disc1_22(fam)  
  if (no.active.par==2&&y.range[1]==0&&y.range[2]==Inf)
    disc2_33(fam)
  if (no.active.par==3&&y.range[1]==0&&y.range[2]==Inf)
    disc3_33(fam)
}

################################################################################
################################################################################
################################################################################
# exit 
on.exit(rm(dat, envir=.GlobalEnv))
if (save==TRUE&&nfails>0) thedata<<-data.frame(thedata)
if (save==TRUE&&nfails>0) theParameters<<-theParameters
if (save==TRUE&&nDfails>0) theDdata<<-data.frame(theDiffdata)

cat("----------------------------------------------------", "\n")
cat("number of failed model fits", nfails, "\n")
if (nfails>0) cat("The data for failed models are saved in data.frame thedata \n")
cat("----------------------------------------------------", "\n")
cat("The number of models in which RS() and mixed() methods differ is", nDfails, "\n")
if (nDfails>0) cat("The data for failed models are saved in data.frame theDdata \n")
cat("************************* test for", paste(fname), "finished here ************************", "\n")
cat("****************************************************************************", "\n")
}
################################################################################
################################################################################
################################################################################
################################################################################

#*******************************************************************************

plotDiff <- function(...)
{
nrows<-dim(theDdata)[2]
for (i in 1:nrows) 
 {
 Da<-theDdata[,i]
 op<-par(ask=TRUE)
 histDist(Da, ...)
 }
par(op) 
}
################################################################################
################################################################################
#test_discrete_gamlss_dist("PO",mu.val=c(1,2,10), y.range=c(0, Inf))
#test_discrete_gamlss_dist("BI",mu.val=c(.1,.2,.9), y.range=c(0, 10), bd=10)
#
#
#test_discrete_gamlss_dist("NBI",mu.val=c(1,2,10), y.range=c(0, Inf))
#test_discrete_gamlss_dist("BB",mu.val=c(1,2,10), y.range=c(0, 10), bd=10)
#test_discrete_gamlss_dist("ZASICHEL",mu.val=c(.1,.2,.9), y.range=c(0, 10), sigma.val = c(1,2), tau.range=c(0,1), tau.val=c(.1,.2))
#test_discrete_gamlss_dist("SICHEL",mu.val=c(1,2,10), y.range=c(0, Inf))
#test_discrete_gamlss_dist("BB",mu.val=c(1,2,10), bd=10, y.range=c(0, Inf))



# test_discrete_gamlss_dist("ZASICHEL",mu.val=c(1,2), y.range=c(0, Inf), sigma.val = c(1,2), tau.range=c(0,1), tau.val=c(.1,.2), nu.val=c(-1,0,1))



