
# function to plot distribution in the book
#
#require(gamlss)
#rm(list=ls())
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# GENERAL FUNCTIONS 
# 1 mtexti() TO CREATE TEXT AROUND A PLOT
#----------------------------------------------------
# FUNCTIONS FOR DISCRETE family DISTRIBUTIONS
# 2   count_1_31()  one parameter family count with three plots
# 3   count_1_22() one parameter family count with 2X2 plots
# 4   count_2_32() two parameter count with 3x2 plot
# 5   count_2_32R() two parameter count with 3x2 plot
# 6   count_2_33() two parameter count with 3x3 plot
# 7   count_3_32() three parameters count with 3x2 plot
# 8   count_3_33() three parameters count with 3x3 plot
#-----------------------------------------------------
# FUNCTIONS FOR CONTINUOUS family DISTIBUTIONS -inf +inf
# 9    contR_2_12()   
# 10   contR_3_11()
# 11   contR_4_13()
#-------------------------------------------------------------------
# FUNCTIONS FOR CONTINUOUS family DISTIBUTIONS 0 +inf
# 12   contRplus_2_11()
# 13   contRplus_3_13()
# 14   contRplus_4_33()
#-------------------------------------------------------------------
# FUNCTIONS FOR DISCRETE BINOMIAL
# 15 binom_1_31()
# 16 binom_2_33()
# 17 binom_3_33()
#-------------------------------------------------------------------
# FUNCTIONS FOR CONTINUOUS family DISTIBUTIONS 0 1
# 18 contR01_2_13()
# 19 contR01_4_33()
#------------------------------------------------------------------
#-----------------------------------------------------------------------
# putting text around a graph
#-----------------------------------------------------------------------
# FUNCTION 1
#----------------------------------------------------------------------=
mtexti <- function(text, side, off = 0.25,
                   srt = if(side == 2) 90  else
                     if(side == 4) 270 else 0, 
                    ypos=NULL, xpos=NULL, ...) {
  # dimensions of plotting region in user units
  usr <- par("usr")
  # dimensions of plotting region in inches
  pin <- par("pin")
  # user units per inch
  upi <- c(usr[2]-usr[1],
           usr[4]-usr[3]) / pin
  # default x and y positions
  xpos <- if(is.null(xpos)) (usr[1] + usr[2])/2 else xpos
  ypos <- if(is.null(ypos)) (usr[3] + usr[4])/2 else ypos 
  if(1 == side)
    ypos <- usr[3] - upi[2] * off
  if(2 == side)
    xpos <- usr[1] - upi[1] * off
  if(3 == side)
    ypos <- usr[4] + upi[2] * off
  if(4 == side)
    xpos <- usr[2] + upi[1] * off
  text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
}
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# I am not sure about this
#<<echo=FALSE,include=FALSE>>=
# dgen<-function(familyr=c("norm","gamma"),x){
#   familyr <- match.arg(familyr)
#   familyr <- paste0("d", familyr, "(", as.character(x),")")
#   cat("Evaluating", familyr, "...\n")
#   return(eval(parse(text=familyr)))
# }
#------------------------------------------------------------------
#------------------------------------------------------------------
###################################################################
# Gillian's one parameter discrete
### Discrete count, 1 parameter 3 figures 3x1
# count_1_31("PO")
###################################################################
#-----------------------------------------------------------------------
# FUNCTION 2
#----------------------------------------------------------------------=
#plot one parameter with three plots
count_1_31 <- function(family = PO, 
                           mu = c(1,2,5), 
                         miny = 0, 
                         maxy = 10, 
                   cex.axis = 1.2, 
                      cex.all = 1.5)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
           else if (is.character(family)) family
           else if (is.call(family)) as.character(family[[1]])
           else if (is.function(family)) deparse(substitute(family))
           else if (is(family, "gamlss.family"))  family$family[1]
          else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
   if (type!="Discrete") stop("This plot is design for discrete distributions")
   if (nopar>=2) stop("This plot is design for one parameter discrete distributions")
  #---------------------------------------------------------
     op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,1))
      y <- miny:maxy
  #1
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[1])")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[1]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=(cex.all+0.02) )
  #2
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[2])")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",yaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[2]),"  ")),bty="n",
         cex=cex.all)
  axis(4,cex.axis=cex.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)  
  #3
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[3])")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,ylim=c(0,1.2*max(fy)),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[3]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
###################################################################
#-----------------------------------------------------------------------
# FUNCTION 3
#----------------------------------------------------------------------=
# Gillian's one parameter discrete
### Discrete count, 1 parameter   2x2 figures
# count_1_22()
###################################################################
# four plots
count_1_22 <- function(family=PO, mu=c(1,2,5,10), miny=0, 
                     maxy=20, cex.axis=1.2, cex.all=1.5)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (nopar>=2) stop("This plot is design for one parameter discrete distributions")
  #---------------------------------------------------------
  op<-par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(2,2))
      y <- miny:maxy
    fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1])")))
    fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2])")))
    fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[3])")))
    fy4 <- eval(parse(text=paste0("d",fname,"(y,mu[4])")))
  mm <- max(fy1, fy2, fy3, fy4)
  #1
  #fy <- eval(parse(text=paste0("d",family,"(y,mu[1])")))
  plot(y,fy1,type="h",ylab="f(y)",lwd=1.5, xaxt="n", ylim=c(0,1.1*mm),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[1]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all) 
  mtexti(fname, 3 , off=.5, cex=(cex.all+0.02), xpos=21 )
  #2
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[2])")))
  plot(y,fy2,type="h",ylab="f(y)",lwd=1.5,xaxt="n",yaxt="n", ylim=c(0,1.1*mm),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[2]),"  ")),bty="n",
         cex=cex.all)
  # axis(4,cex.axis=cex.axis)
  #mtexti("f(y)", 2, 0.6,cex=cex.all)  
  #3
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[3])")))
  plot(y,fy3,type="h",ylab="f(y)",lwd=1.5, ylim=c(0,1.1*mm),yaxt="n",
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[3]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  #4
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[4])")))
  plot(y,fy4,type="h",ylab="f(y)",lwd=1.5,yaxt="n",  ylim=c(0,1.1*mm),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[4]),"  ")),bty="n",
         cex=cex.all)
  axis(4,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
###################################################################
###################################################################
### Discrete count, 2 parameter 3 mu x 2 sigma 
# count_2_32()
###################################################################
#-----------------------------------------------------------------------
# FUNCTION 4
#----------------------------------------------------------------------=
#### Discrete count, 2 parameters
#------------------------------------------------------------------
# a 3 mu  by 2 sigma  table 
count_2_32 <- function(family=NBI, mu= c(0.5, 1,5), sigma = c(.1, 2), miny=0,
                       maxy=10, cex.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #---------------------------------------------------------
      op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,2))
  ### mu: 3 values
  ### sigma: 2 values
  y <- miny:maxy
  #if(family=="BB") bd <- ",bd=maxy" else bd=""
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy), cex=cex.all,
       cex.axis=cex.axis)
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3, cex=cex.all)
  mtexti("f(y)", 2, 0.6, cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=(cex.all+0.02), xpos=10 )
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1, cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy), cex.axis=cex.axis)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}

#------------------------------------------------------------------
###################################################################
#-----------------------------------------------------------------------
# FUNCTION 5
#----------------------------------------------------------------------=
### Discrete count, 2 parameter reverse 3 sigma x 2 mu 
# count_2_32R()
###################################################################
# a 3 mu  by 2 sigma  table 
count_2_32R <- function(family=NBI, mu= c(1,2), sigma=c(0.1,1,2), miny=0, 
                   maxy=10, cex.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #---------------------------------------------------------
     op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,2))
  ### mu: 3 values
  ### sigma: 2 values
  y <- miny:maxy
 # if(family=="BB") bd <- ",bd=maxy" else bd=""
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[i]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2],sigma[i]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.axis)
  mtexti(bquote(paste(mu," = ",.(mu[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=(cex.all+0.02), xpos=10 )
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[j]))),3,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[i]))),4,cex=cex.all,off=.6,srt=90)
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[i]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2],sigma[i]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[i]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2],sigma[i]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
###################################################################
### Discrete, 2 parameter 3 mu x 3 sigma 
# count_2_33()
###################################################################
#-----------------------------------------------------------------------
# FUNCTION 6
#----------------------------------------------------------------------=
#### Discrete count, 2 parameters
# count_2_33()
#------------------------------------------------------------------
# a 3 mu  by 3 sigma  table 
count_2_33 <- function(family=NBI, mu= c(0.1,1, 2), sigma = c(0.5, 1, 2), 
                       miny=0, maxy=10, cex.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #--------------------------------------------------------- 
       op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  ### mu: 3 values
  ### sigma: 2 values
  y <- miny:maxy
  #if(family=="BB") bd <- ",bd=maxy" else bd=""
  #--------------------------------------------
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3]",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #----------
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.axis)
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6, cex=cex.all)
  #----------
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(fname, 3 ,off=.5, cex=cex.all+0.02 )
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #-----------
  #1,3
  i=1
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #------------------------------------- 
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3]",")")))
  maxy <- 1.2*max(fy1,fy2,fy3)
  #-----------
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #-----------
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  #axis(4,cex.axis=cex.all)
  #-----------
  #2,3
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)
  #----------------------------------------
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3]",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  
  #3,3
  j=3
  plot(y,fy3,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# FUNCTION 7
#-----------------------------------------------------------------------
###################################################################
### Discrete, 3 parameter 3 mu x 2 sigma  plus nu superimposed
# count_3_32()
###################################################################
count_3_32 <- function(family=SICHEL, mu=c(1,5,10), sigma = c(0.5,1), 
                       nu=c(-0.5,0.5),
                  miny=0, maxy=10, cex.axis=1.5, cex.all=1.5,
                  cols=c("darkgray", "black"), spacing = 0.2,
                  legend.cex=1, legend.x="topright",
                  legend.where=c("left","right"))
{
  #--------------------------------------------------------
  legend.where <- match.arg(legend.where)
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter discrete distributions")
  #--------------------------------------------------------- 
       op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,2))
        y <- miny:maxy
  # cex.axis <- cex.axis
  # cex.all <- cex.all
  ### Row 1
  i=1
  fy11 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy12 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy21 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy22 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  maxy <- 1.2*max(fy11,fy12,fy21,fy22)
  #1,1
  j=1
  plot(y,fy11,type="h",xaxt="n",ylim=c(0,maxy), cex=cex.all,
       cex.axis=cex.axis, col=cols[1])
  lines(y+spacing,fy12,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3, off=.5, cex=cex.all, xpos=10)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="left") legend(legend.x,legend=as.expression(ex.leg),
         lty=1:2,col=cols[1:2], lwd=2, cex=legend.cex)
  #1,2
  i=1
  j=2
  plot(y,fy21,type="h",xaxt="n", yaxt="n", ylim=c(0,maxy), col=cols[1])
  lines(y+spacing,fy22,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="right") legend(legend.x,legend=as.expression(ex.leg),
                                   lty=1:2,col=cols[1:2], lwd=2, cex=legend.cex)

  ### Row 2
  i=2
  fy11 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy12 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy21 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy22 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  maxy <- 1.2*max(fy11,fy12,fy21,fy22)


  #2,1
  j=1
  plot(y,fy11,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy12,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6, cex=cex.all)


  #2,2
  j=2
  plot(y,fy21,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy22,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)


  ### Row 3
  i=3
  fy11 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy12 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy21 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy22 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  maxy <- 1.2*max(fy11,fy12,fy21,fy22)

  #3,1
  j=1
  plot(y,fy11,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis,col=cols[1])
  lines(y+spacing,fy12,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)

  #3,2
  j=2
  plot(y,fy21,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy22,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 8
#----------------------------------------------------------------------=
###################################################################
### Discrete, 3 parameter 3 mu x 3 sigma  plus nu superimposed
# count_3_33()
###################################################################
count_3_33 <- function(family=SICHEL, mu= c(1,5,10), sigma = c(0.5,1,2), nu=c(-0.5,0.5,1),
                     miny=0, maxy=10, cex.axis=1.5, cex.all=1.5,
                     cols=c("darkgray", "black"), spacing = 0.3,
                     legend.cex=1, legend.x="topright",
                     legend.where=c("left","right", "center"))
{
  #--------------------------------------------------------
  legend.where <- match.arg(legend.where) 
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
     type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  y <- miny:maxy
  # cex.axis <- cex.axis
  # cex.all <- cex.all
  ### Row 1
  i=1
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1]",")")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2]",")")))
  maxy <- 1.2*max(fy111,fy112,fy121,fy122,fy131, fy132)
  #1,1 nu 1 2
  j=1
  plot(y,fy111,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.axis,
       cex.axis=cex.axis, col=cols[1])
  lines(y+spacing,fy112,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6, cex=cex.all)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="left") legend(legend.x,legend=as.expression(ex.leg),
                            lty=1:2,col=cols[1:2],lwd=2,cex=legend.cex)
  #1,2 nu 1 2
  i=1
  j=2
  plot(y,fy121,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy122,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(fname, 3 ,off=.5, cex=cex.all )
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="center") legend(legend.x,legend=as.expression(ex.leg),
         lty=1:2,col=cols[1:2],lwd=2,cex=legend.cex)
  #1,3 nu 1 2
  i=1
  j=3
  plot(y,fy131,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy132,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="right") legend(legend.x,legend=as.expression(ex.leg),
                                     lty=1:2,col=cols[1:2],lwd=2,cex=legend.cex)
  ### Row 2
  i=2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1]",")")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2]",")")))
  maxy <- 1.2*max(fy211,fy212,fy221,fy222,fy221,fy232)
  #2,1 nu 1,2 
  j=1
  plot(y,fy211,type="h",xaxt="n", yaxt="n", ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy212,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #2,2 nu 1,2 
  j=2
  plot(y,fy221,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy222,type="h",lty=2,col=cols[2])
  #2,2 nu 1,2 
  j=3
  plot(y,fy231,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy232,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)
  ### Row 3
  i=3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1]",")")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2]",")")))
  maxy <- 1.2*max(fy311,fy312,fy321,fy322,fy331, fy332)
  
  #3,1  nu 1,2 
  j=1
  plot(y,fy311,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis,col=cols[1])
  lines(y+spacing,fy312,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  
  #3,2 nu 1,2 
  j=2
  plot(y,fy321,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy322,type="h",lty=2,col=cols[2])
  # mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis,off=0.6,srt=90)
  # axis(1,cex.axis=cex.axis)
  # mtexti("y", 1, 0.6,cex=cex.axis)
  
  #3,3 nu 1,2 
  j=2
  plot(y,fy331,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy332,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}

#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 9
#----------------------------------------------------------------------=
#------------------------------------------------------------------------
# Continuous 2 parameters -Inf +Inf  
# contR_2_12() 
#------------------------------------------------------------------------
contR_2_12 <- function(family="NO", mu=c(0,-1,1), sigma=c(1,0.5,2), 
                       cols=c(gray(.1),gray(.2),gray(.3)), 
                   ltype = c(1,2,3), maxy=7, no.points=201, y.axis.lim=1.1, 
                   cex.axis=1.5, cex.all=1.5, legend.cex=1, legend.x="topleft" )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
         else if (is.character(family)) family
         else if (is.call(family)) as.character(family[[1]])
         else if (is.function(family)) deparse(substitute(family))
         else if (is(family, "gamlss.family"))  family$family[1]
         else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter continuous  distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,2))
  #mu=vector of length 3
  #sigma= vector of length 3
  y<-seq(-maxy,maxy,length=no.points)
  
  pdf11 <- paste0("d",fname,"(y,mu[1],1)")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y,mu[2],1)")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y,mu[3],1)")
   fy13 <- eval(parse(text=pdf13))
  
  pdf21 <- paste0("d",fname,"(y,0,sigma[1])")
  fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y,0,sigma[2])")
  fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y,0,sigma[3])")
  fy23 <- eval(parse(text=pdf23))
  maxfy=y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23)
  ylabel <- "f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1], lwd=2, ylim =c(0,maxfy), cex.axis=cex.axis)
  lines(fy12~y, col=cols[2], lty=ltype[2], lwd=2)
  lines(fy13~y, col=cols[3], lty=ltype[3], lwd=2)
  ex.leg <- c(bquote(paste(mu," = ",.(mu[1]))),bquote(paste(mu," = ",.(mu[2]))),bquote(paste(mu," = ",.(mu[3]))))
  legend(legend.x,legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = 1")),3, cex=cex.all)

  mtexti(fname, 3 , off=.5, cex=cex.all, xpos=8 )
  #2
  plot(fy21~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy22~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
              bquote(paste(sigma," = ",.(sigma[3]))))
  legend(legend.x,legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(mu," = 0")),3,cex=cex.all)
  par(op)
}

#contR2("NO",c(1,2,3),c(1,2,3),6)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#***********************************************************************
#-----------------------------------------------------------------------
# FUNCTION 10
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
# contR_3_11()
#-----------------------------------------------------------------------
contR_3_11 <- function(family="PE", mu=0, sigma=1, nu=c(1,2,3), 
                       cols=c(gray(.1),gray(.2),gray(.3)), maxy=7, no.points=201, 
                   ltype = c(1,2,3), y.axis.lim=1.1, cex.axis=1.5, cex.all=1.5,
                   legend.cex=1, legend.x="topleft")
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
          else if (is.character(family)) family
          else if (is.call(family)) as.character(family[[1]])
          else if (is.function(family)) deparse(substitute(family))
          else if (is(family, "gamlss.family"))  family$family[1]
          else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,1))
  #mu=vector of length 3
  #sigma= vector of length 3
      y <-seq(-maxy,maxy,length=no.points)
  pdf11 <- paste0("d",fname,"(y, mu=mu,sigma=sigma, nu=nu[1])")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y, mu=mu,sigma=sigma, nu=nu[2])")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu,sigma=sigma, nu=nu[3])")
   fy13 <- eval(parse(text=pdf13))
  maxfy <- y.axis.lim*max(fy11,fy12,fy13)
 ylabel <- "f(y)"
  #1
 
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,
       ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  legend(legend.x,legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.7, cex=cex.all)
  # mtexti(paste0("mu = ",mu),3, cex=1)
  mtexti(bquote(paste(mu," = ",.(mu)," , ",sigma," = ",.(sigma))),3,cex=cex.all)
  # bquote(paste0(bquote(paste0("mu = ",mu))), ", sigma =",sigma))
  par(op)
}
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#***********************************************************************
#-----------------------------------------------------------------------
# FUNCTION 11
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
#  contR_4_13()
#  contR_4_13( legend.cex=1.13, legend.where="right", legend.x="topright")
#-----------------------------------------------------------------------
contR_4_13 <- function(family="SEP3", mu=0, sigma=1,  nu=c(0.5,1,2), 
                   tau=c(1,2,5),  cols=c(gray(.1),gray(.2),gray(.3)), maxy=7, 
                   no.points=201, ltype = c(1,2,3), 
                   y.axis.lim=1.1, cex.axis=1.5, cex.all=1.5, 
                   legend.cex=1, legend.x="topleft", 
                   legend.where=c("left","right") )
{
#--------------------------------------------------------
  legend.where <- match.arg(legend.where)
  fname <- if (is.name(family)) as.character(family)
         else if (is.character(family)) family
         else if (is.call(family)) as.character(family[[1]])
         else if (is.function(family)) deparse(substitute(family))
         else if (is(family, "gamlss.family"))  family$family[1]
         else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==4)) stop("This plot is design for four parameter continuous distributions")
#--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,3))
  #mu=vector of length 3
  #sigma= vector of length 3
  
  y<-seq(-maxy,maxy,length=no.points)
  pdf11 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[1], tau=tau[1])")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[2], tau=tau[1])")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[3], tau=tau[1])")
   fy13 <- eval(parse(text=pdf13))
  
  pdf21 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[1], tau=tau[2])")
   fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[2], tau=tau[2])")
   fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[3], tau=tau[2])")
   fy23 <- eval(parse(text=pdf23))
  
  pdf31 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[1], tau=tau[3])")
   fy31 <- eval(parse(text=pdf31))
  pdf32 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[2], tau=tau[3])")
   fy32 <- eval(parse(text=pdf32))
  pdf33 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[3], tau=tau[3])")
   fy33 <- eval(parse(text=pdf33))
  
  
  maxfy= y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23,fy31,fy32,fy33)
  ylabel <-"f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy),
       cex.axis=cex.axis)
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
 if (legend.where=="left") legend(legend.x,legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(tau," = ",.(tau[1]))),3, cex=cex.all)
  #2
  plot(fy21~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy22~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
  #             bquote(paste(sigma," = ",.(sigma[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #       lty=1:3, col=cols[1:3], lwd=2, cex=1)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(tau," = ",.(tau[2]))),3, cex=cex.all)
  mtexti(fname, 3 , off=.7, cex=cex.all+0.02)
  #3
  plot(fy31~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy32~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy33~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
  #             bquote(paste(sigma," = ",.(sigma[3]))))
  if (legend.where=="right") legend(legend.x,legend=as.expression(ex.leg),
                                   lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(tau," = ",.(tau[3]))),3, cex=cex.all)
  par(op)
}
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#***********************************************************************
#-----------------------------------------------------------------------
# FUNCTION 12
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
# contRplus_2_11()
#-----------------------------------------------------------------------
contRplus_2_11 <- function(family=GA, mu=1, sigma=c(.1,.6,1), 
                           cols=c(gray(.1),gray(.2),gray(.3)), 
                           maxy=4, no.points=201, 
                   y.axis.lim=1.1, ltype = c(1,2,3),
                   cex.axis=1.5, cex.all=1.5,
                   legend.cex=1, legend.x="topright"
                   )
{
  #--------------------------------------------------------
   fname <- if (is.name(family)) as.character(family)
         else if (is.character(family)) family
         else if (is.call(family)) as.character(family[[1]])
         else if (is.function(family)) deparse(substitute(family))
         else if (is(family, "gamlss.family"))  family$family[1]
         else stop("the family must be a character or a gamlss.family name")
     fam1 <- eval(parse(text=fname)) # the family to output
      fam <- as.gamlss.family(family) # this is created so I can get things
   dorfun <- paste("d",fname,sep="") # say dNO
    nopar <- fam$nopar # or fam1$nopar
     type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,1))
  #mu=vector of length 3
  #sigma= vector of length 3
  y <-seq(0.001,maxy,length=no.points)
   pdf11 <- paste0("d",fname,"(y, mu=mu,sigma=sigma[1])")
    fy11 <- eval(parse(text=pdf11))
   pdf12 <- paste0("d",fname,"(y,mu=mu,sigma=sigma[2])")
  fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu,sigma=sigma[3])")
  fy13 <- eval(parse(text=pdf13))
  maxfy <- y.axis.lim*max(fy11,fy12,fy13)
  ylabel <- "f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),bquote(paste(sigma," = ",.(sigma[3]))))
  legend(legend.x,legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.7, cex=cex.all)
  # mtexti(paste0("mu = ",mu),3, cex=1)
  mtexti(bquote(paste(mu," = ",.(mu))),3,cex=cex.all)
  par(op)
}
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#***********************************************************************
#-----------------------------------------------------------------------
# FUNCTION 13
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
# contRplus_3_13()
#-----------------------------------------------------------------------
contRplus_3_13 <- function(family="BCCG", mu=1, sigma=c(0.15, 0.2, 0.5), 
                    nu=c(-2,0,4), 
                    cols=c(gray(.1),gray(.2),gray(.3)), 
                    maxy=4, ltype = c(1,2,3),
                   no.points=201, y.axis.lim=1.1,
                   cex.axis=1.5, cex.all=1.5,
                   legend.cex=1, legend.x="topright",
                   legend.where=c("left","right"))
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  legend.where <- match.arg(legend.where)
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,3))
  #mu=vector of length 3
  #sigma= vector of length 3
  y <- seq(0.001,maxy,length=no.points)
  pdf11 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[1], nu=nu[1])")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[1], nu=nu[2])")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[1], nu=nu[3])")
   fy13 <- eval(parse(text=pdf13))
  
  pdf21 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[2], nu=nu[1])")
  fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[2], nu=nu[2])")
  fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[2], nu=nu[3])")
  fy23 <- eval(parse(text=pdf23))
  
  pdf31 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[3], nu=nu[1])")
  fy31 <- eval(parse(text=pdf31))
  pdf32 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[3], nu=nu[2])")
  fy32 <- eval(parse(text=pdf32))
  pdf33 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[3], nu=nu[3])")
  fy33 <- eval(parse(text=pdf33))
  
  
  maxfy= y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23,fy31,fy32,fy33)
  ylabel <-"f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1], cex.axis=cex.axis,
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #        lty=ltype, col=cols[1:3], lwd=2, cex=1)
  
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3, cex=cex.all)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  if (legend.where=="left") legend(legend.x, legend=as.expression(ex.leg),
                                    lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  #2
  plot(fy21~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy22~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[2]))),3, cex=cex.all)
  mtexti(fname, 3 , off=.7, cex=(cex.all+0.02))
  #3
  plot(fy31~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy),cex.axis=cex.axis)
  lines(fy32~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy33~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
  #             bquote(paste(sigma," = ",.(sigma[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #        lty=1:3, col=cols[1:3], lwd=2, cex=1)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  if (legend.where=="right") legend(legend.x, legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[3]))),3, cex=cex.all)
  par(op)
}
#--------------------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 14
#----------------------------------------------------------------------=# #
# contRplus_4_33(legend.x="topleft') 
#--------------------------------------------------------------------------
contRplus_4_33 <- function(family=BCT, mu=1, sigma = c(0.15,0.2,0.5), 
                           nu=c(-4,0,2), tau=c(100, 5, 1),
                       cols=c(gray(.1),gray(.2),gray(.3)), 
                       maxy=4, ltype = c(1,2,3),
                       no.points=201, y.axis.lim=1.1,
                       cex.axis=1.5, cex.all=1.5,
                       legend.cex=1, legend.x="topright",
                       legend.where=c("left","right"))
{
  #--------------------------------------------------------
  legend.where <- match.arg(legend.where)
   fname <- if (is.name(family)) as.character(family)
       else if (is.character(family)) family
       else if (is.call(family)) as.character(family[[1]])
       else if (is.function(family)) deparse(substitute(family))
       else if (is(family, "gamlss.family"))  family$family[1]
       else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
    if (type!="Continuous") stop("This plot is design for continuous distributions")
    if (!(nopar==4)) stop("This plot is design for four parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
    y <- seq(0.001,maxy,length=no.points)
  ### Row 1
  i=1 # i tau j sigma h nu
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[1]",",tau[1])")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[2]",",tau[1])")))
  fy113 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[3]",",tau[1])")))
  
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[1]",",tau[1])")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[2]",",tau[1])")))
  fy123 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[3]",",tau[1])")))
  
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[1]",",tau[1])")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[2]",",tau[1])")))
  fy133 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[3]",",tau[1])")))
  
  maxyrow1 <- y.axis.lim*max(fy111 ,fy112,fy113, fy121,fy122,fy123, fy131, fy132,fy133)
  #i = 2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[1]",",tau[2])")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[2]",",tau[2])")))
  fy213 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[3]",",tau[2])")))
  
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[1]",",tau[2])")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[2]",",tau[2])")))
  fy223 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[3]",",tau[2])")))
  
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[1]",",tau[2])")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[2]",",tau[2])")))
  fy233 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[3]",",tau[2])")))
  
  maxyrow2 <- y.axis.lim*max(fy211 ,fy212,fy213, fy221,fy222,fy223, fy231, fy232,fy233)
  # i = 3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[1]",",tau[3])")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[2]",",tau[3])")))
  fy313 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[3]",",tau[3])")))
  
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[1]",",tau[3])")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[2]",",tau[3])")))
  fy323 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[3]",",tau[3])")))
  
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[1]",",tau[3])")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[2]",",tau[3])")))
  fy333 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[3]",",tau[3])")))
  maxyrow3 <- y.axis.lim*max(fy311 ,fy312,fy313, fy321,fy322,fy323, fy331, fy332,fy333)  
  
  ylabel <-"f(y)"

  # 1 1 
  plot(fy111~y, type="l", xaxt="n",  col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow1), cex.axis=cex.axis)
  lines(fy112~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy113~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  if (legend.where=="left") legend(legend.x,legend=as.expression(ex.leg),
                                    lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3, cex=cex.all)
  # 1 2 
  plot(fy121~y, type="l", xaxt="n", yaxt="n", col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow1), cex.axis=cex.axis)
  lines(fy122~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy123~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(sigma," = ",.(sigma[2]))),3, cex=cex.all)
  mtexti(fname, 3 , off=.7, cex=cex.all+0.02)
  # 1 3
  plot(fy131~y, type="l",  xaxt="n", yaxt="n", col=cols[1], ylab="", xlab="y", 
       lty=ltype[1], lwd=2, ylim=c(0,maxyrow1))
  lines(fy132~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy133~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(sigma," = ",.(sigma[3]))),3, cex=cex.all)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  if (legend.where=="right") legend(legend.x, legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=legend.cex)
  mtexti(bquote(paste(tau," = ",.(tau[1]))),4, cex=cex.all)
  # 2 1 
  plot(fy211~y, type="l", xaxt="n", col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2), cex.axis=cex.axis)
  lines(fy212~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy213~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  # 2 2 
  plot(fy221~y, type="l", xaxt="n", yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2), cex.axis=cex.axis)
  lines(fy222~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy223~y, col=cols[3],lty=ltype[3],lwd=2)
  # 2 3
  plot(fy231~y, type="l",  xaxt="n",yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2),  cex.axis=cex.axis)
  lines(fy232~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy233~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(tau," = ",.(tau[2]))),4, cex=cex.all)
  # 3 1
  plot(fy311~y, type="l",  col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3), cex.axis=cex.axis)
  lines(fy312~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy313~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6, cex=cex.all)
  # 2 2 
  plot(fy321~y, type="l",  yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3), cex.axis=cex.axis)
  lines(fy322~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy323~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6, cex=cex.all)
  # 2 3
  plot(fy331~y, type="l",yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3), cex.axis=cex.axis)
  lines(fy232~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy233~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6, cex=cex.all)
  mtexti(bquote(paste(tau," = ",.(tau[3]))),4, cex=cex.all)
  
#mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3,cex=cex.axis)
  par(op)
}

#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 15
# binom_1_31()
#----------------------------------------------------------------------=
binom_1_31 <- function(family=BI, mu=c(0.1,.5,.7), bd=NULL, 
                       miny=0, maxy=20, cex.axis=1.2, 
                       cex.all=1.5)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (nopar>=2) stop("This plot is design for one parameter discrete distributions")
  #---------------------------------------------------------
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,1))
  bd <- if (is.null(bd))  maxy else bd
  y <-0:bd
  #1
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[1], bd=bd)")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[1]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=1.5 )
  #2
  fy <-eval(parse(text=paste0("d",fname,"(y,mu[2], bd=bd)")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",yaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[2]),"  ")),bty="n",
         cex=cex.all)
  axis(4,cex.axis=cex.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)  
  #3
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[3], bd=bd)")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,ylim=c(0,1.2*max(fy)),
       cex.axis=cex.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[3]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
#----------------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 16
#----------------------------------------------------------------------=
# binom_2_33() 
#----------------------------------------------------------------------
binom_2_33 <- function(family=BB, mu= c(0.1,.5, .8), sigma = c(0.5, 1, 2), bd=NULL, miny=0, maxy=10, cex.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  ### mu: 3 values
  ### sigma: 2 values
  bd <- if (is.null(bd))  maxy else bd
  y <-0:bd
  #--------------------------------------------
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1], bd=bd",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2], bd=bd",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3], bd=bd",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #----------
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.axis)
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #----------
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(fname, 3 ,off=.5, cex=cex.all+0.02 )
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #-----------
  #1,3
  i=1
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #------------------------------------- 
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1], bd=bd",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2], bd=bd",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3], bd=bd",")")))
  maxy <- 1.2*max(fy1,fy2,fy3)
  #-----------
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #-----------
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  #axis(4,cex.axis=cex.all)
  #-----------
  #2,3
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)
  #----------------------------------------
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1], bd=bd",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2], bd=bd",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3], bd=bd",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  
  #3,3
  j=3
  plot(y,fy3,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 17
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
binom_3_33 <- function(family=ZIBB, mu= c(.1,.5,.8), sigma = c(0.5,1,2), 
                     nu=c(0.01,0.3), bd = NULL,
                     miny=0, maxy=10, cex.axis=1.5, cex.all=1.5,
                     cols=c("darkgray", "black"), spacing = 0.3,
                     legend.cex=1, legend.x="topright",
                     legend.where=c("left","right", "center"))
{
  #--------------------------------------------------------
  legend.where <- match.arg(legend.where)
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  bd <- if (is.null(bd))  maxy else bd
  y <-0:bd
  cex.axis <- cex.axis
  cex.all <- cex.all
  ### Row 1
  i=1
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1], bd=bd",")")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2], bd=bd",")")))
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1], bd=bd",")")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2], bd=bd",")")))
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1], bd=bd",")")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2], bd=bd",")")))
  maxy <- 1.2*max(fy111,fy112,fy121,fy122,fy131, fy132)
  
  #1,1 nu 1 2
  j=1
  plot(y,fy111,type="h",xaxt="n",ylim=c(0,maxy),
       cex=cex.all,
       col=cols[1], cex.axis=cex.axis)
  lines(y+spacing,fy112,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="left") legend(legend.x,legend=as.expression(ex.leg),
                                   lty=1:2,col=cols[1:2],lwd=2, cex=legend.cex)
  #1,2 nu 1 2
  i=1
  j=2
  plot(y,fy121,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1],
       cex=cex.all,cex.axis=cex.axis)
  lines(y+spacing,fy122,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(fname, 3 ,off=.5, cex=cex.all+0.02 )
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="center") legend(legend.x,legend=as.expression(ex.leg),
                                    lty=1:2,col=cols[1:2],lwd=2, cex=legend.cex)
  #1,3 nu 1 2
  i=1
  j=3
  plot(y,fy131,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1],
       cex=cex.all,cex.axis=cex.axis)
  lines(y+spacing,fy132,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  if (legend.where=="right") legend(legend.x,legend=as.expression(ex.leg),
         lty=1:2,col=cols[1:2],lwd=2, cex=legend.cex)
  ### Row 2
  i=2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1], bd=bd",")")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2], bd=bd",")")))
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1], bd=bd",")")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2], bd=bd",")")))
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1], bd=bd",")")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2], bd=bd",")")))
  maxy <- 1.2*max(fy211,fy212,fy221,fy222,fy221,fy232)
  #2,1 nu 1,2 
  j=1
  plot(y,fy211,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1],
       cex=cex.all,cex.axis=cex.axis)
  lines(y+spacing,fy212,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #2,2 nu 1,2 
  j=2
  plot(y,fy221,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1],  cex=cex.all,
              cex.axis=cex.axis)
  lines(y+spacing,fy222,type="h",lty=2,col=cols[2])
  #2,2 nu 1,2 
  j=3
  plot(y,fy231,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1],
       cex=cex.all,cex.axis=cex.axis)
  lines(y+spacing,fy232,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis)
  ### Row 3
  i=3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1], bd=bd",")")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2], bd=bd",")")))
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1], bd=bd",")")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2], bd=bd",")")))
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1], bd=bd",")")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2], bd=bd",")")))

  maxy <- 1.2*max(fy311,fy312,fy321,fy322,fy331, fy332)
  
  #3,1  nu 1,2 
  j=1
  plot(y,fy311,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.all,col=cols[1],
       cex=cex.all,cex.axis=cex.axis)
  lines(y+spacing,fy312,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  #3,2 nu 1,2 
  j=2
  plot(y,fy321,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy322,type="h",lty=2,col=cols[2])
  # mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis,off=0.6,srt=90)
  # axis(1,cex.axis=cex.axis)
  # mtexti("y", 1, 0.6,cex=cex.axis)
  
  #3,3 nu 1,2 
  j=2
  plot(y,fy331,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1], cex.axis=cex.axis)
  lines(y+spacing,fy332,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}

#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 18
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
# contR01_2_13()
#------------------------------------------------------------------------
contR01_2_13 <- function(family="BE", mu=c(0.2, .5, .8), sigma=c(0.2, 0.5, .8), 
                         cols=c(gray(.1),gray(.2),gray(.3)), 
                         ltype = c(1,2,3), maxy=7, no.points=201, 
                         y.axis.lim=1.1, maxYlim=10,
                         cex.axis=1.5, cex.all=1.5,
                         legend.cex=1, legend.x="topright",
                         legend.where=c("left","right", "center"))
{
  #--------------------------------------------------------
  legend.where <- match.arg(legend.where)
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,3))
  #mu=vector of length 3
  #sigma= vector of length 3

  y<-seq(0.001, .999,length=no.points)
  #--------------
  pdf11 <- paste0("d",fname,"(y,mu[1],sigma[1])")
  fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y,mu[2],sigma[1])")
  fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y,mu[3],sigma[1])")
  fy13 <- eval(parse(text=pdf13))
  #---------------------------------
  pdf21 <- paste0("d",fname,"(y,mu[1],sigma[2])")
  fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y,mu[2],sigma[2])")
  fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y,mu[3],sigma[2])")
  fy23 <- eval(parse(text=pdf23))
  #---------------------------------
  pdf31 <- paste0("d",fname,"(y,mu[1],sigma[3])")
   fy31 <- eval(parse(text=pdf31))
  pdf32 <- paste0("d",fname,"(y,mu[2],sigma[3])")
   fy32 <- eval(parse(text=pdf32))
  pdf33 <- paste0("d",fname,"(y,mu[3],sigma[3])")
   fy33 <- eval(parse(text=pdf33))
  
  maxfy=y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23, fy31, fy32, fy33)
  maxfy <- min(maxfy, maxYlim)
  ylabel <- "f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1], cex=cex.all,
       ylab="", xlab="y",lty=ltype[1], lwd=2, ylim =c(0,maxfy), cex.axis=cex.axis)
  lines(fy12~y, col=cols[2], lty=ltype[2], lwd=2)
  lines(fy13~y, col=cols[3], lty=ltype[3], lwd=2)
  ex.leg <- c(bquote(paste(mu," = ",.(mu[1]))),bquote(paste(mu," = ",.(mu[2]))),bquote(paste(mu," = ",.(mu[3]))))
  if (legend.where=="left") legend(legend.x,legend=as.expression(ex.leg),
                          lty=1:3,col=cols[1:3],lwd=2, cex=legend.cex)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[1]))), 3, cex=cex.all)
  
  mtexti(fname, 3 , off=.5, cex=cex.all+0.02, xpos=8 )
  #2
  plot(fy21~y, type="l", col=cols[1], cex=cex.all,
       yaxt="n", xlab="y",lty=ltype[1],lwd=2, ylim=c(0,maxfy), cex.axis=cex.axis)
  lines(fy22~y, col=cols[2],lty=ltype[2], lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3] ,lwd=2)
  ex.leg <- c(bquote(paste(mu," = ",.(mu[1]))),bquote(paste(mu," = ",.(mu[2]))),bquote(paste(mu," = ",.(mu[3]))))
  if (legend.where=="center") legend(legend.x,legend=as.expression(ex.leg),
                   lty=1:3,col=cols[1:3],lwd=2, cex=legend.cex)
  
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[2]))), 3, cex=cex.all)
  
  #3
  plot(fy31~y, type="l", col=cols[1], yaxt="n",
       ylab="", xlab="y",lty=ltype[1], lwd=2, ylim =c(0,maxfy),  cex.axis=cex.axis)
  lines(fy32~y, col=cols[2], lty=ltype[2], lwd=2)
  lines(fy33~y, col=cols[3], lty=ltype[3], lwd=2)
  ex.leg <- c(bquote(paste(mu," = ",.(mu[1]))),bquote(paste(mu," = ",.(mu[2]))),bquote(paste(mu," = ",.(mu[3]))))
  if (legend.where=="right") legend(legend.x,legend=as.expression(ex.leg),
                    lty=1:3,col=cols[1:3],lwd=2, cex=legend.cex)
  # legend("topleft",legend=as.expression(ex.leg),
  #        lty=ltype, col=cols[1:3], lwd=2, cex=1.5)
  
  mtexti("y", 1, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[3]))), 3, cex=cex.all)
  par(op)
}

#contR01_4_33("GB1", mu=0.5, sigma=c(0.2,0.4,0.6), nu=c(0.5,1,2), tau=c(0.5,1,2), maxYlim=10,cex.axis=1.4, cex.all=1.4)
#contR2("NO",c(1,2,3),c(1,2,3),6)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#***********************************************************************

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# FUNCTION 19
#----------------------------------------------------------------------=
#-----------------------------------------------------------------------
# contR01_4_33()
contR01_4_33 <- function(family=GB1, mu=c(0.5), sigma = c(0.2,0.5,0.7), 
                        nu = c(1,2,5), tau=c(0.5, 1, 2),
                       cols=c(gray(.1),gray(.2),gray(.3)), 
                      maxy=0.999, ltype = c(1,2,3),
                       no.points=201, y.axis.lim=1.1, 
                      maxYlim=10,   cex.axis=1.5, cex.all=1.5,
                      legend.cex=1, legend.x="topright",
                      legend.where=c("left","right", "center"))
{
  #--------------------------------------------------------
legend.where <- match.arg(legend.where)
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
   fam <- as.gamlss.family(family) # this is created so I can get things
dorfun <- paste("d",fname,sep="") # say dNO
 nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==4)) stop("This plot is design for four parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  y <- seq(0.001,maxy,length=no.points)
  ### Row 1
  i=1 # i tau j sigma h nu
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[1]",",tau[1])")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[2]",",tau[1])")))
  fy113 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[3]",",tau[1])")))
  
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[1]",",tau[1])")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[2]",",tau[1])")))
  fy123 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[3]",",tau[1])")))
  
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[1]",",tau[1])")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[2]",",tau[1])")))
  fy133 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[3]",",tau[1])")))
   maxYval <- max(fy111 ,fy112,fy113, fy121,fy122,fy123, fy131, fy132,fy133)
  maxyrow1 <- if (maxYval> maxYlim) maxYlim else maxYval
  #i = 2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[1]",",tau[2])")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[2]",",tau[2])")))
  fy213 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[3]",",tau[2])")))
  
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[1]",",tau[2])")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[2]",",tau[2])")))
  fy223 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[3]",",tau[2])")))
  
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[1]",",tau[2])")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[2]",",tau[2])")))
  fy233 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[3]",",tau[2])")))

  maxYval <- max(fy211 ,fy212,fy213, fy221,fy222,fy223, fy231, fy232,fy233)
  maxyrow2 <- if (maxYval> maxYlim) maxYlim else maxYval
  #maxyrow2 <- y.axis.lim*max(fy211 ,fy212,fy213, fy221,fy222,fy223, fy231, fy232,fy233)
  # i = 3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[1]",",tau[3])")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[2]",",tau[3])")))
  fy313 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[1],nu[3]",",tau[3])")))
  
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[1]",",tau[3])")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[2]",",tau[3])")))
  fy323 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[2],nu[3]",",tau[3])")))
  
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[1]",",tau[3])")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[2]",",tau[3])")))
  fy333 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[3],nu[3]",",tau[3])")))
  maxYval <- max(fy311 ,fy312,fy313, fy321,fy322,fy323, fy331, fy332,fy333)
  maxyrow3 <- if (maxYval> maxYlim) maxYlim else maxYval
  #maxyrow3 <- y.axis.lim*max(fy311 ,fy312,fy313, fy321,fy322,fy323, fy331, fy332,fy333)  
  ylabel <-"f(y)"
  # 1 1 
  plot(fy111~y, type="l", xaxt="n",  col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow1), cex.axis=cex.axis, cex=cex.all)
  lines(fy112~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy113~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  if (legend.where=="left") legend(legend.x, legend=as.expression(ex.leg),
                                   lty=1:3,col=cols[1:3],lwd=2, cex=legend.cex)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3, cex=cex.all)
  # 1 2 
  plot(fy121~y, type="l", xaxt="n", yaxt="n", col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow1), cex.axis=cex.axis, cex=cex.all)
  lines(fy122~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy123~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(sigma," = ",.(sigma[2]))),3, cex=cex.all)
  mtexti(fname, 3 , off=.7, cex=cex.all+0.02)
  if (legend.where=="center") legend(legend.x,legend=as.expression(ex.leg),
                                   lty=1:3,col=cols[1:3],lwd=2, cex=legend.cex)
  # 1 3
  plot(fy131~y, type="l",  xaxt="n", yaxt="n", col=cols[1], ylab="", xlab="y", 
       lty=ltype[1], lwd=2, ylim=c(0,maxyrow1), cex.axis=cex.axis, cex=cex.all)
  lines(fy132~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy133~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(sigma," = ",.(sigma[3]))),3, cex=cex.all)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  if (legend.where=="right") legend(legend.x,legend=as.expression(ex.leg),
                                   lty=1:3,col=cols[1:3],lwd=2, cex=legend.cex)
  mtexti(bquote(paste(tau," = ",.(tau[1]))),4,cex=cex.all)
  # 2 1 
  plot(fy211~y, type="l", xaxt="n", col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2), cex.axis=cex.axis, cex=cex.all)
  lines(fy212~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy213~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  # 2 2 
  plot(fy221~y, type="l", xaxt="n", yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2), cex.axis=cex.axis, cex=cex.all)
  lines(fy222~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy223~y, col=cols[3],lty=ltype[3],lwd=2)
  # 2 3
  plot(fy231~y, type="l",  xaxt="n",yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2), cex.axis=cex.axis, cex=cex.all)
  lines(fy232~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy233~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(tau," = ",.(tau[2]))),4, cex=cex.all)
  # 3 1
  plot(fy311~y, type="l",  col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3), cex.axis=cex.axis, cex=cex.all)
  lines(fy312~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy313~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti( ylabel, 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6, cex=cex.all)
  # 3 2 
  plot(fy321~y, type="l", xaxt="n", yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3), cex.axis=cex.axis, cex=cex.all)
  lines(fy322~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy323~y, col=cols[3],lty=ltype[3],lwd=2)
  #axis(1,cex.axis=cex.axis)
  mtexti("y", 1, 0.6, cex=cex.all)
  # 3 3
  plot(fy331~y, type="l",yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3), cex.axis=cex.axis, cex=cex.all)
  lines(fy232~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy233~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6, cex=cex.all)
  mtexti(bquote(paste(tau," = ",.(tau[3]))),4,cex=cex.all)
  #mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3,cex=cex.axis)
  par(op)
}

#-----------------------------------------------------------------
#-----------------------------------------------------------------





