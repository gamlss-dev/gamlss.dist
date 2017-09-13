/*
 *  tocdfSIbis.c.c 
 *   R package gamlss.dist by ............ Copyright (C) ......... .
 *   This file is part of the R package gamlss.dist.
 *
 *   The R package gamlss.dist is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   The R package gamlss.dist is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *  Created by Marco Enea on 07/05/14.
 *
 #   tocdfS <- function (y, mu, sigma, nu, bsum=TRUE, ...)
#   {
#       ly <- length(y)       
#    sigma <- rep(sigma, length = ly)
#       mu <- rep(mu, length = ly)   
#       nu <- rep(nu, length = ly) 
#       ty <- rep(0,length(y))
#      cdf <- rep(0,length(y))
#    alpha <- sqrt(1+2*sigma*mu)/sigma
#    lbes <-  log(besselK(alpha,nu+1))-log(besselK((alpha),nu)) 
#     for (i in 1:length(y))
#     {
#          lyp1 <- y[i]+1 
#         tynew <- lpnew <- rep(0,lyp1)
#         tynew[1] <- mu[i]*((1+2*sigma[i]*mu[i])^(-0.5))*exp(lbes[i])
#         lpnew[1] <- -nu[i]*log(sigma[i]*alpha[i])+log(besselK(alpha[i],nu[i]))-
#                      log(besselK(1/sigma[i],nu[i])) 
#         dum <- ifelse(lyp1==1, 1,2)
#         for (j in dum:lyp1)
#         {
#             if (j !=1)
#              {
#             tynew[j] <- (sigma[i]*(2*(j-1+nu[i])/mu[i])+(1/tynew[j-1]))*
#                          (mu[i]/(sigma[i]*alpha[i]))**2
#             lpnew[j] <- lpnew[j-1] + log(tynew[j-1]) - log(j-1)
#              }           
#             ty[i] <- tynew[lyp1] 
#         }
#         if (bsum ) 
#             cdf[i] <- sum(exp(lpnew))
#     }
#     cdf
#   }
*/
#include "tofyHead.h"
#include <math.h>
#include <Rmath.h>
void tocdfSIbis(double *y, double *mu, double *sigma, double *nu, double *ans, int *ny) {
	int i, j, lyp1;
	double alpha[*ny];
  double sumT;
  double lbes;
  for (i = 0; i < *ny; i++)
  {
      lyp1 = y[i]+1;
      sumT = 0;
      alpha[i] = sqrt(1+2*sigma[i]*mu[i])/sigma[i];
      lbes = log(bessel_k(alpha[i],nu[i],1));  
      double tynew[lyp1];
      double lpnew[lyp1]; 
      tynew[0] = mu[i]*pow(1+2*sigma[i]*mu[i],-0.5)*exp(log(bessel_k(alpha[i],nu[i]+1,1))-lbes);
      lpnew[0] = -nu[i]*log(sigma[i]*alpha[i]) +  lbes - log(bessel_k(1/sigma[i],nu[i],1)); 
      for (j = 1; j < lyp1; j++)
      {
        tynew[j] = (sigma[i]*(2*(j + nu[i])/mu[i])+(1/tynew[j-1]))*pow(mu[i]/(sigma[i]*alpha[i]),2);
        lpnew[j] = lpnew[j-1] + log(tynew[j-1]) - log(j);
       }
      for (j=0 ; j< lyp1; j++) sumT += exp(lpnew[j]);
      ans[i] = sumT;
  }
}

