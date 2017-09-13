/*
 *  cdfSICHEL.c
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
*/
#include "tofyHead.h"
#include <math.h>
#include <Rmath.h>
void cdfSICHEL(double *y, double *mu, double *sigma, double *nu, double *ans, int *ny) {
	int i, j, lyp1;
	//double alpha[*ny], cvec[*ny];
  double sumT, lbes, cvec, alpha;
  for (i = 0; i < *ny; i++)
  {
      lyp1 = y[i]+1;
      sumT = 0;
      cvec = exp( log(bessel_k(1/sigma[i], nu[i]+1, 1)) - log(bessel_k(1/sigma[i], nu[i], 1)) );
      alpha = sqrt(1 + 2*sigma[i]*mu[i]/cvec)/sigma[i];
      lbes = log(bessel_k(alpha, nu[i]+1, 1)) - log(bessel_k(alpha, nu[i], 1));        
      double tynew[lyp1];
      double lpnew[lyp1]; 
      tynew[0] = (mu[i]/cvec)*pow(1+2*sigma[i]*mu[i]/cvec,-0.5)*exp(lbes);
      lpnew[0] = -nu[i]*log(sigma[i]*alpha) +  log(bessel_k(alpha,nu[i],1)) - log(bessel_k(1/sigma[i],nu[i],1)); 
      for (j = 1; j < lyp1; j++)
      {
        tynew[j] = (cvec*sigma[i]*(2*(j + nu[i])/mu[i])+(1/tynew[j-1]))*pow(mu[i]/(sigma[i]*alpha*cvec),2);
        lpnew[j] = lpnew[j-1] + log(tynew[j-1]) - log(j);
        }
      for (j=0 ; j< lyp1; j++) sumT += exp(lpnew[j]);
      ans[i] = sumT;
  }

}

