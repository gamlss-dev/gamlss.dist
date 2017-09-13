/*
 *  toySI1.c 
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
 *
 */

#include "tofyHead.h"
#include <math.h>

void tofySI1(double *y, double *mu, double *sigma, double *nu, double *lbes, double *ans, int *ny, int *maxy) {
	int maxyp1 = *maxy + 1;
	double tofY[maxyp1];
	int iy, i, j;
	double alpha;
  for (i = 0; i < *ny; i++)
  {
      iy = y[i]+1;
      tofY[0] = mu[i]*pow(1+2*sigma[i]*mu[i],-0.5)*exp(lbes[i]); 
      alpha = sqrt(1 + 2*sigma[i]*mu[i])/sigma[i];
      for (j = 1; j < iy; j++)
      {
         tofY[j] = ( sigma[i]*(2*(j+nu[i])/mu[i]) + (1/tofY[j-1])) * pow(mu[i]/(sigma[i]*alpha),2);
      }
    ans[i] = tofY[iy-1];
  }
}
