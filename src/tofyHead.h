/*
 *  header file for PIG, DEL, SI and SICHEL distributions
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
#ifndef tofyHead
#define tofyHead
void tofyPIG1(double *y, double *mu,double *sigma, double *ans, int *ny, int *maxy);
void tofyPIG2(double *y, double *mu,double *sigma, double *ans, int *ny, int *maxy);
void tocdf(double *y, double *mu, double *sigma, double *ans, int *ny);
void tofydel1(double *y, double *mu,double *sigma, double *nu, double *ans, int *ny, int *maxy);
void tofydel2(double *y, double *mu,double *sigma, double *nu, double *ans, int *ny, int *maxy);
void tofySI1(double *y, double *mu, double *sigma, double *nu, double *lbes, double *ans, int *ny, int *maxy);
void tofySI2(double *y, double *mu, double *sigma, double *nu, double *lbes, double *ans, int *ny, int *maxy);
void tocdfSIbis(double *y, double *mu, double *sigma, double *nu, double *ans, int *ny);
void tofySICHEL1(double *y, double *mu,double *sigma, double *nu, double *lbes, double *cvec, double *ans, int *ny, int *maxy);
void tofySICHEL2(double *y, double *mu,double *sigma, double *nu, double *lbes, double *cvec, double *ans, int *ny, int *maxy);
void cdfSICHEL(double *y, double *mu, double *sigma, double *nu, double *ans, int *ny);
void dDPOgetC5_C(double *mu, double *sigma, int *lmu, int *ly, double *ans);
#endif /* tofyHead */
