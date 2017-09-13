/*################################################################################
##
##   R package gamlss.dist by ............ Copyright (C) ......... .
##   This file is part of the R package gamlss.dist.
##
##   The R package gamlss.dist is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   The R package gamlss.dist is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   C Code by Alexios Ghalanos 2013
#################################################################################*/
#ifndef ST3_H
#define ST3_H

double st3_dldm(const double, const double, const double, const double, const double);
void c_st3_dldm(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldm2(const double, const double, const double, const double, const double);
void c_st3_d2ldm2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_dldd(const double, const double, const double, const double, const double);
void c_st3_dldd(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldd2(const double,const double, const double, const double, const double);
void c_st3_d2ldd2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_dldv(const double,const double, const double, const double, const double);
void c_st3_dldv(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldv2(const double,const double, const double, const double, const double);
void c_st3_d2ldv2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_dldt(const double,const double, const double, const double, const double);
void c_st3_dldt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldt2(const double,const double, const double, const double, const double);
void c_st3_d2ldt2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldmdd(const double,const double, const double, const double, const double);
void c_st3_d2ldmdd(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldmdv(const double,const double, const double, const double, const double);
void c_st3_d2ldmdv(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldmdt(const double,const double, const double, const double, const double);
void c_st3_d2ldmdt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldddv(const double ,const double , const double , const double , const double );
void c_st3_d2ldddv(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldddt(const double,const double, const double, const double, const double);
void c_st3_d2ldddt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_d2ldvdt(const double,const double, const double, const double, const double);
void c_st3_d2ldvdt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n);
double st3_dst3(const double x,const double, const double, const double, const double, const int );
void c_st3_dst3(double *x,double *mu, double *sigma, double *nu, double *tau, double *ans, int *n, int* logr);
double st3_pst3(const double q,const double, const double, const double, const double, const int , const int );
void c_st3_pst3(double *q,double *mu, double *sigma, double *nu, double *tau, double *ans, int *n, int *lower_tail, int* logr);
double st3_qst3(const double p,const double, const double, const double, const double, const int , const int );
void c_st3_qst3(double *p,double *mu, double *sigma, double *nu, double *tau, double *ans, int *n, int *lower_tail, int* logr);
#endif /* ST3_H */
