/* @(#)llalbr.c	1.1   07/09/89 */
/* created by: R.L.Glenn
*     To determine the northing 'NOR' and easting 'EAS' of the point 'lat', 
*     'lon'. The origin of the NOR-EAS coordinate system will be placed so  
*     that LATBAS, LONCEN will have coordinates NBASE, EBASE. THe       
*     projection will use standard parallels 'STLATS' and 'STLATN'.     
*     unless set otherwise by a call to ALBRST, standard U.S.A. values  
*     are used.                                                         
*                                                                       
*     western hemisphere is presumed                                    
*****/
#include <ctype.h>
#include <stdio.h>
#include <math.h>

llalbr(lat,lon,NOR,EAS)
      float *lat, *lon;
      double *EAS, *NOR;
{                                                     
      double STLATS, STLATN, ROSQ, EN, LATBAS, LONCEN;
      double NBASE, EBASE, AENRPD, RHOBAS;
      double TBBON, RPD, PI, FTPERM, SINSEC, SNSC36;
      double SEMAJ, SEMIN, ECC, ECCSQ, EPSQ, Q203, Q6011, Q305;
      double Q407, Q7013, Q509, Q8015, PROANG, ANG, PROCOS;
      double RHO, RHOSQ, SINLAT, X, XX, PROSIN;
      double PRELAT, PREANG;
      double DLON, DLAT;

      SEMAJ = 6378206.4;
      SEMIN = 6356583.8;
      ECC = 8.2271854223003E-2;
      ECCSQ = 6.768657997291054E-3;
      EPSQ = 6.814784945915042E-3;
      RPD = 1.74532925199433E-2;
      PI = 3.141592653589793;
      FTPERM = 3.28083333333333;
      SINSEC = 4.8481368110952E-6;
      SNSC36 = 1.745329251994272E-2;

      STLATS = 29.5;
      STLATN = 45.5;
      ROSQ = 1.50995771793488E+14;
      EN = 6.02903500627953E-1;
      LATBAS = 23.;
      LONCEN = 96.;
      NBASE = 0.0;
      EBASE = 0.0;
      AENRPD = 1.05226511577575E-2;                                  
      RHOBAS = .992907955799441E+07;
      TBBON = 1.34038556964216E+14;

      Q203 = 6.66666666666667E-1;
      Q6011 = 5.45454545454545E-1;
      Q305 = 6.E-1;
      Q407 = 5.71428571428571E-1;
      Q7013 = 5.38461538461538E-1;           
      Q509 = 5.55555555555556E-1;
      Q8015 = 5.33333333333333E-1;           
      PRELAT = 2.E+02;
      PREANG = 2.E+02;                                

/*     **** Start executable code ****    */

      DLON = *lon;                                                 
      DLAT = *lat;                                                 

/*     obtain trig function of projection angle (positive angle to east) */
      ANG = LONCEN - DLON;
      if (ANG != PREANG)
        {
        PREANG = ANG;
        PROANG = ANG * AENRPD;
        PROSIN = sin (PROANG);
        PROCOS = cos (PROANG);
        }

/*     obtain projection radius RHO */

      if (DLAT != PRELAT)
        {
        PRELAT = DLAT;
        SINLAT = sin (DLAT*RPD);
        X = SINLAT * ECC;
        XX = X * X;
        RHOSQ = ROSQ - (((((((Q8015*XX+Q7013)*XX+Q6011)*XX+Q509)*XX+Q407)*XX+Q305)*XX+Q203)*XX+1.0)*SINLAT*TBBON;
/*
*     The polynomial in XX is a truncation of the series solution of    
*      the intergal from 0 to lat of 
*            (cos(P)*DP/(1-ECCSQ*((sin(P)*power(2)))*power(2)) 
*       divided by SINLAT.                                              
*     An alternate solution, slower in machine execution, is            
*      (.5*X/(1-XX)+.25*LOG((1+X)/(1-X)))/ECC                           
*     TBBON is the product (2*(SEMAJ*power(2))*(1-ECCSQ))/*NOR
*                equal (2*(SEMIN*SEMIN))/*NOR
*/                                                                       
        RHO = sqrt (RHOSQ);
        }

/*     calculate NOR-EAS coordinates */

      *(EAS) = RHO * PROSIN + EBASE;
      *(NOR) = RHOBAS - RHO * PROCOS;

      return;                                                            
}
