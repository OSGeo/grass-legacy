/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       v.out.atlas
 * AUTHOR(S):    R. L. Glenn
 * PURPOSE:      see below.
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/* @(#)albrll.c	1.1   07/04/89 */
/* created by: R.L.Glenn
*     To determine Latitude 'lat' and Longitude 'lon' of the point N,E  
*     the origin of the N_E coordinate system will be placed so that
*     LATBASE, LONCEN will have coordinates NBASE, EBASE. The projection
*     will use standard parallels 'STLATS' and 'STLATN'. Unless set
*     otherwise by a call to albrst, standard US of A values are used.
*                                                                       
*     Western Hemisphere is presumed                                    
*                                                                       
*****/

#include <ctype.h>
#include <stdio.h>
#include <math.h>

albrll(lat, lon, NOR, EAS)
     float *lat, *lon;
     double *NOR, *EAS;
{
  double STLATS, STLATN, ROSQ, EN, LATBAS, LONCEN;
  double NBASE, EBASE, AENRPD, RHOBAS;
  double TBBON, RPD, PI, FTPERM, SINSEC, SNSC36, Q305;
  double SEMAJ, SEMIN, ECC, ECCSQ, EPSQ, Q203, Q6011;
  double Q407, Q7013, Q509, Q8015, PROANG, ANG, PROCOS;
  double RHO, RHOSQ, NUMER, SINLAT, TOL, APXSIN, X, XX;
  double DENOM, DLON;
  
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
  
  Q203 = .666666666666667E+0;
  Q6011 = .545454545454545E+0;
  Q305 = .6E+0;
  Q407 = .571428571428571E+0;
  Q7013 = .538461538461538E+0;           
  Q509 = .555555555555556E+0;
  Q8015 = .533333333333333E+0;           
  
  /*     ****  Start executable code ****/                                  
  
  /*     PROANG=angle of projection. RHO=Projection radius.                
   *      RHOBAS=A fixed point along the central meridian from which PROANG
   *       is measured.                                                    
   *                                                                       
   *     will check to determine whether or not point lies on central      
   *      meridian --- if not, will calulate longitude from PROANG        
   */
  if ((*EAS - EBASE) != 0)
    {
      PROANG = atan((*EAS-EBASE)/(RHOBAS-*NOR));
      ANG = PROANG/AENRPD;
      DLON = LONCEN - ANG;                                              
      PROCOS = cos (PROANG);
      RHO = (RHOBAS-*NOR)/PROCOS;                                           
      RHOSQ = RHO*RHO;
    }
  else
    {
      DLON = LONCEN;
      RHO = RHOBAS - *NOR;
      RHOSQ = RHO * RHO;
    }
  
  /*
   *     will determine latitude by first picking a reasonable estimate
   *     for SINLAT and then going thru successive iterations to arrive
   *     at a more accurate approximation for SINLAT.
   */
  
  /*                                                                       
   *     The terms 'NUMER' and 'DENOM' are parts of an expression used
   *      to approximate SINLAT --- this expression involves ROSQ, RHOSQ,
   *       TBBON, and a polynomial in XX derived from the series solution
   *        of the intergal from 0 to lat of (cos(p))*DP/(1-ECCSQ)*sin(P)
   *         power(2)/NOR divided by SINLAT ----- P represents latitude
   *       TBBON is the product (2*SEMAJ*(power(2))*(ECCSQ))/*NOR 
							     *                   equal to 2*SEMIN*powr(2))/*NOR.
													   */                                                                       
  NUMER = (ROSQ - RHOSQ)/TBBON;
  /*  Approximate SINLAT by assuming SINLAT*ECC much less than 1 */
  SINLAT = NUMER;
  TOL = 5.E-10 * sqrt (1.0 - (SINLAT*SINLAT));
  while (1)
    {
      APXSIN = SINLAT;
      X = APXSIN * ECC;
      XX = X * X;
      DENOM = (((((((Q8015*XX+Q7013)*XX+Q6011)*XX+Q509)*XX+Q407)*XX+Q305)*XX+Q203)*XX+1.0);
      SINLAT = NUMER / DENOM;
      /*   
       *     If this approximation is not sufficiently close to the previous
       *      value of SINLAT, then this most recent value will be used to
       *       determine a new X which, in turn, will determine still
       *        another approximation for SINLAT.
       */
      if (((fabs(SINLAT-APXSIN)) - TOL) <= 0) break;
    }
  
  *(lat) = asin(SINLAT)/RPD;
  *(lon) = DLON;
  return (0);                                                            
}
