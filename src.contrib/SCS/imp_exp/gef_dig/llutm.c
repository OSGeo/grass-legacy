/* @(#)llutm.c	1.1   12/04/87 */
/* created by: R.L.Glenn
*      Determines the Northing, 'NOR', Easting, 'EAS', and Zone, 'ZON',
*      in the Universal Transverse Mercator Grid System for the point
*      located by Latitude 'lat' degrees north and
*      Longitude 'lon' degrees west.
*
*      The zone may be fixed by the calling program to facilitate
*      zone overlaps and similar NONstandard zones by using the
*      negative of the zone number as 'ZON' --- This arguement will
*      not be changed by the routine.
*
*      PROGRAMMED FOR North-Western Quadrisphere ONLY
*****/
#include <ctype.h>
#include <stdio.h>
#include <math.h>

static double NCO[] = { 0.0, 
6.367399689169782E+6, -3.250286241790479E+4,  1.384048366716706E+2,
-7.33538130501429E-1,  4.219413411982918E-3, -2.529287942342808E-5,
1.556058285276589E-7, -9.73847437473002E-10 };
static double LCO[] = { 1.0,
 -1.353731599458211E-2,  9.162946216858431E-5,  -3.095662638412468E-5,
  4.148716543194615E-7, -1.268441397827776E-7,   2.158262546320965E-9,
-6.173915584213417E-10, 1.190710620936894E-11, -3.157187252484188E-12 };

llutm(lat,lon,NOR,EAS,ZON) 
	int *ZON ;
	float *lat, *lon;
	double *NOR, *EAS;
{
      int ier;
      double KZERO, SEMAJ, SEMIN, ECC, ECCSQ, EPSQ;
      double NUPHIK, PHI=0.0, FILAT, COSPHI, SINPHI;
      double TANPHI, COSSQ, SINSQ, TANSQ, RPD, PI, FTPERM;
      double SINSEC, SNSC36;
      double ARG, CENLON, P, PSCS, ESCS, FIIIP, A6P, FVP, B5P;
      double FIK, APPARG, PART, Q, QNK, FVII, FVIII;
      double QD6, FXI, FX, QE5;
      
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
      KZERO = .9996;
      FILAT = 0.0;
/*
      Process
*/
      if (*ZON >= 0)
	{
	 *ZON = (186E0 - *lon)/6E0;
	}
      if ((*lat - PHI) != 0)
	{
        PHI = *lat;
        ARG = *lat * RPD;
        COSPHI = cos(ARG);
        COSSQ = COSPHI*COSPHI;
        SINPHI = sin(ARG);
        SINSQ = SINPHI*SINPHI;
        TANSQ = SINSQ/COSSQ;
        TANPHI = SINPHI/COSPHI;
        NUPHIK = KZERO*SEMAJ / sqrt(1E0 - ECCSQ*SINSQ);
        FILAT = (((((((NCO[8]*COSSQ+NCO[7])*COSSQ+NCO[6])*COSSQ+NCO[5])*COSSQ+NCO[4])*COSSQ+NCO[3])*COSSQ+NCO[2])*COSPHI*SINPHI+NCO[1]*ARG)*KZERO;
	}
        CENLON = (30-abs(*ZON))*6E0+3E0;
        P = (CENLON - *lon) * SNSC36;
/*
  *** P IS NEGATIVE WEST OF THE CENTRAL MERIDIAN
*/
        PSCS = P * P * COSSQ;
        ESCS = EPSQ * COSSQ;
        FIIIP = ((4E0*ESCS+9E0)*ESCS+5E0 - TANSQ)*30E0;
        A6P = (TANSQ-58E0)*TANSQ+6E2*ESCS - 330E0*EPSQ+61E0;
        *(NOR) = ((A6P*PSCS+FIIIP)*PSCS+360E0)*PSCS*TANPHI*NUPHIK/720E0+FILAT;
        FVP = (1E0 - TANSQ+ESCS)*20E0;
        B5P = (TANSQ-18E0)*TANSQ+72E0*ESCS - 58E0*EPSQ+5E0;
        *(EAS) = ((B5P*PSCS+FVP) * PSCS +120E0)*P*COSPHI*NUPHIK/120E0+500000E0;
      return ;
}
