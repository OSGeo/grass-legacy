/* @(#)utmll.c	1.1   12/04/87 */
/* created by: R.L.Glenn
*      Determines the Latitude 'lat' and Longitude 'lon' for the point
*      located NOR meters NORTH and EAS meters EAST in ZON of the
*      Universal Transverse Mercator Grid System.
*      Programmed for North-western Quadrisphere ONLY
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

utmll(lat,lon,NOR,EAS,ZON) 
	int *ZON ;
	float *lat, *lon;
 	double *NOR, *EAS;
{
      int ier;
      double KZERO, SEMAJ, SEMIN, ECC, ECCSQ, EPSQ;
      double NUPHIK, PHI, FILAT, COSPHI, SINPHI;
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
      if ((*NOR - FILAT) != 0)
         {
         FIK = *NOR/KZERO;
         ARG=FIK/NCO[1];
         COSPHI=cos(ARG);
         COSSQ=COSPHI*COSPHI;
         SINPHI=sin(ARG);
	 while(1)
	   {
           APPARG=ARG;
           PART=((((((NCO[8]*COSSQ+NCO[7])*COSSQ+NCO[6])*COSSQ+NCO[5])*COSSQ+NCO[4])*COSSQ+NCO[3])*COSSQ+NCO[2])*COSPHI*SINPHI;
           ARG=(FIK-PART)/NCO[1];
           COSPHI=cos(ARG);
           COSSQ=COSPHI*COSPHI;
           SINPHI=sin(ARG);
           if ( fabs(APPARG-ARG) <= 5.E-10) break;
  	   }
         SINSQ=SINPHI*SINPHI;
         NUPHIK=KZERO*SEMAJ/sqrt(1.0-ECCSQ*SINSQ);
         TANSQ=SINSQ/COSSQ;
         TANPHI=SINPHI/COSPHI;
         PHI=ARG/RPD;
         FILAT=(((((((NCO[8]*COSSQ+NCO[7])*COSSQ+NCO[6])*COSSQ+NCO[5])*COSSQ+NCO[4])*COSSQ+NCO[3])*COSSQ+NCO[2])*COSPHI*SINPHI+NCO[1]*ARG)*KZERO;
         }


/**** Q is negative East of Central Meridian  */
         Q=0.5-*EAS*1.E-6;
         QNK=(Q/NUPHIK)*(Q/NUPHIK);
         ESCS=EPSQ*COSSQ;
         FVII=(ESCS+1.0)*5.E+11;
         FVIII=((((ESCS+2.)*(COSSQ-SINSQ)-ESCS-ESCS)*EPSQ+TANSQ)*3.0+5.0)/24.E-24;
         QD6=((COSSQ*107.0-SINSQ*162.0)*EPSQ+(2.0+TANSQ-EPSQ+ESCS)*45.*TANSQ+61.0)/720.E-36;
         *(lat) = PHI-((QD6*QNK-FVIII)*QNK+FVII)*QNK*TANPHI/SNSC36;
         FXI=1E6;
         FX=(1E0+TANSQ+TANSQ+ESCS)/6E-18;
         QE5=((24E0*TANSQ+28E0)*TANSQ-2E0*ESCS+8E0*EPSQ+5E0)/12E-30;
         *(lon) = (30-*ZON)*6E0+3E0+((QE5*QNK-FX)*QNK+1E6)*Q/(COSPHI*SNSC36*NUPHIK);
      return ;
}
