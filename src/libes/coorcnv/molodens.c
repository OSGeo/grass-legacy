/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       coorcnv library 
 * AUTHOR(S):    Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE: 	 molodensky and other datum shift functions
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/*********************************************************************** 
 *
 * Implemented by Andreas Lange, andreas.lange@rhein-main.de 
 * literature used:
 * Seeber, G. (1989): Satellitengeodaesie. Grundlagen, 
 * Methoden und Anwendungen. - W. de Gruyter, Berlin. 
 *
 * Bauer, M. (1997): Vermessung und Ortung mit Satelliten, NAVSTAR-GPS
 * und andere satellitengestützte Navigationssysteme. - Herbert
 * Wichmann Verlag, Heidelberg. 
 * (in german, sorry). 
 *
 * Other references are cited in the source code. 
 *
 * Parts of the code are inspired by code seen in the JEEPS
 * Library (copyright Alan Bleasby, ableasby@hgmp.mrc.ac.uk). 
 *
 * Other parts of the code are inspired by code seen in the
 * NIMA geotrans packet (copyright NIMA). 
 *
 * There are no compound data types defined for this library as
 * the other functions from the CC lib do not use elaborate data
 * types either. This is cumbersome to type, but it is consistent
 * within the other functions in GRASS. 
 *
 ***********************************************************************/

#include "gis.h"
#include "CC.h"
/* for definition of PI and others: */
#include "projects.h" 
#include <math.h>
/* local includes, only for this library: */
#include "cc.h"


static double deg2rad(double);
static double rad2deg(double);

int 
CC_datum_shift_CC(double Sphi, double Slam, double Sh, 
		  double Sa, double Se2, 
		  double *Dphi, double *Dlam, double *Dh, 
		  double Da, double De2, 
		  double dx, double dy, double dz)
{
  double x, y, z;

  /* The Block Shift does not work for the North or South Pole,  
   * where lon = 0 and lat +/- 90 degree,
   * but usually those points are not shifted with
   * different datums IMHO. */

  if ( ( Slam == 0.0 ) && (( Sphi < -MOLODENSKY_MAX ) || ( Sphi > MOLODENSKY_MAX )) ) {
    return 1;
  } 

  if (CC_lld2geo(Sa, Se2, Sphi, Slam, Sh, &x, &y, &z) == 0)
    return 0;

  x = x + dx;
  y = y + dy;
  z = z + dz;
  
  if (CC_geo2lld(Da, De2, x, y, z, Dphi, Dlam, Dh) == 0)
    return 0;

  return 1;

}

int 
CC_datum_to_datum_shift_CC(int Sdatum, 
			   double Sphi, double Slam, double Sh, 
			   int Ddatum, 
			   double *Dphi, double *Dlam, double *Dh)
{
  double Sa, Se2, Da, De2;
  double Sdx, Sdy, Sdz, Ddx, Ddy, Ddz;
  double x, y, z;
  char *Sname, Sellips[50];
  char *Dname, Dellips[50];

  if ( (Sname = CC_get_datum_by_nbr(Sdatum)) == NULL )
    return 0;
  if ( (CC_get_datum_parameters(Sname, Sellips, &Sdx, &Sdy, &Sdz)) == 0)
    return 0;
  if ( (CC_get_spheroid(Sellips, &Sa, &Se2)) == 0 )
    return 0;

  if ( (Dname = CC_get_datum_by_nbr(Ddatum)) == NULL )
    return 0; 
  if ( (CC_get_datum_parameters(Dname, Dellips, &Ddx, &Ddy, &Ddz)) == 0)
    return 0;
  if ( (CC_get_spheroid(Dellips, &Da, &De2)) == 0 )
    return 0;

  /* The Block Shift does not work for the North or South Pole,  
   * where lon = 0 and lat +/- 90 degree,
   * but usually those points are not shifted with
   * different datums IMHO. */

  if ( ( Slam == 0.0 ) && (( Sphi < -MOLODENSKY_MAX ) || ( Sphi > MOLODENSKY_MAX )) ) {
    return 1;
  } 
  
  if (CC_lld2geo(Sa, Se2, Sphi, Slam, Sh, &x, &y, &z) == 0)
    return 0;

  /* from source datum to wgs84 and from wgs84 to destination datum */
  x = x + Sdx + (-Ddx);
  y = y + Sdy + (-Ddy);
  z = z + Sdz + (-Ddz);

  if (CC_geo2lld(Da, De2, x, y, z, Dphi, Dlam, Dh) == 0)
    return 0;
  
  return 1;

}

/* a full description of the molodensky transformation is found at:
 *   http://www.utexas.edu/depts/grg/gcraft/notes/datum/gif/molodens.gif
 *   http://www.anzlic.org.au/icsm/gdatum/molodens.html
 * CAVEAT: Please check the accuracy you need for your equations!
 * I explicitly exclude any responsibility for this code!
 */

int 
CC_datum_shift_Molodensky(double Sphi, double Slam, double Sh, 
			  double Sa, double Se2, double rSf,
			  double *Dphi, double *Dlam, double *Dh, 
			  double Da, double De2, double rDf,
			  double dx, double dy, double dz)
{
  double bda, da, df, dh, Sf, Df; 
  double sinphi, sinlam;
  double dphi, dlam, coslam, cosphi;
  double tmp1, tmp2, tmp3; 
  double Rn, Rm; 

  /* rSf and rDf are the inverse flattening */

  /* e2 = esq = f+f - f*f 
   * we could compute these values, but as we get them from the 
   * table it is faster/simpler to supply them to the function 
   */

  /* the molodensky formula does not work for points near the poles
   * e. g. north of 89.75N and south of 89.75S. 
   * So we use a Block Shift on these values */

  if ( (Sphi < -MOLODENSKY_MAX) || (Sphi > MOLODENSKY_MAX) ) {
    if (CC_datum_shift_CC(Sphi, Slam, Sh, Sa, Se2,
			  Dphi, Dlam, Dh, 
			  Da, De2, dx, dy, dz) == 0)
      return 0;

    return 1;
  }

  Sf = (double)1.0 / rSf;
  Df = (double)1.0 / rDf;

  bda  = (double)1.0 - Sf;
  Sphi = deg2rad(Sphi);
  Slam = deg2rad(Slam);

  da = Da - Sa;
  df = Df - Sf;

  sinphi = sin(Sphi); 
  cosphi = cos(Sphi); 
  sinlam = sin(Slam); 
  coslam = cos(Slam);

  Rn   = Sa / sqrt((double)1.0 - Se2 * sinphi * sinphi);
  tmp1 = ((double)1.0 - Se2 * sinphi * sinphi);
  Rm   = Sa * ((double)1.0 - Se2) / sqrt( (tmp1 * tmp1 * tmp1) );

  tmp1 = ((-dx * sinphi * coslam - dy * sinphi * sinlam) + dz * cosphi);
  tmp2 = (da * (Rn * Se2 * sinphi * cosphi) / Sa);
  tmp3 = df * (Rm / bda + Rn * bda) * sinphi * cosphi;
    
  dphi = ((tmp1 + tmp2) + tmp3) / (Rm + Sh);
  dlam = (- dx * sinlam + dy * coslam) / ((Rn + Sh) * cosphi);
  dh   = dx * cosphi * coslam + dy * cosphi * sinlam + 
         dz * sinphi - da * (Sa/Rn) + 
         df * bda * Rn * sinphi * sinphi;

  *Dphi = Sphi + dphi;
  *Dlam = Slam + dlam;
  *Dh   = Sh + dh;
  
  *Dphi = rad2deg(*Dphi);
  *Dlam = rad2deg(*Dlam);

  return 1;

}

int 
CC_datum_to_datum_shift_M(int Sdatum, 
			  double Sphi, double Slam, double Sh, 
			  int Ddatum, 
			  double *Dphi, double *Dlam, double *Dh)
{
  double Sa, Se2, Sf, Da, De2, Df;
  double Sdx, Sdy, Sdz, Ddx, Ddy, Ddz;
  double x, y, z;
  char *Sname, Sellips[50];
  char *Dname, Dellips[50];

  if ( (Sname = CC_get_datum_by_nbr(Sdatum)) == NULL )
    return 0;
  if ( (CC_get_datum_parameters(Sname, Sellips, &Sdx, &Sdy, &Sdz)) == 0)
    return 0; 
  if ( (CC_get_spheroid_by_name(Sellips, &Sa, &Se2, &Sf)) == 0 )
    return 0;
  if ( (Dname = CC_get_datum_by_nbr(Ddatum)) == NULL )
    return 0;
  if ( (CC_get_datum_parameters(Dname, Dellips, &Ddx, &Ddy, &Ddz)) == 0)
    return 0;
  if ( (CC_get_spheroid_by_name(Dellips, &Da, &De2, &Df)) == 0 )
    return 0;

  /* problem: we get f=(double)-1.0 if there is no value in database, 
   * so we should check for this and return an error
   */

  if (Sf < 0)
    return 0;
  if (Df < 0)
    return 0;

  x = Sdx + (-Ddx);
  y = Sdy + (-Ddy);
  z = Sdz + (-Ddz);

  if (CC_datum_shift_Molodensky(Sphi, Slam, Sh, 
				Sa, Se2, Sf, 
				Dphi, Dlam, Dh, 
				Da, De2, Df, 
				x, y, z) == 0)
    return 0;
  
  return 1;

}

/* for a description of the Bursa Wolf transformation see:
 *  http://www.posc.org/Epicentre.2_2/DataModel/ExamplesofUsage/eu_cs35.html
 *
 * see also:
 *  source code of NIMA geotrans, esp. file dt_cc/datum/datum.c
 *
 * CAVEAT: check which sign convention you must use as european and
 * australian/american users use different systems.
 * If you have no rotational parameters and/or no 
 * scaling parameter you should use Molodensky or
 * block transformation. 
 * The Scale value is expressed as ppm (parts per million). 
 * Check if you really need an accuracy as high as with this! 
 */

int 
CC_datum_shift_BursaWolf(double Sphi, double Slam, double Sh, 
			 double Sa, double Se2, 
			 double *Dphi, double *Dlam, double *Dh,
			 double Da, double De2, 
			 double dx, double dy, double dz,
			 double Rx, double Ry, double Rz,
			 double Scale) 
{
  double M;           /* Scale               */
  double x, y, z;     /* shifted coordinates */

  /* Scale is expressed in ppm */
  if ( Scale < 0 )
    return 0;
  if ( Scale >= 10000 )
    return 0;

  M = (double)(Scale / 1.0e06);

  Rx /= RADIANS_TO_SECONDS;
  Ry /= RADIANS_TO_SECONDS;
  Rz /= RADIANS_TO_SECONDS;

  if (CC_lld2geo(Sa, Se2, Sphi, Slam, Sh, &x, &y, &z) == 0)
    return 0;

  /* Bursa-Wolf transformation or Helmert transformation,
   * sometimes referred to as 3d similarity transformation, 
   * european and american/australian users do not agree on the
   * signs of the rotational parameters RX, RY and RZ!
   *  XB = M  (  1   - RZ  + RY)  XA + dx
   *  YB = M  (+ RZ  +  1  - RX)  YA + dy
   *  ZB = M  (- RY  + RX    1 )  ZA + dz
   */
  
  x = x + dx - Rz * y - Ry * z + ( M * x );
  y = y + dy - Rz * x + Rx * z + ( M * y );
  z = z + dz + Ry * x - Rx * y - ( M * z ); 

  if (CC_geo2lld(Da, De2, x, y, z, Dphi, Dlam, Dh) == 0)
    return 0;

  return 1;

}
 
/* this is not possible because the datum database 
 * currently provides no values
 * for the rotational parameters and the Scale parameter!
 *
 * for now just a placeholder!
 */

int 
CC_datum_to_datum_shift_BW(int Sdatum, 
			   double Sphi, double Slam, double Sh, 
			   int Ddatum, 
			   double *Dphi, double *Dlam, double *Dh)
{
  double Sa, Se2, Da, De2;
  double Sdx, Sdy, Sdz, Ddx, Ddy, Ddz;
  double Rx, Ry, Rz, Scale;
  double x, y, z;
  char *Sname, Sellips[50];
  char *Dname, Dellips[50];
  
  if ( (Sname = CC_get_datum_by_nbr(Sdatum)) == NULL )
    return 0;
  if ( (CC_get_datum_parameters(Sname, Sellips, &Sdx, &Sdy, &Sdz)) == 0)
    return 0; 
  if ( (CC_get_spheroid(Sellips, &Sa, &Se2)) == 0 )
    return 0;
  if ( (Dname = CC_get_datum_by_nbr(Ddatum)) == NULL )
    return 0;
  if ( (CC_get_datum_parameters(Dname, Dellips, &Ddx, &Ddy, &Ddz)) == 0)
    return 0;
  if ( (CC_get_spheroid(Dellips, &Da, &De2)) == 0 )
    return 0;

  x = Sdx + (-Ddx);
  y = Sdy + (-Ddy);
  z = Sdz + (-Ddz);
  Rx = 0;
  Ry = 0;
  Rz = 0;
  Scale = 0;
  
  if (CC_datum_shift_BursaWolf(Sphi, Slam, Sh, Sa, Se2, 
			       Dphi, Dlam, Dh, Da, De2, 
			       x, y, z, Rx, Ry, Rz, Scale) == 0)
    return 0;
  
  return 1;  

}

static double 
deg2rad(double val)
{

  return val*(double)((double)PI/(double)180.0);

}

static double 
rad2deg(double val)
{

  return val*(double)((double)180.0/(double)PI);

}
