/***********************************************************************
 * GRASS 5.0 gis library
 * get_datum.c, get datum parameters from location database
 *
 * Andreas Lange, andreas.lange@rhein-main.de
 * version 0.9
 * modified Jul 13 2000 
 *
 ***********************************************************************/

#include "gis.h"
#include "glocale.h"
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

static int getvalue(const char *, double *);
static char *PERMANENT = "PERMANENT";

/* G_get_datum_parameters
 *
 * This routine returns the datum parameters from the database.
 * If the PROJECTION_FILE exists in the PERMANENT mapset, read info from
 * that file, otherwise return WGS 84 values.
 *
 * Returns: 1 ok, 0 default values used, -1 on internal error.
 */

/*!
 * \brief get datum parameters from database
 *
 * This function sets the datum parameters for the map datum of the current database.
 * These are the semi-major axis <b>a</b> (in meters), the eccentricity squared
 * <b>e2</b> and the inverse flattening <b>f</b> of the spheroid associated
 * with the database and the x shift <b>dx</b>, the y shift <b>dy</b> and the
 * z shift <b>dz</b> of the map datum associated with the database. If there is
 * no map datum explicitely associated with the actual database, the standard
 * values for the WGS84 spheroid and map datum are set. The funcion returns 1 on
 * success, 0 if the default WGS84 parameters are set. If an error occurs, the
 * function dies with a diagnostic message (HINT: to change, very bad practice to
 * die in a library function!). 
 *
 *  \param a
 *  \param e2
 *  \param f
 *  \param dx
 *  \param dy
 *  \param dz
 *  \return int
 */

int 
G_get_datum_parameters (double *a, double *e2, double *f, 
			double *dx, double *dy, double *dz)
{
  int in_stat, get_parms = 0;
  char *str, *dat, *ellps, err[1024], ipath[1024];
  struct Key_Value *proj_keys;


  G__file_name (ipath, "", PROJECTION_FILE, PERMANENT);
  
  /* no PROJ_INFO file existing, return default wgs84 values */
  if (access(ipath,0) !=0) 
    {
      *a  = 6378137.0 ;
      *e2 = .006694385 ;
      *f  = 298.257223563;
      *dx = 0.0;
      *dy = 0.0;
      *dz = 0.0;
      return 0;
    }
  
  /* read in key values */
  proj_keys = G_read_key_value_file(ipath, &in_stat); 
  if (in_stat !=0)
    {
      sprintf (err, _("Unable to open file %s in %s"),PROJECTION_FILE,PERMANENT);
      G_fatal_error (err);
    }
  
  /* if datum key is existing, process values */
  if ((dat = G_find_key_value("datum",proj_keys))!=NULL) {
    
    str = G_find_key_value("a",proj_keys); 
    get_parms += getvalue(str, a);
      
    str = G_find_key_value("es",proj_keys);
    get_parms += getvalue(str, e2);
      
    str = G_find_key_value("f",proj_keys);
    get_parms += getvalue(str, f);
    
    str = G_find_key_value("dx",proj_keys);
    get_parms += getvalue(str, dx);
    
    str = G_find_key_value("dy",proj_keys);
    get_parms += getvalue(str, dy);
    
    str = G_find_key_value("dz",proj_keys);
    get_parms += getvalue(str, dz);
  
    if (get_parms) {
      if (G_datum_shift(G_get_datum_by_name(dat), dx, dy, dz) == 0) {
	sprintf(err, _("Error reading datum shift parameters for %s from table"), dat);
	G_fatal_error(err);
	return -1;
      }
      /* get ellipsoid parameters from PROJ_INFO */
      ellps = G_find_key_value("ellps",proj_keys);
      if (ellps!=NULL) {
	if (G_get_spheroid_by_name(ellps, a, e2, f) == 0) {
	  sprintf(err, _("Error reading ellipsoid parameters for %s from table"), ellps);
	  G_fatal_error(err);
	  return -1;
	}
      } else {
	sprintf(err, _("No ellipsoid field %s in file %s in %s"), ellps, PROJECTION_FILE,PERMANENT);
	G_fatal_error (err);
	return -1;
      }
    }
    return 1;
  } else {
    /* no datum key in PROJ_INFO, supply wgs84 params */
    *a  = 6378137.0 ;
    *e2 = .006694385 ;
    *f  = 298.257223563;
    *dx = 0.0;
    *dy = 0.0;
    *dz = 0.0;
    return 0;
  }
}

/* placeholder, not yet implemented */

/*!
 * \brief get datum parameters from database
 *
 * This is a placeholder as the 7
 * parameter datum shift support is not implemented yet. 
 *
 *  \param a
 *  \param e2
 *  \param f
 *  \param dx
 *  \param dy
 *  \param dz
 *  \param rx
 *  \param ry
 *  \param rz
 *  \param m
 *  \return int
 */

int
G_get_datum_parameters7(double *a, double *e2, double *f, 
			double *dx, double *dy, double *dz, 
			double *rx, double *ry, double *rz, double *m)
{
  return -1;
}

static int 
getvalue (const char *key, double *value)
{
  char err[512];

  if (key!=NULL) {
    if(sscanf(key,"%lf",value)!=1) {
	sprintf (err, _("invalid value: field %s in file %s in %s")
		 ,key,PROJECTION_FILE,PERMANENT);
	G_fatal_error (err);
    } else {
      return 0;
    }
  }
  return 1;
}
