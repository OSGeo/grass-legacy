/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE: 	 coorcnv library      
 * AUTHOR(S):    Original author unknown - probably CERL
 *               Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE:      spheroid/ellipsoid related functions
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include "CC.h"
#include "gis.h"

static int same(const char *, const char *);

/* see also G_ask_ellipse_name */

/* old: get parameters for spheroid pointed to by name 
 */
int 
CC_get_spheroid (const char *name, double *a, double *e2)
{
  if (G_get_ellipsoid_by_name(name, a, e2) == 0)
    return 0;
  return 1;
}

/* new: get number of spheroid pointed to by name 
 * returns number of spheroid on success, -1 else 
 */
int 
CC_get_spheroid_by_name (const char *name, double *a, double *e2, double *f)
{
  int i;
  char *test;  

  for (i = 0; ; i++) {
    if ( (test = G_ellipsoid_name(i)) == NULL) 
      break;
    if (same(test,name)) {
      (void) G_get_spheroid_by_name(name, a, e2, f);
      return i;
    }
  }
  return -1;
}

/* get the name of spheroid number n from table 
 */
char *
CC_get_spheroid_by_nbr (int n)
{
  char *name;
  
  if ( (name = G_ellipsoid_name(n)) == NULL )
    return NULL;
  return name;
}

/* old: this is to maintain backward compatibility 
 */
char *
CC_spheroid_name (int n)
{
  char *name;
  
  if ( (name = G_ellipsoid_name(n)) == NULL )
    return NULL;
  return name;
}

static int 
same(const char *a, const char *b)
{
  while (*a && *b)
    if (tolower(*a++) != tolower (*b++))
      return 0;
  return (*a == 0 && *b == 0);
}


