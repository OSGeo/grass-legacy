/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include "stdio.h"
#include "gis.h"
#include "Vect.h"
/*

   Routines:    
   Vect_open_old (Map, name, mapset)
   Vect_open_new (Map, name)
   Vect_rewind (Map)
   Vect_close (Map)

   These routines all just call the V# equivalents to pass the function
   off to more level-specific code.
 */

#define MAX_OPEN_LEVEL 2

static int open_old_dummy () { return 0; }
static int open_new_dummy () { return 0; }

static int Open_level = 0;

static int (*Open_old_array[][3]) () =
{
    { open_old_dummy, V1_open_old_nat, V2_open_old_nat }
   ,{ open_old_dummy, V1_open_old_shp }
#ifdef HAVE_POSTGRES
   ,{ open_old_dummy, V1_open_old_post }
#endif
};

static int (*Open_new_array[][2]) () =
{
    { open_new_dummy, V1_open_new_nat }
   ,{ open_new_dummy, V1_open_new_shp }
#ifdef HAVE_POSTGRES
   ,{ open_new_dummy, V1_open_new_post }
#endif
};

/*
   **  Predetermine level you want to open for read at.  If it can't open
   **  that level, the open will fail.  The specified level must be
   **  set before any call to open.  The default is to try to open
   **  the highest level possible, and keep stepping down until success.
   **
   **  NOTE!!  This should only be used to set when you wish to force
   **  a lower level open.  If you require a higher level, then just
   **  check the return to verify the level instead of forcing it.
   **  This is because future releases will have higher levels which
   **  will be downward compatible and which your programs should 
   **  support by default.
 */
int 
Vect_set_open_level (int level)
{
  Open_level = level;
  if (Open_level < 1 || Open_level > MAX_OPEN_LEVEL)
    {
      fprintf (stderr, "Warning, Programmer requested unknown open_level\n");
      Open_level = 0;
    }

  return 0;
}

/*
   ** returns Level of openness.   [ 1, 2, (3) ]
   ** and -1 on error
 */
int
Vect_open_old (
		struct Map_info *Map,
		char *name,
		char *mapset)
{
  char buf[200], buf2[200], xname[512], xmapset[512], name_buf[1024];
  FILE *fp;
  int level, level_request;
  int format;

#ifdef GDEBUG
      G_debug (1, "Vect_open_old(): name = %s mapset= %s", name, mapset);
#endif
      
  level_request = Open_level;
  Open_level = 0;
  
  if (G__name_is_fully_qualified (name, xname, xmapset)) {
      sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, xname);
      sprintf (buf2, "%s@%s", GRASS_VECT_COOR_ELEMENT, xmapset); /* ==coor@mapset */
       
      Map->name = G_store (xname);
      Map->mapset = G_store (xmapset);
  } else {
      sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);
      sprintf (buf2, "%s", GRASS_VECT_COOR_ELEMENT);
      Map->name = G_store (name);
      Map->mapset = G_store (mapset);
  }

  G__file_name (name_buf, buf, buf2, mapset);
  Map->digit_file = G_store (name_buf);  
  
  /* Read vector format information */
  format = 0;
  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);
  fp = G_fopen_old (buf, GRASS_VECT_FRMT_ELEMENT, mapset);
  if ( fp == NULL) {
#ifdef GDEBUG
      G_debug ( 1, "Vector format: %d (native)", format);
#endif
      format = GV_FORMAT_NATIVE;
  } else {
      format = dig_read_frmt_ascii ( fp, &(Map->fInfo) );
      fclose (fp); 
      
      G_debug ( 1, "Vector format: %d (non-native)", format);
  }
  Map->format = format;
    
#ifndef HAVE_POSTGRES
  if ( Map->format == GV_FORMAT_POSTGIS )
      G_fatal_error ("PostGIS support is not compiled in GRASS vector library.\n");
#endif
  
  if (level_request) {
      level = level_request;
      G_debug ( 1, "Level request = %d", level_request);
      if (0 != (*Open_old_array[format][level_request]) (Map))
	level = -1;
  } else {
      for (level = MAX_OPEN_LEVEL; level; level--)
	  if (0 == (*Open_old_array[format][level]) (Map)) {
	      break;
	  }
  }

  if ( level >= 1 ) {
      if ( (Vect__read_head (Map)) != GRASS_OK ) return (-1);
      Map->open = VECT_OPEN_CODE;
      Map->level = level;
      Map->mode = MODE_READ;
      Map->Constraint_region_flag = 0;
      Map->Constraint_type_flag = 0;
  }
      
#ifdef GDEBUG
      G_debug (1, "Vect_open_old(): vector opened on level %d", level);
#endif
  return (level);
}

/*
**  Returns level  [ 1 ]  or -1 on error 
*/
int 
Vect_open_new (
		struct Map_info *Map,
		char *name,
		int with_z)
{
    int format;
    char buf[200];
    FILE *fp;
    
    format = 0;
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);
    fp = G_fopen_old (buf, GRASS_VECT_FRMT_ELEMENT, G_mapset());
    if ( fp == NULL) {
#ifdef GDEBUG
        G_debug ( 1, "Vector format: %d (non-native)", format);
#endif
        format = GV_FORMAT_NATIVE;
    } else {
        format = dig_read_frmt_ascii ( fp, &(Map->fInfo) );
        fclose (fp); 
#ifdef GDEBUG
        G_debug ( 1, "Vector format: %d (non-native)", format);
#endif
    }
    Map->format = format;
    
    if (0 > (*Open_new_array[format][1]) (Map, name, with_z))
        return -1;

    Open_level = 0;
    return 1;
}

