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
#include "string.h"
#include <sys/types.h>
#include <sys/stat.h>
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
   ,{ open_old_dummy, V1_open_old_shp, V2_open_old_shp }
#ifdef HAVE_POSTGRES
   ,{ open_old_dummy, V1_open_old_post, V2_open_old_post }
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
*  Returns Level of openness.   [ 1, 2, (3) ] or -1 for error.
*  In case of error, the functions respect fatal error settings.
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

  G_debug (1, "Vect_open_old(): name = %s mapset= %s", name, mapset);
      
  level_request = Open_level;
  Open_level = 0;
  Vect__init_head (Map);
  
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
  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
  G_debug (1, "open format file: '%s/%s/%s", Map->mapset, buf, GRASS_VECT_FRMT_ELEMENT);
  fp = G_fopen_old (buf, GRASS_VECT_FRMT_ELEMENT, Map->mapset);
  if ( fp == NULL) {
      G_debug ( 1, "Vector format: %d (native)", format);
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

  if ( level >= 1 && Vect__read_head (Map) == GRASS_OK ) {
	  Map->open = VECT_OPEN_CODE;
	  Map->level = level;
	  Map->mode = MODE_READ;
	  Map->Constraint_region_flag = 0;
	  Map->Constraint_type_flag = 0;
	  G_debug (1, "Vect_open_old(): vector opened on level %d", level);
  } else {
      level = -1;
      G_debug (1, "Vect_open_old(): vector was not opened");
      switch ( Vect_get_fatal_error () ) {
          case GV_FATAL_EXIT:
              G_fatal_error ( "Cannot open old vector %s", Vect_get_full_name(Map) ); 
	      break;
          case GV_FATAL_PRINT:
              fprintf(stderr, "ERROR: Cannot open old vector %s\n", Vect_get_full_name(Map) ); 
	      break;
          case GV_FATAL_RETURN:
	      break;
      }
      Vect_set_fatal_error (GV_FATAL_EXIT);
  }
      
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
    
    Vect__init_head (Map);

    Map->name = G_store (name);
    Map->mapset = G_store ( G_mapset() );
    
    format = 0;
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);
    fp = G_fopen_old (buf, GRASS_VECT_FRMT_ELEMENT, G_mapset());
    if ( fp == NULL) {
        format = GV_FORMAT_NATIVE;
        G_debug ( 1, "Vector format: %d (native)", format);
    } else {
        format = dig_read_frmt_ascii ( fp, &(Map->fInfo) );
        fclose (fp); 
        G_debug ( 1, "Vector format: %d (non-native)", format);
    }
    Map->format = format;
    
    if (0 > (*Open_new_array[format][1]) (Map, name, with_z)) {
	  switch ( Vect_get_fatal_error () ) {
	      case GV_FATAL_EXIT:
		  G_fatal_error ( "Cannot open new vector %s", Vect_get_full_name(Map) ); 
		  break;
	      case GV_FATAL_PRINT:
		  fprintf(stderr, "ERROR: Cannot open new vector %s\n", Vect_get_full_name(Map) ); 
		  return (-1);
		  break;
	      case GV_FATAL_RETURN:
		  return (-1);
		  break;
	  }
	  Vect_set_fatal_error (GV_FATAL_EXIT);
    }

    Open_level = 0;
    return 1;
}

/*
**  Returns: 1 OK
**           0 error 
*/
int 
Vect_coor_info ( struct Map_info *Map, struct Coor_info *Info )
{
    char buf[2000], path[2000], *ptr;
    struct stat stat_buf;
    int ret;
    
    switch (  Map->format ) {
        case GV_FORMAT_NATIVE :
            sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
	    G__file_name (path, buf, GRASS_VECT_COOR_ELEMENT, Map->mapset);
            G_debug ( 1, "get coor info: %s", path);
	    if (0 != stat (path, &stat_buf)) {
		G_warning ("Could not stat file '%s'\n", path);
		Info->size = -1L;
		Info->mtime = -1L;
	    } else {
		Info->size = (long) stat_buf.st_size;      /* file size */
		Info->mtime = (long) stat_buf.st_mtime;    /* last modified time */
	    }
	    break;
        case GV_FORMAT_SHAPE :
	    strcpy ( buf, Map->fInfo.shp.file ); 
	    ptr = buf + strlen(buf) - 4;
	    if ( (strcmp(ptr,".shp") == 0) || (strcmp(ptr,".SHP") == 0) ) {
	        strcpy ( path, buf ); 
                G_debug ( 1, "get coor info: %s", path);
	        ret = stat (path, &stat_buf);
	        if ( ret != 0 )
		    G_warning ("Could not stat file '%s'\n", path);
	    } else {
                sprintf( path, "%s.shp", buf );
                G_debug ( 1, "get coor info: %s", path);
	        ret = stat (path, &stat_buf);
		if ( ret != 0 ) {
                    sprintf( path, "%s.SHP", buf );
	            ret = stat (path, &stat_buf);
                    G_debug ( 1, "get coor info: %s", path);
		}
	        if ( ret != 0 ) {
                    sprintf( path, "%s[.shp|.SHP]", buf );
		    G_warning ("Could not stat files '%s'\n", path);
		}
	    }
	    if ( ret != 0 ) {
		Info->size = -1L;
		Info->mtime = -1L;
	    } else {
		Info->size = (long) stat_buf.st_size;      /* file size */
		Info->mtime = (long) stat_buf.st_mtime;    /* last modified time */
	    }
	    break;
        case GV_FORMAT_POSTGIS :
 	    Info->size = 0L;
	    Info->mtime = 0L;
	    break;
    }
    G_debug ( 1, "Info->size = %ld, Info->mtime = %ld", Info->size, Info->mtime);
	
    return 1;
}

/* Open topo file.
*  
*  Return: 0 success
*         -1 error */
int 
Vect_open_topo (struct Map_info *Map)
{
    int  err;
    char buf[500];
    FILE *fp;
    struct Coor_info CInfo;
    struct Plus_head *Plus;
    
    G_debug (1, "Vect_open_topo(): name = %s mapset= %s", Map->name, Map->mapset);

    Plus = &(Map->plus);
    
    
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    fp = G_fopen_old (buf, GV_TOPO_ELEMENT, Map->mapset);

    if ( fp == NULL ) { /* topo file is not available */
	G_debug( 1, "Cannot open topo file for vector '%s@%s'.\n", 
		      Map->name, Map->mapset);
	return -1;
    }
  
    /* get coor info */ 
    Vect_coor_info ( Map, &CInfo); 

    /* load head */
    dig_Rd_Plus_head (fp, Plus);
    G_debug ( 1, "Topo head: coor size = %ld, coor mtime = %ld", 
	                              Plus->coor_size, Plus->coor_mtime);

    /* do checks */
    err = 0;
    if ( CInfo.size != Plus->coor_size ) {
	G_warning ( "Size of 'coor' file differs from value saved in topo file.\n");
	err = 1;
    }
    /* Do not check mtime because mtime is changed by copy */
    /*
    if ( CInfo.mtime != Plus->coor_mtime ) {
	G_warning ( "Time of last modification for 'coor' file differs from value saved in topo file.\n");
	err = 1;
    }
    */
    if ( err ) {
	G_warning ( "Please rebuild topology for vector '%s@%s'\n", Map->name,
	                          Map->mapset );
	return -1;
    }
    
    /* load topo to memory */
    dig_init_plus ( Plus );    
    dig_load_plus ( Plus, fp );    
   
    fclose ( fp );  

    return 0;
}

