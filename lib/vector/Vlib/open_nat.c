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
#include <unistd.h>
#include "Vect.h"
#include "gis.h"

#include <sys/types.h>
#include <sys/stat.h>

static char name_buf[1024];

/* Open old file.
*  Map->name and Map->mapset must be set before
*  
*  Return: 0 success
*         -1 error */
int 
V1_open_old_nat ( struct Map_info *Map )
{
  int  ret;
  char buf[500];
  FILE *fp;

  
  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
  Map->dig_fp = G_fopen_old (buf, GRASS_VECT_COOR_ELEMENT, Map->mapset);

  if ( Map->dig_fp == NULL ) return -1;

  if ( !(dig__read_head (Map)) ) return (-1);

  /* set conversion matrices */
  dig_init_portable ( &(Map->head.port), Map->head.port.byte_order );
  
  return (0);
}

/* Open new file.
*  
*  Return: 0 success
*         -1 error */
int 
V1_open_new_nat (
	      struct Map_info *Map,
	      char *name,
	      int with_z)
{
  char buf[200];
  FILE *fp;
  struct stat info;


  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);

  /* TODO open better */
  Map->dig_fp = G_fopen_new (buf, GRASS_VECT_COOR_ELEMENT);
  if ( Map->dig_fp == NULL ) return (-1);
  fclose ( Map->dig_fp );
  Map->dig_fp = G_fopen_modify (buf, GRASS_VECT_COOR_ELEMENT);
  if ( Map->dig_fp == NULL ) return (-1);

  /* check to see if dig_plus file exists and if so, remove it */
  G__file_name (name_buf, buf, GV_TOPO_ELEMENT, G_mapset ());
  if (stat (name_buf, &info) == 0)	/* file exists? */
       unlink (name_buf);

  G__file_name (name_buf, buf, GRASS_VECT_COOR_ELEMENT, G_mapset ());
  Map->digit_file = G_store (name_buf);		/*need? */

  Map->name = G_store (name);
  Map->mapset = G_store (G_mapset ());
  Map->open = VECT_OPEN_CODE;
  Map->level = LEVEL_1;
  Map->mode = MODE_WRITE;
  Map->Constraint_region_flag = 0;	/* these do not apply to to write, but */
  Map->Constraint_type_flag = 0;	/* init them anyway                   */

  Vect__init_head (&(Map->head));
  Map->head.with_z = with_z;
  Vect__write_head (Map);

  /* set conversion matrices */
  dig_init_portable ( &(Map->head.port), dig__byte_order_out ());

  if ( !(dig__write_head (Map)) ) return (-1);
  
  return 0;
}


/* Open old file on level 2.
*  Map->name and Map->mapset must be set before
*  
*  Return: 0 success
*         -1 error */
int 
V2_open_old_nat (struct Map_info *Map)
{
    int  ret;
    char buf[500];
    FILE *fp;
    
    G_debug (1, "V2_open_old_nat(): name = %s mapset= %s", Map->name, Map->mapset);

    /* check if topo is available */
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    fp = G_fopen_old (buf, GV_TOPO_ELEMENT, Map->mapset);

    if ( fp == NULL ) { /* topo file is not available */
	G_debug( 1, "Cannot open topo file for vector '%s@%s'.\n", 
		      Map->name, Map->mapset);
	return -1;
    }
    
    /* open dig file */
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    Map->dig_fp = G_fopen_old (buf, GRASS_VECT_COOR_ELEMENT, Map->mapset);
    if ( Map->dig_fp == NULL ) {
        fclose ( fp );  
	return -1;
    }
    if ( !(dig__read_head (Map)) ) return (-1);
    /* set conversion matrices */
    dig_init_portable ( &(Map->head.port), Map->head.port.byte_order );
    

    /* load topo to memory */
    dig_init_plus ( &(Map->plus) );    
    dig_load_plus ( &(Map->plus), fp );    
   
    fclose ( fp );  
    /*
    if (NULL != Vect__P_init (Map, name, mapset))
    {
      return -1;
    }
    */
    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_2;
    Map->mode = MODE_READ;

    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag = 0;
    Map->next_line = 1;

    return 0;
}

