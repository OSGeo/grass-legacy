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
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"


int 
Vect_print_header (struct Map_info *Map)
{
  fprintf (stdout, "\nSelected information from dig header\n");
  fprintf (stdout, " Organization:  %s\n", Vect_get_organization(Map) );
  fprintf (stdout, " Map Name:      %s\n", Vect_get_map_name(Map) );
  fprintf (stdout, " Source Date:   %s\n", Vect_get_map_date(Map) );
  fprintf (stdout, " Orig. Scale:   %d\n", Vect_get_scale(Map)  );

  return 0;
}

/* Vect__write_head () writes head information to text file.
 * returns: GRASS_OK - success
 *          GRASS_ERR - error
 */
int
Vect__write_head (struct Map_info *Map)
{
    char buf[200];	
    FILE *head_fp;

    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);

    head_fp = G_fopen_new (buf, GRASS_VECT_HEAD_ELEMENT);
    if ( head_fp == NULL)
      {
        G_warning ("Cannot Open Vector %s@%s Head File\n", Map->name, Map->mapset);
        return (GRASS_ERR);
      }
	
    fprintf (head_fp, "ORGANIZATION: %s\n", Vect_get_organization(Map) );
    fprintf (head_fp, "DIGIT DATE:   %s\n", Vect_get_date(Map) );
    fprintf (head_fp, "DIGIT NAME:   %s\n", Vect_get_person(Map) );
    fprintf (head_fp, "MAP NAME:     %s\n", Vect_get_map_name(Map) );
    fprintf (head_fp, "MAP DATE:     %s\n", Vect_get_map_date(Map) );
    fprintf (head_fp, "MAP SCALE:    %d\n", Vect_get_scale(Map) );
    fprintf (head_fp, "OTHER INFO:   %s\n", Vect_get_comment(Map) );
    fprintf (head_fp, "ZONE:         %d\n", Vect_get_zone(Map) );
    fprintf (head_fp, "MAP THRESH:   %f\n", Vect_get_thresh(Map) );
    
    fclose (head_fp);
    return (GRASS_OK);
}

/* Vect__read_head () reads head information from text file.
 * returns: GRASS_OK - success
 *          GRASS_ERR - error
 */
int
Vect__read_head (struct Map_info *Map)
{
    FILE *head_fp;
    char buff[1024];
    char *ptr;

    G_debug (1, "Vect__read_head(): vector = %s@%s", Map->name, Map->mapset);
    sprintf (buff, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    head_fp = G_fopen_old (buff, GRASS_VECT_HEAD_ELEMENT, Map->mapset); 
    if ( head_fp == NULL)
      {
        G_warning ("Cannot Open Vector %s Head File\n", Map->name);
        return (GRASS_ERR);
      }
   
    while ( NULL != fgets (buff, sizeof (buff), head_fp)) {

	for (ptr = buff; *ptr != '\n'; ptr++);	/* Remove new-line char */
	*ptr = '\0';

	if (!(ptr = G_index (buff, ':')))
	return (-1);
	ptr++;			/* Search for the start of text */
	while (*ptr == ' ')
	ptr++;

	if (strncmp (buff, "ORGANIZATION:", 12) == 0)
	  Vect_set_organization ( Map, ptr );  
	else if (strncmp (buff, "DIGIT DATE:", 11) == 0)
	  Vect_set_date ( Map, ptr );  
	else if (strncmp (buff, "DIGIT NAME:", 11) == 0)
	  Vect_set_person ( Map, ptr );  
	else if (strncmp (buff, "MAP NAME:", 9) == 0)
	  Vect_set_map_name ( Map, ptr );  
	else if (strncmp (buff, "MAP DATE:", 9) == 0)
	  Vect_set_map_date ( Map, ptr );  
	else if (strncmp (buff, "MAP SCALE:", 10) == 0)
	  Vect_set_scale ( Map, atoi (ptr) );  
	else if (strncmp (buff, "OTHER INFO:", 11) == 0)
	  Vect_set_comment ( Map, ptr );  
	else if (strncmp (buff, "ZONE:", 5) == 0 || strncmp (buff, "UTM ZONE:", 9) == 0)
	  Vect_set_zone ( Map, atoi (ptr) );  
	else if (strncmp (buff, "WEST EDGE:", 10) == 0) {}
	else if (strncmp (buff, "EAST EDGE:", 10) == 0) {}
	else if (strncmp (buff, "SOUTH EDGE:", 11) == 0) {}
	else if (strncmp (buff, "NORTH EDGE:", 11) == 0) {}
	else if (strncmp (buff, "MAP THRESH:", 11) == 0)
	  Vect_set_thresh ( Map, atof (ptr) );  
	else 
	  G_warning("Unknown keyword %s in vector head\n", buff);
    }
    
    fclose (head_fp);
    return (GRASS_OK);
}


/* set and get header informations */
/* name, mapset, full name */
char *
Vect_get_name (struct Map_info *Map)
{
    return (Map->name);
}

char *
Vect_get_mapset (struct Map_info *Map)
{
    return (Map->mapset);
}

char *
Vect_get_full_name (struct Map_info *Map)
{
    char *ptr;

    ptr = G_malloc ( strlen(Map->name) +  strlen(Map->mapset) + 2 );
    sprintf (ptr, "%s@%s", Map->name, Map->mapset);
    return (ptr);
}

/* Is 3D (with z ) ? */
int
Vect_is_3d (struct Map_info *Map )
{
    return ( Map->head.with_z );
}

/* organization */
int
Vect_set_organization (struct Map_info *Map, char *str )
{
    G_free ( Map->head.organization );
    Map->head.organization = G_store ( str );
    return (0);
}

char *
Vect_get_organization (struct Map_info *Map)
{
    return (Map->head.organization);
}

/* date of digitization */
int
Vect_set_date (struct Map_info *Map, char *str )
{
    G_free ( Map->head.date );
    Map->head.date = G_store ( str );
    return (0);
}

char *
Vect_get_date (struct Map_info *Map)
{
    return (Map->head.date);
}

/* user who digitized the map */
int
Vect_set_person (struct Map_info *Map, char *str )
{
    G_free ( Map->head.your_name );
    Map->head.your_name = G_store ( str );
    return (0);
}

char *
Vect_get_person (struct Map_info *Map)
{
    return (Map->head.your_name);
}

/* map name */
int
Vect_set_map_name (struct Map_info *Map, char *str )
{
    G_free ( Map->head.map_name );
    Map->head.map_name = G_store ( str );
    return (0);
}

char *
Vect_get_map_name (struct Map_info *Map)
{
    return (Map->head.map_name);
}

/* date when the source map was originally produced */
int
Vect_set_map_date (struct Map_info *Map, char *str )
{
    G_free ( Map->head.source_date );
    Map->head.source_date = G_store ( str );
    return (0);
}

char *
Vect_get_map_date (struct Map_info *Map)
{
    return (Map->head.source_date);
}

/* scale */
int
Vect_set_scale (struct Map_info *Map, int scale )
{
    Map->head.orig_scale = scale;
    return (0);
}

int
Vect_get_scale (struct Map_info *Map)
{
    return ((int) Map->head.orig_scale);
}

/* Comment, other info */
int
Vect_set_comment (struct Map_info *Map, char *str )
{
    G_free ( Map->head.line_3 );
    Map->head.line_3 = G_store ( str );
    return (0);
}

char *
Vect_get_comment (struct Map_info *Map)
{
    return (Map->head.line_3);
}

/* Zone */
int
Vect_set_zone (struct Map_info *Map, int zone )
{
    Map->head.plani_zone = zone;
    return (0);
}

int
Vect_get_zone (struct Map_info *Map)
{
    return (Map->head.plani_zone);
}

/* Threshold used for digitization */
int
Vect_set_thresh (struct Map_info *Map, double thresh )
{
    G_debug ( 1, "Vect_set_thresh(): thresh = %f", thresh );
    Map->head.digit_thresh = thresh;
    return (0);
}

double
Vect_get_thresh (struct Map_info *Map)
{
    G_debug ( 1, "Vect_get_thresh(): thresh = %f", Map->head.digit_thresh );
    return (Map->head.digit_thresh);
}


