/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.7 Radim Blazek and David D. Gray.
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
#include "glocale.h"

static int lookup(char *, char *, char *, int);

/*!
 \fn int Vect_print_header (struct Map_info *Map)
 \brief print vector map header
 \return 0 on success
 \param Map_info structure
*/
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

/* Vect__read_head () reads head information from text file (GRASS_VECT_HEAD_ELEMENT). 
 * returns: GRASS_OK - success
 *          GRASS_ERR - error
 */
int
Vect__read_head (struct Map_info *Map)
{
    FILE *head_fp;
    char buff[2001];
    char *ptr;

    G_debug (1, "Vect__read_head(): vector = %s@%s", Map->name, Map->mapset);
    sprintf (buff, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    head_fp = G_fopen_old (buff, GRASS_VECT_HEAD_ELEMENT, Map->mapset); 
    if ( head_fp == NULL)
      {
        G_warning ("Cannot Open Vector %s Head File\n", Map->name);
        return (GRASS_ERR);
      }
   
    while ( G_getl2 (buff, 2000, head_fp) ) {

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

/*!
 \fn int Vect_is_3d (struct Map_info *Map)
 \brief check if vector map is 3D (with z)
 \return 1 on success, 0 of not 3D
 \param Map_info structure
*/
int
Vect_is_3d (struct Map_info *Map )
{
    return ( Map->head.with_z );
}

/*!
 \fn int Vect_set_organization (struct Map_info *Map, char *str )
 \brief set organization string in map header
 \return 0 on success
 \param Map_info structure, organization string
*/
int
Vect_set_organization (struct Map_info *Map, char *str )
{
    G_free ( Map->head.organization );
    Map->head.organization = G_store ( str );
    return (0);
}

/*!
 \fn char *Vect_get_organization (struct Map_info *Map)
 \brief get organization string from map header
 \return organization string
 \param Map_info structure
*/
char *
Vect_get_organization (struct Map_info *Map)
{
    return (Map->head.organization);
}

/*!
 \fn int Vect_set_date (struct Map_info *Map, char *str )
 \brief set date of digitization string in map header
 \return 0 on success
 \param Map_info structure,  date of digitization string
*/
/* SUGGESTION: this should be coupled to DateTime functions to support time series*/
int
Vect_set_date (struct Map_info *Map, char *str )
{
    G_free ( Map->head.date );
    Map->head.date = G_store ( str );
    return (0);
}

/*!
 \fn char *Vect_get_date (struct Map_info *Map)
 \brief get date of digitization string from map header
 \return date of digitization string
 \param Map_info structure
*/
/* SUGGESTION: this should be coupled to DateTime functions to support time series*/
char *
Vect_get_date (struct Map_info *Map)
{
    return (Map->head.date);
}

/*!
 \fn int Vect_set_person (struct Map_info *Map, char *str )
 \brief set  user name string who digitized the map in map header
 \return 0 on success
 \param Map_info structure,   user name string
*/
int
Vect_set_person (struct Map_info *Map, char *str )
{
    G_free ( Map->head.your_name );
    Map->head.your_name = G_store ( str );
    return (0);
}

/*!
 \fn char *Vect_get_person (struct Map_info *Map)
 \brief get user name string who digitized the map from map header
 \return user name string
 \param Map_info structure
*/
char *
Vect_get_person (struct Map_info *Map)
{
    return (Map->head.your_name);
}

/*!
 \fn int Vect_set_map_name (struct Map_info *Map, char *str )
 \brief set  map name string in map header
 \return 0 on success
 \param Map_info structure, map name string
*/
int
Vect_set_map_name (struct Map_info *Map, char *str )
{
    G_free ( Map->head.map_name );
    Map->head.map_name = G_store ( str );
    return (0);
}

/*!
 \fn char *Vect_get_map_name (struct Map_info *Map)
 \brief get map name string in map header
 \return map name string
 \param Map_info structure
*/
char *
Vect_get_map_name (struct Map_info *Map)
{
    return (Map->head.map_name);
}

/*!
 \fn int Vect_set_map_date (struct Map_info *Map, char *str )
 \brief set date string when the source map was originally produced in map header
 \return 0 on success
 \param Map_info structure,  date when the source map was originally produced string
*/
int
Vect_set_map_date (struct Map_info *Map, char *str )
{
    G_free ( Map->head.source_date );
    Map->head.source_date = G_store ( str );
    return (0);
}

/*!
 \fn char *Vect_get_map_date (struct Map_info *Map)
 \brief get date string when the source map was originally produced in map header
 \return date when the source map was originally produced string
 \param Map_info structure
*/
char *
Vect_get_map_date (struct Map_info *Map)
{
    return (Map->head.source_date);
}

/*!
 \fn int Vect_set_scale (struct Map_info *Map,  int scale)
 \brief set  map scale in map header
 \return 0 on success
 \param Map_info structure, map scale
*/
int
Vect_set_scale (struct Map_info *Map, int scale )
{
    Map->head.orig_scale = scale;
    return (0);
}

/*!
 \fn int Vect_get_scale (struct Map_info *Map)
 \brief get  map scale from map header
 \return map scale
 \param Map_info structure
*/
int
Vect_get_scale (struct Map_info *Map)
{
    return ((int) Map->head.orig_scale);
}

/*!
 \fn int Vect_set_comment (struct Map_info *Map, char *str )
 \brief set comment or other info string in map header
 \return 0 on success
 \param Map_info structure, comment or other info string
*/
int
Vect_set_comment (struct Map_info *Map, char *str )
{
    G_free ( Map->head.line_3 );
    Map->head.line_3 = G_store ( str );
    return (0);
}

/*!
 \fn  char *Vect_get_comment (struct Map_info *Map )
 \brief get comment or other info string from map header
 \return comment or other info string
 \param Map_info structure
*/
char *
Vect_get_comment (struct Map_info *Map)
{
    return (Map->head.line_3);
}

/*!
 \fn int Vect_set_zone (struct Map_info *Map, int zone )
 \brief set projection zone in map header
 \return 0 on success
 \param Map_info structure, projection zone
*/
int
Vect_set_zone (struct Map_info *Map, int zone )
{
    Map->head.plani_zone = zone;
    return (0);
}


/*!
 \brief get projection zone from map header
 \return projection zone
 \param Map_info structure
*/
int
Vect_get_zone (struct Map_info *Map)
{
    return (Map->head.plani_zone);
}

/*!
 *  \brief get projection from map header
 *  \param Map_info structure
 *  \return Returns the projection type of map
 *      PROJECTION_XY  0 - x,y (Raw imagery),
 *      PROJECTION_UTM 1 - UTM   Universal Transverse Mercator,
 *      PROJECTION_SP  2 - State Plane (in feet),
 *      PROJECTION_LL  3 - Latitude-Longitude
 *
*/
int
Vect_get_proj (struct Map_info *Map)
{
    return (Map->proj);
}


/*!
 * \brief query cartographic projection name of vector map
 *
 * Returns a pointer to a string which is a printable name for
 * projection code <b>proj</b> (as returned by <i>Vect_get_proj()</i>). Returns
 * NULL if <b>proj</b> is not a valid projection.
 *
 *  \param proj
 *  \return char * 
 */

char *Vect_get_proj_name (struct Map_info *Map)
{
    int n;
    static char name[256];
    char *G__projection_name();

    switch(n=Vect_get_proj(Map))
    {
    case PROJECTION_XY:
    case PROJECTION_UTM:
    case PROJECTION_LL:
    case PROJECTION_SP:
	return G__projection_name(n);
    }
    if(!lookup (PROJECTION_FILE, "name", name, sizeof(name)))
	strcpy (name, _("Unknown projection"));
    return name;
}


/*!
 \fn int Vect_set_thresh (struct Map_info *Map, double thresh )
 \brief set threshold used for digitization in map header
 \return 0 on success
 \param Map_info structure, threshold used for digitization
*/
int
Vect_set_thresh (struct Map_info *Map, double thresh )
{
    G_debug ( 1, "Vect_set_thresh(): thresh = %f", thresh );
    Map->head.digit_thresh = thresh;
    return (0);
}

/*!
 \fn double Vect_get_zone (struct Map_info *Map )
 \brief get threshold used for digitization from map header
 \return threshold used for digitization
 \param Map_info structure
*/
double
Vect_get_thresh (struct Map_info *Map)
{
    G_debug ( 1, "Vect_get_thresh(): thresh = %f", Map->head.digit_thresh );
    return (Map->head.digit_thresh);
}


/* from lib/gis/proj3.c */
static int lookup(char *file, char *key, char *value, int len)
{
    char path[1024];

    G__file_name (path, "", file, "PERMANENT");
    return G_lookup_key_value_from_file(path, key, value, len) == 1;
}
