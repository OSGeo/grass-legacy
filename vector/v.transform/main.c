/****************************************************************************
*
* MODULE:       v.transform
* AUTHOR(S):    See below also.
*               Eric G. Miller <egm2@jps.net>
*               Upgrade to 5.7 Radim Blazek
* PURPOSE:      To transform a vector layer's coordinates via a set of tie
*               points.
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
/*
*History:
*- This takes an ascii digit file in one coordinate system and converts
*  the map to another coordinate system.
*  Uses the transform library:  $(TRANSLIB)
*
*  Written during the ice age of Illinois, 02/16/90, by the GRASS team, -mh.
*
*- Modified by Dave Gerdes  1/90  for dig_head stuff
*- Modified by Radim Blazek to work on binary files 2002
*/
#define MAIN

#include <stdlib.h>
#include <string.h>
#include "trans.h"
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "local_proto.h"
#include "glocale.h"

int main (int argc, char *argv[])
{
    struct file_info  Current, Trans, Coord ;
    struct GModule *module;
    struct Option *old, *new, *pointsfile, *xshift, *yshift, *zshift, *xscale, *yscale, *zscale, *zrot;
    struct Flag *quiet_flag, *tozero_flag, *shift_flag;
    char   *mapset, mon[4], date[40], buf[1000];
    struct Map_info Old, New;
    int    day, yr; 
    BOUND_BOX box;
    double ztozero;

    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description = "Transforms an vector map layer from one "
			  "coordinate system into another coordinate system.";

    quiet_flag = G_define_flag();
    quiet_flag->key		= 'q';
    quiet_flag->description = "suppress display of residuals or other information"; 

    tozero_flag = G_define_flag();
    tozero_flag->key		= 't';
    tozero_flag->description = "shift all z values to bottom=0"; 

    shift_flag = G_define_flag();
    shift_flag->key		= 's';
    shift_flag->description = "Instead of points use transformation options "
		"(xshift, yshift, zshif, xscale, yscale, zscale, zrot)"; 

    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt		= "old,vector,vector";
    old->description		= "vector map to be transformed";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->gisprompt		= "new,vector,vector";
    new->description		= "resultant vector map";

    pointsfile = G_define_option();
    pointsfile->key		= "pointsfile";
    pointsfile->type		= TYPE_STRING;
    pointsfile->required	= NO;
    pointsfile->multiple	= NO;
    pointsfile->description	= "file holding transform coordinates";
    pointsfile->gisprompt       = "old_file,,points";

    xshift = G_define_option();
    xshift->key		= "xshift";
    xshift->type	= TYPE_DOUBLE;
    xshift->required	= NO;
    xshift->multiple	= NO;
    xshift->description	= "shifting value for x coordinates";
    xshift->answer     = "0.0";

    yshift = G_define_option();
    yshift->key		= "yshift";
    yshift->type	= TYPE_DOUBLE;
    yshift->required	= NO;
    yshift->multiple	= NO;
    yshift->description	= "shifting value for y coordinates";
    yshift->answer     = "0.0";

    zshift = G_define_option();
    zshift->key		= "zshift";
    zshift->type	= TYPE_DOUBLE;
    zshift->required	= NO;
    zshift->multiple	= NO;
    zshift->description	= "shifting value for z coordinates";
    zshift->answer     = "0.0";

    xscale = G_define_option();
    xscale->key		= "xscale";
    xscale->type	= TYPE_DOUBLE;
    xscale->required	= NO;
    xscale->multiple	= NO;
    xscale->description	= "scaling factor for x coordinates";
    xscale->answer     = "1.0";

    yscale = G_define_option();
    yscale->key		= "yscale";
    yscale->type	= TYPE_DOUBLE;
    yscale->required	= NO;
    yscale->multiple	= NO;
    yscale->description	= "scaling factor for y coordinates";
    yscale->answer     = "1.0";

    zscale = G_define_option();
    zscale->key		= "zscale";
    zscale->type	= TYPE_DOUBLE;
    zscale->required	= NO;
    zscale->multiple	= NO;
    zscale->description	= "scaling factor for z coordinates";
    zscale->answer     = "1.0";

    zrot = G_define_option();
    zrot->key		= "zrot";
    zrot->type	= TYPE_DOUBLE;
    zrot->required	= NO;
    zrot->multiple	= NO;
    zrot->description	= "rotation around z axis in degrees counterclockwise";
    zrot->answer     = "0.0";
    
    if (G_parser (argc, argv))
	exit (-1);
    
    strcpy (Current.name, old->answer);
    strcpy (Trans.name, new->answer);

    Vect_check_input_output_name ( old->answer, new->answer, GV_FATAL_EXIT );
   
    if ( !shift_flag->answer ) { 
	if (pointsfile->answer != NULL)
	    strcpy (Coord.name, pointsfile->answer);
	else
	    Coord.name[0] = '\0';
	
	/* open coord file */
	if ( Coord.name[0] != '\0' ){
	    if ( (Coord.fp = fopen(Coord.name, "r"))  ==  NULL) 
		G_fatal_error ( _("Could not open file with coordinates : %s\n"), Coord.name) ;
	}
    }
    
    /* open vectors */
    if ( (mapset = G_find_vector2 ( old->answer, "")) == NULL)
	G_fatal_error ( _("Could not find input map <%s>\n"), old->answer);
    
    Vect_open_old(&Old, old->answer, mapset);

    Vect_open_new (&New, new->answer, Vect_is_3d(&Old) || zshift->answer );

    /* copy and set header */
    Vect_copy_head_data(&Old, &New);

    Vect_hist_copy (&Old, &New);
    Vect_hist_command ( &New );

    sprintf(date,"%s",G_date());
    sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
    sprintf(date,"%s %d %d",mon,day,yr);
    Vect_set_date ( &New, date );
    
    Vect_set_person ( &New, G_whoami() );

    sprintf (buf, "transformed from %s", old->answer);
    Vect_set_map_name ( &New, buf);
    
    Vect_set_scale ( &New, 1 );
    Vect_set_zone ( &New, 0 );
    Vect_set_thresh ( &New, 0.0 );
    
    if ( !shift_flag->answer ) { 
	create_transform_conversion( &Coord, quiet_flag->answer);

	if (Coord.name[0] != '\0')
		fclose( Coord.fp) ;
    }
    
    if (!quiet_flag->answer)
       G_message ( _("\nNow transforming the vectors ...\n"));
    
    Vect_get_map_box (&Old, &box );
    if (tozero_flag->answer)
       ztozero = 0 - box.B;
    else
       ztozero = 0;

    transform_digit_file( &Old, &New, shift_flag->answer,
	    atof(xshift->answer), atof(yshift->answer), atof(zshift->answer), ztozero,
	    atof(zrot->answer), atof(xscale->answer), atof(yscale->answer), atof(zscale->answer)) ;

    Vect_copy_tables ( &Old, &New, 0 );
    Vect_close (&Old);

    if (!quiet_flag->answer) Vect_build (&New, stdout); else Vect_build (&New, NULL);

    Vect_get_map_box (&New, &box );
    G_message ( _("New vector map <%s> boundary coordinates:\n"), new->answer);
    G_message ( _(" N: %-10.3f    S: %-10.3f\n"), box.N, box.S);
    G_message ( _(" E: %-10.3f    W: %-10.3f\n"), box.E, box.W);
    G_message ( _(" B: %6.3f    T: %6.3f\n"), box.B, box.T);

    Vect_close (&New);

    if (!quiet_flag->answer)
	    G_message ( _("'%s' has finished the transformation of the vectors.\n"), argv[0]) ;

    exit(0) ;
}
