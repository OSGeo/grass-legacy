/*
*  This takes an ascii digit file in one coordinate system and converts
*  the map to another coordinate system.
*  Uses the transform library:  ../src/libes/vect32/libes/libtrans.a .
*
*  Written during the ice age of Illinois, 02/16/90, by the GRASS team, -mh.
*
*  Modified by Dave Gerdes  1/90  for dig_head stuff
*  Modified by Radim Blazek to work on binary files
*
*/

#define MAIN

#include <string.h>
#include "trans.h"
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{

	struct dig_head dhead;
	struct file_info  Current, Trans, Coord ;
	struct GModule *module;
	struct Option *old, *new, *pointsfile;
	struct Flag *quiet_flag;
	char   *mapset, mon[4], date[40], buf[1000];
	struct Map_info Old, New;
	int    day, yr; 
	

	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Transforms an vector map layer from one "
		"coordinate system into another coordinate system.";

	quiet_flag = G_define_flag();
	quiet_flag->key		= 'y';
	quiet_flag->description = "suppress display of residuals or other information"; 

        old = G_define_option();
        old->key			= "input";
        old->type			= TYPE_STRING;
        old->required			= YES;
        old->multiple			= NO;
        old->gisprompt			= "old,dig,vector";
        old->description		= "vector map to be transformed";
        
        new = G_define_option();
        new->key			= "output";
        new->type			= TYPE_STRING;
        new->required			= YES;
        new->multiple			= NO;
        new->gisprompt			= "new,dig,vector";
        new->description		= "resultant vector map";

        pointsfile = G_define_option();
        pointsfile->key			= "pointsfile";
        pointsfile->type			= TYPE_STRING;
        pointsfile->required		= NO;
        pointsfile->multiple		= NO;
        pointsfile->description		= "file holding transform coordinates";
        
	if (G_parser (argc, argv))
	    exit (-1);
	
	strcpy (Current.name, old->answer);
	strcpy (Trans.name, new->answer);
	
	if (pointsfile->answer != NULL)
	    strcpy (Coord.name, pointsfile->answer);
	else
	    Coord.name[0] = '\0';
	
	/* open coord file */
        if ( Coord.name[0] != '\0' ){
            if ( (Coord.fp = fopen(Coord.name, "r"))  ==  NULL) 
	        G_fatal_error ("Could not open file with coordinates : %s\n", Coord.name) ;
	}
	
	/* open vectors */
	if ( (mapset = G_find_file2 ("dig", old->answer, "")) == NULL)
	    G_fatal_error ("Could not find input vector %s\n", old->answer);
	
        if( Vect_open_old(&Old, old->answer, mapset) < 1)
            G_fatal_error("Could not open input vector %s\n", old->answer);

	if (0 > Vect_open_new (&New, new->answer)) {
	    Vect_close (&Old);
            G_fatal_error("Could not open output vector %s\n", new->answer);
	}


        /* copy and set header */
	Vect_copy_head_data(&Old.head, &New.head);

        sprintf(date,"%s",G_date());
        sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
	sprintf(date,"%s %d %d",mon,day,yr);
        strcpy( New.head.date, date);
	
        strcpy( New.head.your_name, G_whoami());

        sprintf (buf, "transformed from %s", old->answer);
        /* truncate if string > DIG_MAP_NAME_LEN-1 */
        buf[DIG_MAP_NAME_LEN-1] = '\0';
	strcpy( New.head.map_name, buf);
	
        New.head.orig_scale  =  0.0 ;
	New.head.plani_zone  =  0 ;
	New.head.map_thresh  =  0.0 ;
	New.head.N  =  0.0 ;
	New.head.S  =  0.0 ;
	New.head.E  =  0.0 ;
	New.head.W  =  0.0 ;
	
	create_transform_conversion( &Coord, quiet_flag->answer);

	if (Coord.name[0] != '\0')
		fclose( Coord.fp) ;
	
	if (!quiet_flag->answer)
	    fprintf (stdout,"\n\n Now transforming the vectors ...") ;
	
	transform_digit_file( &Old, &New) ;

	Vect_close (&Old);
	Vect_close (&New);

	if (!quiet_flag->answer)
		fprintf (stdout,"\n '%s' has finished the transformation of the vectors.\n", argv[0]) ;

	/* set mapset */
	Current.mapset = mapset;
	Trans.mapset = G_mapset();

	if ( open_att_files ( &Current, &Trans) )
	{
		if (!quiet_flag->answer)
			fprintf (stdout,"\nThere was no dig_att file to convert with this vector file. \n") ;
		exit(0) ;
	}

	transform_att_file( Current.fp, Trans.fp) ;

	if (!quiet_flag->answer)
	{
		fprintf (stdout," '%s' has finished the transformation of the vector's attribute file.\n", argv[0]) ;
		fprintf (stdout,"Transformation is complete.\n") ;
	}
	fclose( Current.fp) ;
	fclose( Trans.fp) ;

        trans_dig_cats (Current.name, Current.mapset, Trans.name);

	exit(0) ;

}

