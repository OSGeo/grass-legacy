/*
**  v.patch  input=file1,file2,.... output=composite
**
**   patch 2 or more vector files together creating composite
**
**
**  no checking is done for overlapping lines.
**  header information will have to be editted afterwards.
*/

/*
**  Written by Dave Gerdes  8/1988, US Army Construction Engineering Research Lab
**  Upgrade to 5.7 Radim Blazek
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "Vect.h"

int patch(struct Map_info *, struct Map_info *);

int 
main (int argc, char *argv[])
{
	int i, ret;
	char *in_name, *out_name, *mapset;
	char errmsg[200];
	struct GModule *module;
	struct Option *old, *new;
	struct Flag *append;
	struct Map_info InMap, OutMap;
	int  n_files;

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description = "Creates a new binary vector map layer "
		              "by combining other binary vector map layers.";

	old = G_define_option();
	old->key		= "input";
	old->type		= TYPE_STRING;
	old->required		= YES;
	old->multiple		= YES;
	old->gisprompt		= "old,vector,vector";
	old->description	= "vector map(s)--source for composite";

	new = G_define_option();
	new->key		= "output";
	new->type		= TYPE_STRING;
	new->required		= YES;
	new->multiple		= NO;
	new->gisprompt		= "any,vector,vector";
	new->description	= "new vector composite";

        append = G_define_flag();
	append->key = 'a';
        append->description = "Append files to existing file";
	
	if (G_parser (argc, argv)) exit(-1);

	out_name = new->answer;

	/* TODO: check first if any of input is 3D and open output as 3D */
	i=0;
        while (old->answers[i]) {
            in_name = old->answers[i++];
	    Vect_check_input_output_name ( in_name, new->answer, GV_FATAL_EXIT );
        }

	if ( append->answer ) {
	    ret = Vect_open_update ( &OutMap, out_name,  G_mapset() );
	} else {
	    ret = Vect_open_new (&OutMap, out_name, 0);
	}
	Vect_copy_head_data (&InMap, &OutMap);
	Vect_hist_copy (&InMap, &OutMap);
	Vect_hist_command ( &OutMap );
	
	if ( ret < 0 ) 
	   G_fatal_error ( "Not able to open vector file <%s>\n", out_name);

	i = 0;
	while (old->answers[i]) {
		in_name = old->answers[i++];
		fprintf (stdout, "    Patching file %s\n", in_name);
		if ((mapset = G_find_vector2 (in_name, "")) == NULL)
		{
			sprintf (errmsg, "Could not find Vector file <%s>\n", in_name);
			G_fatal_error (errmsg);
		}

		/*new method-with Vect lib*/
		if ((Vect_open_old (&InMap, in_name, mapset)) < 1)
		{
			fprintf (stderr, "Cannot open Vector file <%s>\n", in_name);
			continue;
		}
		if (i == 1) /*first time around, copy first in head to out head*/
		   Vect_copy_head_data ( &InMap, &OutMap);

		ret = patch (&InMap, &OutMap);
		if (ret < 0)
			G_warning ( "Error reading file '%s'." 
			      "Some data may not be correct\n", in_name);

		Vect_close (&InMap);
	}
	n_files = i;
	
	Vect_set_map_name ( &OutMap, "Output from v.patch");
	Vect_set_person ( &OutMap, G_whoami ());

	Vect_build (&OutMap, stdout);
	Vect_close (&OutMap);

	fprintf (stdout, "Patch complete. %d files patched.\n", n_files);
	fprintf (stdout, "Intersections at borders will have to be snapped.\n");
	fprintf (stdout, "Lines common between files will have to be edited.\n");
	fprintf (stdout, "The header information also may have to be edited.\n");

	exit (0);
}

int patch ( struct Map_info *InMap, struct Map_info *OutMap)
{
	int type;
	struct line_pnts *Points;
	struct line_cats *Cats;

	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();

        /* TODO:	
	OutMap->head.orig_scale = GREATER (OutMap->head.orig_scale, InMap->head.orig_scale);
	OutMap->head.digit_thresh = 0;
	OutMap->head.map_thresh = GREATER (OutMap->head.map_thresh, InMap->head.map_thresh);
	*/

	while ( (type = Vect_read_next_line (InMap, Points, Cats)) > 0) {
		Vect_write_line (OutMap, type, Points, Cats);
	}

	Vect_destroy_line_struct (Points);
	Vect_destroy_cats_struct (Cats);

	if (type != -2) return (-1);

	return (0);
}

