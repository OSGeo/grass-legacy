/*  @(#)b_a_dig.c	2.1  6/26/87  */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	FILE *ascii, *att;
	struct Option *old, *new;
	struct Flag *verf;
	char *mapset;
	char errmsg[200];
	struct Map_info Map;
	int    ver=5, pnt=0; 

	G_gisinit(argv[0]);

	old = G_define_standard_option(G_OPT_V_INPUT);

	new = G_define_option();
	new->key		= "output";
	new->type		=  TYPE_STRING;
	new->required		=  NO;
	new->multiple		=  NO;
	new->gisprompt  	= "new,dig_ascii,ascii vector" ;
	new->description	= "name of resulting ascii file";

        verf = G_define_flag ();
        verf->key               = 'o';
        verf->description       = "create old (version 4) ascii file"; 

	if (G_parser (argc, argv)) exit(-1);

	if (!*(old->answer))
	{
    	    fprintf (stderr, "%s: Command line error: missing input name.\n\n", argv[0]);
	    G_usage();
    	    exit (-1);
	}

	if ((mapset = G_find_vector2 (old->answer, "")) == NULL)
	{
		sprintf (errmsg, "Could not find vector file <%s>\n", old->answer);
		G_fatal_error (errmsg);
	}
	
	Vect_set_open_level (1);	/* only need level I */
	Vect_open_old (&Map, old->answer, mapset); 

	if ( verf->answer )
 	    ver = 4;	
		
	if ( new->answer != NULL ) {
	    if ( (ascii = G_fopen_new("dig_ascii", new->answer) ) == NULL )
	    {
		    sprintf(errmsg, "Not able to open ascii file <%s>\n", new->answer) ;
		    G_fatal_error (errmsg);
	    }
	    pnt = 0;
	    
	    write_head(ascii, &Map) ;
	    fprintf (ascii, "VERTI:\n");

	} else { /* write points to stdout */
	    ascii = stdout;
	    pnt = 1;
        }

	/* Open dig_att */
	att = NULL;
	if ( ver == 4 && !pnt ) {
	    if ( G_find_file ("dig_att", new->answer, G_mapset() ) != NULL )
		G_fatal_error ( "dig_att file already exist" );
		
	    if ( (att = G_fopen_new("dig_att", new->answer) ) == NULL )
	        G_fatal_error ( "Not able to open dig_att file <%s>\n", new->answer) ;
	}

	bin_to_asc (ascii, att, &Map, ver, pnt) ;

	if ( !pnt )  fclose(ascii) ;
	if ( att != NULL )  fclose(att) ;

	Vect_close (&Map);

	exit(0) ;
}

