/*  @(#)b_a_dig.c	2.1  6/26/87  */

#include "stdio.h"
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	FILE *ascii;
	struct Option *old, *new;
	struct Flag *verf, *pntf;
	char *mapset;
	char errmsg[200];
	struct Map_info Map;
	int    ver=5, pnt=0; 

	G_gisinit(argv[0]);

	old = G_define_option();
	old->key		= "input";
	old->type		=  TYPE_STRING;
	old->required		=  YES;
	old->multiple		=  NO;
	old->gisprompt  	= "old,vector,vector" ;
	old->description	= "binary vector file to be converted to ascii";

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

	/*
        pntf = G_define_flag ();
        pntf->key               = 'p';
        pntf->description       = "writes points and centroids only, one on one row"; 
	*/
	
	if (G_parser (argc, argv))
		exit(-1);

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
	if ( (Vect_open_old (&Map, old->answer, mapset) ) < 1)
	{
		sprintf(errmsg, "Could not open vector file <%s>\n", old->answer);
		G_fatal_error (errmsg);
	}

	if ( verf->answer )
 	    ver = 4;	
		
	if ( new->answer != NULL ) {
	    if ( (ascii = G_fopen_new("dig_ascii", new->answer) ) == NULL )
	    {
		    sprintf(errmsg, "Not able to open ascii file <%s>\n", new->answer) ;
		    G_fatal_error (errmsg);
	    }
	    pnt = 0;
	    
	    dig_write_head_ascii(ascii, &(Map.head)) ;
	    fprintf (ascii, "VERTI:\n");

	} else { /* write points to stdout */
	    ascii = stdout;
	    pnt = 1;
        }

	bin_to_asc (ascii, &Map, ver, pnt) ;

	if ( !pnt )
	    fclose(ascii) ;

	Vect_close (&Map);

	exit(0) ;
}

