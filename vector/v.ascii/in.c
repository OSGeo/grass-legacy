#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

#define	A_DIR	"dig_ascii"


int 
main (int argc, char *argv[])
{
	FILE *ascii;
	struct dig_head d_head;
	struct Option *old, *new;
	struct Flag *zcoorf, *pntf;
	char   *mapset;
	char   errmsg[200];
	int    ver=5, zcoor=WITHOUT_Z, points_format; 

	struct Map_info Map;

	G_gisinit(argv[0]);

/************************** Command Parser ************************************/
	old = G_define_option();
	old->key			= "input";
	old->type			=  TYPE_STRING;
	old->required		=  NO;
	old->multiple		=  NO;
	old->gisprompt  	= "old,dig_ascii,ascii vector";
	old->description	= "ascii file to be converted to binary vector file";

	new = G_define_standard_option(G_OPT_V_OUTPUT);

	zcoorf = G_define_flag ();
        zcoorf->key           	= 'z';
	zcoorf->description   	= "create 3D file";  
	
	/*
        pntf = G_define_flag ();
        pntf->key               = 'p';
        pntf->description       = "reads points and centroids only, one on one row"; 
        */
	
	if (G_parser (argc, argv))
		exit(-1);

	if ( !*(new->answer) )
	{
	    fprintf (stderr, "%s: Command line error: missing output name.\n\n", argv[0]);
		    G_usage();
	    exit (-1);
	}
/*****************************************************************************/
        
	if ( old->answer != NULL ) {
	    if ((mapset = G_find_file2 (A_DIR, old->answer, "")) == NULL)
	    {
		    sprintf (errmsg, "Could not find ascii file <%s>\n", old->answer);
		    G_fatal_error (errmsg);
	    }
	    if ( (ascii = G_fopen_old (A_DIR, old->answer, mapset) ) == NULL )
	    {
		    sprintf(errmsg, "Could not open ascii file <%s>\n", old->answer);
		    G_fatal_error (errmsg);
	    }
	    points_format = 0;
        } else { /* write points to stdout */
            ascii = stdin;
	    points_format = 1;
        }

	/* check dimension */
	if (zcoorf->answer) {
	    zcoor = 1;	    
	}

	Vect_open_new (&Map, new->answer, zcoor);

	if ( !points_format ) {
    	    read_head(ascii, &Map);
	}

	asc_to_bin(ascii, &Map, points_format) ;
        
	if ( !points_format )
	    fclose(ascii) ;

	Vect_build ( &Map, stdout );
	Vect_close (&Map);

	exit(0) ;
}

