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
	struct Option *old, *new, *format_opt;
	struct Flag *verf;
	int  format;
	struct Map_info Map;
	int    ver=5, pnt=0; 
	struct GModule *module;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description = "Convert a GRASS binary vector map to a GRASS ASCII vector map";

	old = G_define_standard_option(G_OPT_V_INPUT);

	new = G_define_option();
	new->key		= "output";
	new->type		=  TYPE_STRING;
	new->required		=  NO;
	new->multiple		=  NO;
	new->gisprompt  	= "file,file,file" ;
	new->description	= "path to resulting ascii file or ASCII vector name if '-o' is defined";

	format_opt = G_define_option();
	format_opt->key		= "format";
	format_opt->type	=  TYPE_STRING;
	format_opt->required	=  NO;
	format_opt->multiple	=  NO;
	format_opt->options	= "point,standard";
	format_opt->answer	= "point";
	format_opt->description	= "output format";

        verf = G_define_flag ();
        verf->key               = 'o';
        verf->description       = "create old (version 4) ascii file"; 

	if (G_parser (argc, argv)) exit(-1);

	if ( format_opt->answer[0] == 'p' )
	    format = FORMAT_POINT;
	else 
	    format = FORMAT_ALL;
	
	if ( verf->answer ) ver = 4;	

	if ( ver == 4 && format == FORMAT_POINT ) {
	   G_fatal_error ("format 'point' is not supported for old version" );
	}

	if ( ver == 4 && new->answer == NULL ) {
	   G_fatal_error ("'output' must be given for old version" );
	}

	Vect_set_open_level (1);	/* only need level I */
	Vect_open_old (&Map, old->answer, ""); 

		
	if ( new->answer ) {
	    if ( ver == 4 ) {
	        ascii = G_fopen_new("dig_ascii", new->answer);
	    } else {
	        ascii = fopen ( new->answer, "w" );
	    }
		
	    if ( ascii == NULL ) {
		G_fatal_error("Cannot open file [%s]", new->answer );
	    }
	} else {
	    ascii = stdout;
	}
	
	if ( format == FORMAT_ALL ) {
	    write_head(ascii, &Map) ;
	    fprintf (ascii, "VERTI:\n");
        }

	/* Open dig_att */
	att = NULL;
	if ( ver == 4 && !pnt ) {
	    if ( G_find_file ("dig_att", new->answer, G_mapset() ) != NULL )
		G_fatal_error ( "dig_att file already exist" );
		
	    if ( (att = G_fopen_new("dig_att", new->answer) ) == NULL )
	        G_fatal_error ( "Not able to open dig_att file <%s>\n", new->answer) ;
	}

	bin_to_asc (ascii, att, &Map, ver, format ) ;

	if ( ascii != NULL )  fclose(ascii) ;
	if ( att != NULL )  fclose(att) ;

	Vect_close (&Map);

	exit(0) ;
}

