/* Cell-file line thinning */

/* Mike Baba */
/* DBA Systems */
/* Fairfax, Va */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* January - February 1988 */

#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "local_proto.h"

char *error_prefix;

int 
main (int argc, char *argv[])
{
	char *input, *output;
	struct GModule *module;
	struct Option *opt1, *opt2;

    module = G_define_module();
    module->description =
		"Thins non-zero cells that denote linear "
		"features in a raster map layer.";
					        
	opt1 = G_define_option() ;
	opt1->key        = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of existing raster map" ;

	opt2 = G_define_option() ;
	opt2->key        = "output" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->gisprompt  = "new,cell,raster" ;
	opt2->description= "Name of output raster file" ;

	G_gisinit(argv[0]);

	if (G_parser(argc, argv))
		exit(-1);

	input = opt1->answer;
	output = opt2->answer;

	open_file(input);
	thin_lines();
	close_file(output);
	exit(0);
}

