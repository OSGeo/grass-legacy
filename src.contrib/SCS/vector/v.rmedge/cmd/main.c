/* Vrmedge    1.0   01/10/90
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of removing the outer edge 
*             from an existing vector map layer. Selects all vector
*             boundaries for which one side is 0 or -1.
*    Input arguements:
*             v.rmedge input=vector (digit) file to read
*                      output=vector (digit) file to create
*/
#include <stdio.h>
#include  "gis.h"
#include  "local_proto.h"

int 
main (int argc, char *argv[])
{
	int i, ier, cat_index=0;
	int dissolve=0, cnt;
        char *pntr1, *pntr2;
        char *input, *output, catfile[128];
        FILE *in, *outa, *outb, *catf;
	struct Option *inopt, *outopt;
	struct GModule *module;

        G_gisinit(argv[0]);
        
        /* Set description */
        module              = G_define_module();
        module->description = ""\
        "Selects edge vectors from an existing vector map, removes them, and creates a new vector map.";
        
	inopt = G_define_option();
        inopt->key          = "input";
        inopt->description  = "Name of vector input file";
        inopt->type         = TYPE_STRING;
        inopt->required     = YES;
        inopt->multiple     = NO;
        inopt->gisprompt    = "old,dig,vector";

	outopt = G_define_option();
        outopt->key          = "output";
        outopt->description  = "Name of vector output file";
        outopt->type         = TYPE_STRING;
        outopt->required     = YES;
        outopt->multiple     = NO;
        outopt->gisprompt    = "new,dig,vector";

	if (G_parser(argc, argv)) exit(-1);
	
	input = inopt->answer;
	output = outopt->answer;

    if ( 0 > rmedge(input, output))
       {
       fprintf(stderr," Error in remove edge processing\n");
       exit(1);
       }

    return 0;
}
