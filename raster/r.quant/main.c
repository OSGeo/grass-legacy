#define MAIN
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "global.h"

int 
main (int argc, char *argv[])
{
    char buf[1024];
	struct GModule *module;
    struct Option *input, *basemap, *fprange, *range;
    struct Flag *trunc, *rnd;
    int truncate;
    int round;
    int i;
    CELL new_min, new_max;
    DCELL new_dmin, new_dmax;
    char *basename, *basemapset;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"This routine produces the quantization file for a floating-point map.";

    basemap = G_define_option();
    basemap->key = "basemap";
    basemap->required = NO;
    basemap->type = TYPE_STRING;
    basemap->answer = "NONE";
    basemap->gisprompt  = "old,cell,raster" ;
    basemap->description = "Base map to take quant rules from";

    input = G_define_option();
    input->key = "input";
    input->required = YES;
    input->multiple = YES ;
    input->type = TYPE_STRING;
    input->gisprompt  = "old,cell,raster" ;
    input->description =  "Raster map[s] to be quantized";

    fprange = G_define_option();
    fprange->key = "fprange";
    fprange->key_desc = "dmin,dmax";
    fprange->description = "Floating point range: dmin,dmax";
    fprange->type = TYPE_STRING;
    fprange->answer = "";
    fprange->required = YES;

    range = G_define_option();
    range->key = "range";
    range->key_desc = "min,max";
    range->description = "Integer range: min,max";
    range->type = TYPE_STRING;
    range->answer = "1,255";
    range->required = YES;

    trunc = G_define_flag();
    trunc->key = 't';
    trunc->description	= "truncate floating point data";

    rnd = G_define_flag();
    rnd->key = 'r';
    rnd->description	= "round floating point data";

    if (G_parser(argc, argv))
	exit(1);
    truncate = trunc->answer;
    round = rnd->answer;
    G_quant_init(&quant_struct);

    /* read and check inputs */
    for (noi = 0; input->answers[noi]; noi++)
    {
       name[noi] = G_store(input->answers[noi]);
       mapset[noi] = G_find_cell2 (name[noi], "");
       if (mapset[noi] == NULL)
       {
 	 sprintf (buf, "%s - not found", name[noi]);
	 G_fatal_error (buf);
	 exit(1);
       }

       if(G_raster_map_type(name[noi], mapset[noi]) == CELL_TYPE)
       {
	 sprintf (buf, "%s is integer map, it can't be quantized", name[noi]);
	 G_fatal_error (buf);
	 exit(1);
       }
    }

    basename = basemap->answer;

    /* now figure out what new quant rules to write */
    if(truncate)
    {
	fprintf (stdout,"truncating...\n");
	G_quant_truncate (&quant_struct);
    }

    else if(round)
    {
	fprintf (stdout,"rounding...\n");
	G_quant_round (&quant_struct);
    }

    else if(strncmp(basename, "NONE",4) != 0) 
    /* set the quant to that of basemap */
    {
        basemapset = G_find_cell2 (basename, "");
        if (basemapset == NULL)
        {
 	    sprintf (buf, "%s - not found", basename);
	    G_fatal_error (buf);
	    exit(1);
        }

        if(G_raster_map_type(basename, basemapset) == CELL_TYPE)
        {
	    sprintf (buf, "%s is integer map, it can't be used as basemap", basename);
	    G_fatal_error (buf);
	    exit(1);
        }

	if(G_read_quant(basename, basemapset, &quant_struct)<=0)
        {
	    sprintf (buf, "Can't read quant rules for basemap %s! Exiting.", basename);
	    G_fatal_error (buf);
	    exit(1);
        }
     }

     else if((sscanf(fprange->answer, "%lf,%lf", &new_dmin, &new_dmax)==2)
          && (sscanf(range->answer, "%d,%d", &new_min, &new_max)==2))
     {
       fprintf (stdout,"Setting quant rules for input map[s] to (%f %f) -> (%d,%d)\n",
	       new_dmin, new_dmax, new_min, new_max);
       G_quant_add_rule(&quant_struct, new_dmin,new_dmax, new_min,new_max);
     }

     else /* ask user for quant rules */
     {

        if (!read_rules())
        {
	    if (isatty(0))
	        fprintf (stderr, "No rules specified. Quant table[s] not changed.\n");
	    else
	        G_fatal_error ("no rules specified");
	    exit(1);
        }

    } /* use rules */


    for(i=0; i < noi; i++)
    {
       if( G_write_quant(name[i], mapset[i], &quant_struct) < 0)
   	   fprintf (stdout,"Quant table not changed for %s\n", name[i]);
       else 
	   fprintf (stdout,"New quant table created for %s\n", name[i]);
    }

    exit(0);
}
