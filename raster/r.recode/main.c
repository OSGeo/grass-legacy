#define MAIN
#include <stdio.h>
#include <unistd.h>
#include "global.h"

int 
main (int argc, char *argv[])
{
    char *title;
    char buf[1024];
    int i;
	struct GModule *module;
    struct
    {
	struct Option *input, *output, *title;
	struct Flag *a;
    } parm;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		"Recode raster maps.";
					        
    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->required = YES;
    parm.input->type = TYPE_STRING;
	parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description =  "Raster map to be recoded";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->required = YES;
    parm.output->type = TYPE_STRING;
	parm.output->gisprompt  = "new,cell,raster" ;
    parm.output->description =  "Name for the resulting raster map";

    parm.title = G_define_option();
    parm.title->key = "title";
    parm.title->required = NO;
    parm.title->type = TYPE_STRING;
    parm.title->description =  "Title for the resulting raster map";

    parm.a = G_define_flag() ;
    parm.a->key         = 'a' ;
    parm.a->description = "Align the current region to the input map";

    if (G_parser(argc, argv))
	exit(1);
    name = parm.input->answer;
    result     = parm.output->answer;
    title      = parm.title->answer;
    align_wind = (parm.a->answer);

    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
	sprintf (buf, "%s - not found", name);
	G_fatal_error (buf);
	exit(1);
    }
    if (G_legal_filename(result) < 0)
    {
	sprintf (buf, "%s - illegal name", result);
	G_fatal_error (buf);
	exit(1);
    }
    if (strcmp(name,result)==0 && strcmp(mapset,G_mapset())== 0)
    {
	G_fatal_error ("input map can NOT be the same as output map");
	exit(1);
    }

    if (!read_rules())
    {
	if (isatty(0))
	    fprintf (stderr, "no rules specified. %s not created\n", result);
	else
	    G_fatal_error ("no rules specified");
	exit(1);
    }
    no_mask = 0;
    do_recode();

    exit(0);
}
