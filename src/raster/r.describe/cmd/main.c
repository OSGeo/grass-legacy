#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
        int as_int;
	int verbose;
	int compact;
	int range;
	int windowed;
	int nsteps;
	char name[200];
	char *mapset;
	char msg[100];
	char *no_data_str;
	struct GModule *module;
	struct
	{
	  struct Flag *one;
	  struct Flag *r;
	  struct Flag *q;
	  struct Flag *d;
	  struct Flag *i;
	} flag;
	struct
	{
	  struct Option *map;
	  struct Option *nv;
	  struct Option *nsteps;
	} option;

	module = G_define_module();
	module->description =
		"Prints terse list of category values found in a raster map layer.";

	/* define different options */
	option.map = G_define_option() ;
	option.map->key        = "map" ;
	option.map->type       = TYPE_STRING ;
	option.map->required   = YES ;
	option.map->multiple   = NO;
	option.map->description= "Name of raster map" ;
	option.map->gisprompt  = "old,cell,raster" ;

	option.nv = G_define_option() ;
	option.nv->key		= "nv" ;
	option.nv->type		= TYPE_STRING ;
	option.nv->required	= NO ;
	option.nv->multiple	= NO ;
	option.nv->answer	= "*" ;
	option.nv->description	= "string representing no data cell value";

        option.nsteps = G_define_option() ;
        option.nsteps->key          = "nsteps" ;
        option.nsteps->type         = TYPE_INTEGER ;
        option.nsteps->required     = NO ;
        option.nsteps->multiple     = NO ;
        option.nsteps->answer       = "255" ;
        option.nsteps->description  = "number of quantization steps";

	/*define the different flags */

	flag.one =G_define_flag() ;
	flag.one->key         = '1';
	flag.one->description = "Print the output one value per line";

	flag.r =G_define_flag() ;
	flag.r->key         = 'r';
	flag.r->description = "Only print the range of the data";

	flag.q =G_define_flag() ;
	flag.q->key        = 'q';
	flag.q->description = "Quiet";

	flag.d =G_define_flag() ;
	flag.d->key        = 'd';
	flag.d->description = "Use the current region";

	flag.i =G_define_flag() ;
	flag.i->key        = 'i';
	flag.i->description = "read fp map as integer";



	G_gisinit (argv[0]);

	verbose = 1;

	if (0 > G_parser(argc,argv))
		exit(-1);

	verbose = (! flag.q->answer);
	compact = (! flag.one->answer);
	range =  flag.r->answer;
	windowed =  flag.d->answer;
	as_int =  flag.i->answer;
	no_data_str =  option.nv->answer;
	if (sscanf(option.nsteps->answer, "%d", &nsteps) != 1 || nsteps < 1) {
	  fprintf(stderr, "ERROR: %s = %s -- must be greater than zero\n",
               option.nsteps->key,option.nsteps->answer);
	  G_usage();
	  exit(1);
	}
	strcpy (name, option.map->answer);

	if (mapset =  G_find_cell2 (name, ""))
	{
		describe(name, mapset, compact, verbose, no_data_str,
			range, windowed, nsteps, as_int);
		exit(0);
	}
	sprintf (msg,"%s: [%s] not found", G_program_name(), name);
	G_fatal_error (msg);
	exit(1);
}
