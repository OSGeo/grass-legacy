#define MAIN
#include "gis.h"
#include "display.h"
#include "Vect.h"
#include "raster.h"
#include "digit.h"
#include "whatvect.h"


int main(int argc, char **argv)
{
	struct Flag *once;
	struct Option *opt1;
	char *name, *mapset, *openvect();
	struct Map_info Map;
	struct Categories Cats;
	int level;

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;

	once = G_define_flag();
	once->key = '1';
	once->description = "Identify just one location";

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,dig,vector" ;
	opt1->description= "Name of existing vector map" ;

	if(G_parser(argc,argv))
		exit(1);

	name = opt1->answer;

	/* Look at maps given on command line */

	mapset = openvect (name);
	if (mapset == NULL)
	{
		fprintf (stderr, "Unable to open %s\n", name) ;
		exit(1) ;
	}


	R_open_driver();
	D_setup(0);

	level = Vect_open_old (&Map, name, mapset);
	if (level < 0)
		G_fatal_error ("Can't open vector file");
	;
	if (level < 2)
		G_fatal_error ("You must first run v.support on vector file");

	if (G_read_vector_cats(name, mapset, &Cats) < 0)
		Cats.num = -1  ;

	what(once->answer, &Map, &Cats); 
	R_close_driver();
	Vect_close (&Map);

	exit(0);
}



