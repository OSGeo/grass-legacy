#define MAIN
#include "gis.h"
#include "digit.h"


main(argc, argv)
int argc ;

char **argv ;
{
	struct Flag *once;
	struct Option *opt1;
	char *name, *mapset, *openvect();
	struct Map_info Map;
	struct Categories Cats;
	int level;

struct Flag *xycoord;
struct Option *opt2;
double x, y;
int n;

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;

	once = G_define_flag();
	once->key = '1';
	once->description = "Identify just one location";

xycoord = G_define_flag();
xycoord->key = 'i';
xycoord->description = "Input x and y coordinates";

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,dig,vector" ;
	opt1->description= "Name of existing vector map" ;

opt2 = G_define_option() ;
opt2->key        = "coord" ;
opt2->key_desc   = "East and North Coordinates" ;
opt2->type       = TYPE_STRING ;
opt2->required   = NO ;
opt2->description= "One or More East and North coordinates" ;
opt2->multiple   = YES ;
opt2->answers    = NULL ;

	if(G_parser(argc,argv))
		exit(1);

	name = opt1->answer;

for (n=0; opt2->answers[n] != NULL; n+=2)
{
G_scan_easting (opt2->answers[n], &x, G_projection());
G_scan_northing (opt2->answers[n+1], &y, G_projection());
}

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

if (opt2->answer[0] != NULL)
        whatxy(x, y, &Map, &Cats);
else
	what(once->answer, &Map, &Cats); 

	R_close_driver();
	Vect_close (&Map);

	exit(0);
}



