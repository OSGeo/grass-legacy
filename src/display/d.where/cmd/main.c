#include <string.h>
#include "gis.h"
#include "CC.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
	struct GModule *module;
    struct Option *spheroid;
    struct Flag *once;
    char s_names[2048];
    char *name;
    int have_spheroid;
    int with_info;
    double a,e;
    int i;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Identifies the geographic coordinates associated with "
		"point locations in the active frame on the graphics monitor.";

    once = G_define_flag() ;
    once->key        = '1' ;
    once->description= "one mouse click only";

    spheroid = G_define_option() ;
    spheroid->key        = "spheroid" ;
    spheroid->type       = TYPE_STRING ;
    spheroid->required   = NO ;
    spheroid->description= "Name of a spheroid (for lat/lon coordinate conversion)";
    spheroid->options    = s_names;

    *s_names = 0;
    for (i = 0; name = CC_spheroid_name(i); i++)
    {
	if(i) strcat (s_names, ",");
	strcat (s_names,name);
    }


    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    have_spheroid = 0;
    if(name = spheroid->answer)
    {
	if (G_projection() != PROJECTION_UTM)
	{
	    fprintf (stderr,"WARNING: %s=%s: only valid for UTM databses. Ignored\n",
		spheroid->key, spheroid->answer);    
	}
	else if (CC_get_spheroid (name, &a, &e) == 0)
	{
	    fprintf (stderr,"ERROR: %s=%s: unknown spheroid\n",
		spheroid->key, spheroid->answer);    
	    exit(-1);
	}
	else
	{
	    CC_u2ll_spheroid_parameters (a,e);
	    CC_u2ll_zone (G_zone());
	    have_spheroid = 1;
	}
    }

    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");
    D_setup(0);
    where_am_i(once->answer, have_spheroid) ;
    R_close_driver();

    exit(0);
}
