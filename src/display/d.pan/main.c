/*
 *   d.pan
 *
 *   Change region through graphics - select new center
 */

#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
    int stat;
    struct Flag *quiet;
    struct Option *zoom;
    double magnify;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";

    zoom = G_define_option() ;
    zoom->key        = "zoom" ;
    zoom->type       = TYPE_DOUBLE ;
    zoom->required   = NO ;
    zoom->answer     = "1.0" ;
    zoom->options    = "0.001-1000.0" ;
    zoom->description= "magnification: >1.0 zooms in, <1.0 zooms out" ;

    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    sscanf(zoom->answer,"%lf", &magnify); 

    R_open_driver();

    D_setup(0);

    if (G_projection() == PROJECTION_LL)
    {
    }

/* Do the pan */
    stat = pan(quiet->answer,magnify) ;

    R_close_driver();

    exit(stat);
}
