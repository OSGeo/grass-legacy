/*
 *   Digitize a raster map on the screen image
 */

#define MAIN
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <grass/gis.h>
#include <grass/raster.h>
#include "local_proto.h"
#include <grass/glocale.h>

int main (int argc, char **argv)
{
    struct GModule *module;
    char *polyfile;
    char name[GNAME_MAX];
    FILE *fd;
    int any;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

        if (argc > 1 && ( strcmp(argv[1], "help") == 0 ||
                          strcmp(argv[1], "--help") == 0) )
        {
                G_message(_("Interactive tool used to draw and save "
                "vector features on a graphics monitor using a pointing "
                "device (mouse)."));
                exit(EXIT_SUCCESS);
        }

	module = G_define_module();
	module->description =
		_("Interactive tool used to draw and save "
		"vector features on a graphics monitor using a pointing "
		"device (mouse).");

    if(getenv("GRASS_ANOTHER_BUTTON")){
	    leftb   = 1; lefts   = "Left:  ";
	    middleb = 3; middles = "Right: ";
	    rightb  = 2; rights  = "Middle:";
    }else{
	    leftb   = 1; lefts   = "Left:  ";
	    middleb = 2; middles = "Middle:";
	    rightb  = 3; rights  = "Right: ";
    }

#ifdef DEBUG
    polyfile = "/tmp/r.digit.out";
#else
    polyfile = G_tempfile();
#endif
    fd = fopen (polyfile, "w");
    if (fd == NULL)
    {
	perror (polyfile);
	exit(EXIT_FAILURE);
    }

/* open the graphics and get it setup */
    if (R_open_driver() != 0)
        G_fatal_error ("No graphics device selected!!!");
    setup_graphics();

/* Do the digitizing and record the output into the polyfile */
    any = digitize(fd) ;
    fclose (fd);

/* close the graphics */
    R_close_driver();


#ifdef DEBUG
    fprintf (stdout,"Output is in %s\n", polyfile);
    exit(EXIT_FAILURE);
#endif

/* ask for a map name */
    if (any && get_map_name(name))
	create_map (name, polyfile);
    else
	G_message (_("No map created"));
    unlink (polyfile);
    return(EXIT_SUCCESS) ;
}
