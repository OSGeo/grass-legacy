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
    FILE *fd;
    char *polyfile, *mapname;
    int any;
    struct GModule *module;
    struct Option *output;

    /* must run in a term window */
    G_putenv("GRASS_UI_TERM","1");

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description =
      _("Interactive tool used to draw and save vector features on a graphics"
	" monitor using a pointing device (mouse) and save to a raster map.");

    output = G_define_standard_option(G_OPT_R_OUTPUT);

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    mapname = output->answer;

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
        G_fatal_error(_("No graphics device selected!"));
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

    if (any)
	create_map (mapname, polyfile);
    else
	G_message (_("No map created"));

    unlink (polyfile);

    return(EXIT_SUCCESS) ;
}
