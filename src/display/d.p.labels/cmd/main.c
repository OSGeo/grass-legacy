#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
	struct Cell_head window ;
	char window_name[64] ;
	char *label_name ;
	char *mapset ;
	FILE *infile ;
	char buff[128] ;
	int t, b, l, r ;
        struct Option *opt1;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    opt1 = G_define_option() ;
    opt1->key        = "file" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,paint/labels,paint labels" ;
    opt1->description= "Name of label file" ;

    if (G_parser(argc, argv))
        exit(-1);

/* Check command line */

/* Save map name */
	label_name = opt1->answer ;

/* Make sure map is available */
	mapset = G_find_file ("paint/labels", label_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Label file [%s] not available", label_name);
		G_fatal_error(buff) ;
	}

/* Open map is available */
	infile = G_fopen_old ("paint/labels", label_name, mapset) ;
	if (infile == NULL)
	{
		sprintf(buff,"Cant open labelfile [%s]", label_name);
		G_fatal_error(buff) ;
	}

	if (R_open_driver() != 0)
		G_fatal_error("No graphics device selected");

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting map window") ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;

/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

/* Go draw the cell file */
	do_labels(infile) ;

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();

	exit(0);
}
