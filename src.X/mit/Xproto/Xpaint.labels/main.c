/*  %W%  %G%  */

/*
 *   Xpaint.labels
 *   Usage:  Xpaint.labels label_file
 */

#define USAGE   "label_file"

#include "gis.h"

struct Cell_head window;

main(argc, argv)
int argc;
char **argv;
{
    char *label_name;
    char *mapset;
    FILE *infile;
    char buff[128];

	/* Initialize the GIS calls */
    G_gisinit(argv[0]);

	/* Check command line */
    if (argc != 2) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
	/* Save map name */
    label_name = argv[1];

	/* Make sure map is available */
    mapset = G_find_file("paint/labels", label_name, "");
    if (mapset == NULL) {
        sprintf(buff, "Label file [%s] not available", label_name);
        G_fatal_error(buff);
    }
	/* Open map is available */
    infile = G_fopen_old("paint/labels", label_name, mapset);
    if (infile == NULL) {
        sprintf(buff, "Cant open labelfile [%s]", label_name);
        G_fatal_error(buff);
    }
/*
    if (D_get_cur_wind(window_name))
        G_fatal_error("No current window") ;

    if (D_set_cur_wind(window_name))
        G_fatal_error("Current window not available") ;
*/


	/* Read in the map window associated with window */
    G_get_window(&window);

/*
    if (D_check_map_window(&window))
        G_fatal_error("Setting map window") ;

    if (G_set_window(&window) == -1)
        G_fatal_error("Current window not settable") ;
*/

/* Determine conversion factors
    if (D_get_screen_window(&t, &b, &l, &r))
        G_fatal_error("Getting screen window") ;
    if (D_do_conversions(&window, t, b, l, r))
        G_fatal_error("Error in calculating conversions") ;
*/


/* Go draw the cell file */
    do_labels(infile);


/* Add this command to list
    strcpy(buff, argv[0]) ;
    for(i=1; i<argc; i++)
    {
        strcat(buff, " ") ;
        strcat(buff, argv[i]) ;
    }
    D_add_to_list(buff) ;
*/

}
