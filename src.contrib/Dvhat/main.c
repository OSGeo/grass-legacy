/*  %W%  %G%  */

/*
 *   Dwhat
 *
 *   Usage:  Dvhat [layer names]
 *
 */

#define MAIN
#include "what.h"
#include "gis.h"
#include "digit.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    struct Cell_head window ;
    char temp[128] ;
    int t, b, l, r ;
    int i;

/* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    R_open_driver();

    if (D_get_cur_wind(temp))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(temp))
	G_fatal_error("Current graphics window not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting graphics window") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Can't set current graphics window") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting graphics window coordinates") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;

    nlayers = 0;
    for (i = 1; i < argc; i++)
    {
	if (nlayers >= MAX_LAYERS)
	{
	    fprintf (stderr, "warning: only %d layers allowed\n", MAX_LAYERS);
	    break;
	}
/*
	if (strcmp (argv[i], "-") == 0)
	{
	    if(D_get_cell_name (temp))
		fprintf (stderr, "warning: no data layer drawn in current window\n");
	    else if ((fd[nlayers] = opencell (temp, name[nlayers], mapset[nlayers])) >= 0)
		nlayers++;
	}
	else 
*/
	if (openvect (argv[i], name, mapset) >= 0)
	{
		/*
		fprintf (stderr, "openning   '%s' in '%s'\n", name, mapset);
		*/
		/*
		dig_P_init (name[nlayers], mapset[nlayers], &Map);
		*/
		dig_P_init (name, mapset, &Map);
		nlayers++;
	}
    }
    /*
    if (argc == 1)
    {
	if(D_get_cell_name (temp))
	    fprintf (stderr, "warning: no data layer drawn in current window\n");
	else if ((fd[nlayers] = opencell (temp, name[nlayers], mapset[nlayers])) >= 0)
	    nlayers++;
    }
    */
    if (nlayers == 0)
	fprintf ("No layers specified\n"), exit(0);

    for (i = 0; i < nlayers; i++)
    {
	/*
	if (G_read_vector_cats (name[i], mapset[i], &cats[i]) < 0)
	    cats[i].num = -1;
	*/
    }

    what ();


    R_close_driver();
}
