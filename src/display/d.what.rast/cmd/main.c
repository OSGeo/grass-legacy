#define GLOBAL
#include "what.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
	struct Cell_head window ;
	char temp[128] ;
	int t, b, l, r ;
	int i;
	char **ptr;
	struct Flag *once, *terse, *colrow;
	struct Option *opt1, *fs;


	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;


	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO ;
	opt1->multiple   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= G_malloc(120);
	sprintf(opt1->description, "Name of existing raster map(s). Limit: %d maps.\n\tDefault: Current map on screen", MAX_LAYERS);

	fs = G_define_option ();
	fs->key 	= "fs";
	fs->type 	= TYPE_STRING;
	fs->required	= NO;
	fs->answer	= ":";
	fs->description = "Field separator (terse mode only)";

	once = G_define_flag();
	once->key         = '1';
	once->description = "Identify just one location";

	terse = G_define_flag();
	terse->key 	   = 't';
	terse->description = "Terse output. For parsing by programs.";

	colrow = G_define_flag();
	colrow->key 	    = 'c';
	colrow->description = "Print out col and row for entire map region";


	if (argc != 1)		/* NON-interactive */
	{
		if (G_parser(argc, argv))
			exit(-1);
	}

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

	/* Look at maps given on command line */
	if((ptr = opt1->answers) != NULL)
		for (; *ptr != NULL; ptr++)
		{
			if (nlayers >= MAX_LAYERS)
			{
				fprintf (stderr, "warning: only first %d files specified will be used\n", MAX_LAYERS);
				break;
			}
			if ((fd[nlayers] = opencell (*ptr, name[nlayers], mapset[nlayers])) >= 0)
				nlayers++;
		}
	/* If we are to use currently displayed map */
	else /* if (isatty(0))    I dont figure this one??  -dpg */
	{
		if(D_get_cell_name (temp))
			fprintf (stderr, "warning: no data layer drawn in current window\n");
		else if ((fd[nlayers] = opencell (temp, name[nlayers], mapset[nlayers])) >= 0)
			nlayers++;
	}

	if (nlayers == 0)
	{
		G_usage() ;
		exit(-1);
	}

	for (i = 0; i < nlayers; i++)
	{
		if (G_read_cats (name[i], mapset[i], &cats[i]) < 0)
			cats[i].ncats = -1;
	}

	what (once->answer, terse->answer, colrow->answer, fs->answer) ;

	R_close_driver();
	exit(0);
}
