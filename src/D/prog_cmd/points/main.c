/*
 *   Dpoints
 *
 *   Usage:  Dpoints color=name size=num type=x/diamond/box/+
 *
 *   Draws small marks at points listed in stdin.   
 */

#define USAGE1	"color=name size=num type=x/diamond/box/+"
#define USAGE2	"points, X Y, provided in stdin"
#include "gis.h"

#define MAIN
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	extern int stash_away() ;
	int i ;

	extern double D_get_d_north(), D_get_d_south() ;
	extern double D_get_d_east(), D_get_d_west() ;
	extern int D_move_abs(), D_cont_abs();



/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE1) ;
		fprintf(stderr,"       %s\n", USAGE2) ;
		exit(-1) ;
	}

/* Setup driver and check important information */
	R_open_driver();
	D_setup(0);
/* set D clip window */
	D_set_clip_window ( (int)D_get_d_north(),
			    (int)D_get_d_south(),
			    (int)D_get_d_west(),
			    (int)D_get_d_east());

/* setup the G plot to use the D routines */
	G_setup_plot ( D_get_d_north(),
		       D_get_d_south(),
		       D_get_d_west(),
		       D_get_d_east(),
		       D_move_abs, D_cont_abs);



	R_standard_color (color) ;
	switch(type)
	{
	case TYPE_X:
		draw_points_x(size) ;
		break ;
	case TYPE_PLUS:
		draw_points_plus(size) ;
		break ;
	case TYPE_BOX:
		draw_points_box(size) ;
		break ;
	case TYPE_DIAMOND:
		draw_points_diamond(size) ;
		break ;
	}

	if (infile != stdin)
	{
	/* Add this command to list */
		strcpy(buff, argv[0]) ;
		for(i=1; i<argc; i++)
		{
			strcat(buff, " ") ;
			strcat(buff, argv[i]) ;
		}
		D_add_to_list(buff) ;
	}

	R_close_driver();
}
