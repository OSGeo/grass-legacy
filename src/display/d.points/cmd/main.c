#include "gis.h"

#define GLOBAL
#include "options.h"

main(argc, argv)
int argc ;
char **argv ;
{
	char buff[512] ;
	char window_name[64] ;
	struct Cell_head window ;
	int i ;
	int t, b, l, r ;
	struct Option *opt1, *opt2, *opt3, *opt4;

	opt1 = G_define_option() ;
	opt1->key        = "color" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO ;
	opt1->answer     = "gray" ;
	opt1->options="red,orange,yellow,green,blue,indigo,violet,gray,brown,white,black";
	opt1->description= "Sets the current color to that stated" ;

	opt2 = G_define_option() ;
	opt2->key        = "size" ;
	opt2->type       = TYPE_INTEGER ;
	opt2->required   = NO ;
	opt2->answer     = "5" ;
	opt2->options    = "0-1000" ;
	opt2->description= "Size, in pixels, in which the icon is to be drawn" ;

	opt3 = G_define_option() ;
	opt3->key        = "type" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO ;
	opt3->answer     = "+" ;
	opt3->options    = "x,diamond,box,+" ;
	opt3->description= "Specify the type of the icon" ;

	opt4 = G_define_option() ;
	opt4->key        = "file" ;
	opt4->type       = TYPE_STRING ;
	opt4->required   = NO ;
	opt4->answer     = NULL ;
	opt4->description= "Input is a UNIX file name" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */

	if (G_parser(argc, argv))
		exit(-1);

	color = D_translate_color(opt1->answer) ;
	if (color == 0)
	{
		printf("Don't know the color %s\n", opt1->answer);
		G_usage() ;
		exit(-1);
	}

	sscanf(opt2->answer,"%d",&size);

	if (! strcmp(opt3->answer, "x"))
		type = TYPE_X ;
	else if (! strcmp(opt3->answer, "+"))
		type = TYPE_PLUS ;
	else if (! strcmp(opt3->answer, "box"))
		type = TYPE_BOX ;
	else if (! strcmp(opt3->answer, "diamond"))
		type = TYPE_DIAMOND ;

	if (opt4->answer != NULL)
	{
		infile = fopen(opt4->answer, "r") ;
		if (infile == NULL)
		{
			fprintf(stderr, "File %s not available\n", opt4->answer)
			    ;
			G_usage() ;
			exit(-1);
		}
		temp_file = NULL;
		temp_name = NULL;
	}
	else {
		infile = stdin ;
		temp_name = G_tempfile();
		if ((temp_file = fopen(temp_name, "w")) == NULL)
			G_fatal_error("Unable to open temporary file in d.points.");
	}

	/* Setup driver and check important information */
	R_open_driver();

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

	/* Do the plotting */
	R_standard_color (color) ;
	switch(type)
	{
	case TYPE_X:
		draw_points_x(&window) ;
		break ;
	case TYPE_PLUS:
		draw_points_plus(&window) ;
		break ;
	case TYPE_BOX:
		draw_points_box(&window) ;
		break ;
	case TYPE_DIAMOND:
		draw_points_diamond(&window) ;
		break ;
	}

	/* Add this command to list */
	/* This doesn't make sense
        strcpy(buff, argv[0]) ;
        for(i=1; i<argc; i++) {
            strcat(buff, " ") ;
            strcat(buff, argv[i]) ;
		}
        if (infile == stdin)
        {
          strcat(buff, " < ");
          strcat(buff, temp_name);
          fclose(temp_file);
        }
        D_add_to_list(buff) ;
	*/

	R_close_driver();
}
