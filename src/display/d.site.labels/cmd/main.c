#include "gis.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	struct Cell_head window ;
	char window_name[64] ;
	char *site_name ;
	char *mapset ;
	FILE *infile ;
	char buff[128] ;
	int t, b, l, r ;
	int i ;
        struct Option *opt1, *opt2, *opt3, *opt4, *opt5, *opt6;
	struct Option *opt7, *opt8, *opt9 ;
	struct Flag *mouse;
	

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    mouse = G_define_flag() ;
    mouse->key = 'm';
    mouse->description =  "Use mouse to interactively place scale" ;

    opt1 = G_define_option() ;
    opt1->key        = "file" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,site_lists,sites" ;
    opt1->description= "Name of sites file" ;

    opt2 = G_define_option() ;
    opt2->key        = "ref" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = NO ;
    opt2->answer     = "center";
    opt2->description= "Relative position" ;

    opt3 = G_define_option() ;
    opt3->key        = "color" ;
    opt3->type       = TYPE_STRING ;
    opt3->required   = NO ;
    opt3->options    = "red,white,magenta,brown,blue,yellow,black,orange,green,violet,grey";
    opt3->answer     = "white";
    opt3->description= "Text color" ;

    opt4 = G_define_option() ;
    opt4->key        = "size" ;
    opt4->type       = TYPE_DOUBLE ;
    opt4->required   = NO ;
    opt4->answer     = "10";
    opt4->description= "Size of text (pixels)" ;

    opt5 = G_define_option() ;
    opt5->key        = "width" ;
    opt5->type       = TYPE_INTEGER ;
    opt5->required   = NO ;
    opt5->answer     = "1" ;
    opt5->description= "Width of text" ;

    opt6 = G_define_option() ;
    opt6->key        = "backgr" ;
    opt6->type       = TYPE_STRING ;
    opt6->required   = NO ;
    opt6->answer     = "none" ;
    opt6->options    = "grey,red,white,magenta,brown,blue,yellow,black,orange,green,violet,none";
    opt6->description= "Background color" ;

    opt7 = G_define_option() ;
    opt7->key        = "border" ;
    opt7->type       = TYPE_STRING ;
    opt7->required   = NO ;
    opt7->answer     = "none" ;
    opt7->options    = "grey,red,white,magenta,brown,blue,yellow,black,orange,green,violet,none";
    opt7->description= "Border color" ;

    opt8 = G_define_option() ;
    opt8->key        = "opaque" ;
    opt8->type       = TYPE_STRING ;
    opt8->required   = NO ;
    opt8->answer     = "no" ;
    opt8->options    = "yes,no" ;
    opt8->description= "opaque (yes / no)" ;

    opt9 = G_define_option() ;
    opt9->key        = "font" ;
    opt9->type       = TYPE_STRING ;
    opt9->required   = NO ;
    opt9->options    = "romans,romanc,romancs,romand,romanp,romant,italicc,italiccs,italict" ;
    opt9->answer     = "romans" ;
    opt9->description= "Fontname" ;

    if (G_parser(argc, argv))
        exit(-1);

/* Check command line */

/* Save map name */
	site_name = opt1->answer ;

/* Make sure map is available */
	mapset = G_find_sites (site_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Sites file [%s] not available", site_name);
		G_fatal_error(buff) ;
	}

/* Open map is available */
	infile = G_fopen_sites_old (site_name, mapset) ;
	if (infile == NULL)
	{
		sprintf(buff,"Cant open sitesfile [%s]", site_name);
		G_fatal_error(buff) ;
	}

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

/* Read in the map window associated with window */
 	G_get_window (&window);
	if (D_check_map_window(&window))
		G_fatal_error("Setting map window") ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;

/* Determine conversion factors */
	t = b = l = r = 0;
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

/* Go draw the cell file */
	do_labels(infile,window,opt2->answer, opt3->answer, opt4->answer, 
			opt5->answer,opt6->answer, opt7->answer, opt8->answer, 
			opt9->answer, mouse->answer);

	D_add_to_list(G_recreate_command()) ;

	fclose(infile) ;

	R_close_driver();
}
