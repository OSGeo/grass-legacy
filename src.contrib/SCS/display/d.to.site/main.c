/*  %W%  %G%  */

#define MAIN
#define ERR -1
#include <stdio.h>
#include "gis.h"


main(argc,argv)
int argc;
char *argv[];

{
	char label[100];
    struct Cell_head window ;
    char temp[128] ;
    int t, b, l, r ;
	struct Option *siteopt;


/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
 
		  /* set up the options and flags for the command line parser */
		   
   siteopt = G_define_option();
   siteopt->key             = "sites";
   siteopt->type            =  TYPE_STRING;
   siteopt->required        =  YES;
   siteopt->description		=	"The name of the site file to create";
 
		/* heeeerrrrrre's the   PARSER */
	if (G_parser (argc, argv))
			exit (-1);

		 
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

    where(siteopt->answer);

    R_close_driver();
 
}

