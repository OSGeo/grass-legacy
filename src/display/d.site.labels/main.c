/* Hacked for new sites API by Eric G. Miller <egm2@jps.net> 2000-10-01 */
/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

#define DEFAULT_ATTRIBUTE "string"
#define DEFAULT_INDEX "1"

int main (int argc, char **argv)
{
	struct Cell_head window ;
	char window_name[64] ;
	char *site_name ;
	char *mapset, position[MAX_SITE_STRING] ;
	FILE *infile ;
	char buff[128] ;
	int t, b, l, r ;
	int i, column, index ;
        struct Option *opt1, *opt2, *yref_opt, *opt3, *opt4, *opt6;
	struct Option *opt7, *opt9, *col_opt, *index_opt ;
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

    col_opt = G_define_option();
    col_opt->key     = "attr";
    col_opt->type    = TYPE_STRING;
    col_opt->required = NO;
    col_opt->description = "Type of attribute to use for labels";
    col_opt->options = "string,cat,double,coords,dim";
    col_opt->answer  = DEFAULT_ATTRIBUTE;

    index_opt = G_define_option();
    index_opt->key = "index";
    index_opt->type = TYPE_STRING;
    index_opt->required = NO;
    index_opt->description = "Index of attribute. Ignored when attr=cat or attr=coords.";
    index_opt->answer = DEFAULT_INDEX;
    
    opt2 = G_define_option() ;
    opt2->key        = "xref" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = NO ;
    opt2->options    = "left,center,right";
    opt2->answer     = "center";
    opt2->description = "Relative horizontal position 'left|center|right'" ;

    yref_opt = G_define_option();
    yref_opt->key = "yref";
    yref_opt->type = TYPE_STRING;
    yref_opt->required = NO;
    yref_opt->options = "top,bottom";
    yref_opt->answer  = "bottom";
    yref_opt->description = "Relative vertical position 'top|bottom'" ;

    opt4 = G_define_option() ;
    opt4->key        = "size" ;
    opt4->type       = TYPE_DOUBLE ;
    opt4->required   = NO ;
    opt4->answer     = "10";
    opt4->description= "Size of text (pixels)" ;

    opt3 = G_define_option() ;
    opt3->key        = "color" ;
    opt3->type       = TYPE_STRING ;
    opt3->required   = NO ;
    opt3->options    = "red,white,magenta,brown,blue,indigo,yellow,black,orange,green,violet,grey";
    opt3->answer     = "white";
    opt3->description= "Text color" ;

    opt6 = G_define_option() ;
    opt6->key        = "backgr" ;
    opt6->type       = TYPE_STRING ;
    opt6->required   = NO ;
    opt6->answer     = "none" ;
    opt6->options    = "grey,red,white,magenta,brown,blue,indigo,yellow,black,orange,green,violet,none";
    opt6->description= "Background color" ;

    opt7 = G_define_option() ;
    opt7->key        = "border" ;
    opt7->type       = TYPE_STRING ;
    opt7->required   = NO ;
    opt7->answer     = "none" ;
    opt7->options    = "grey,red,white,magenta,brown,blue,indigo,yellow,black,orange,green,violet,none";
    opt7->description= "Border color" ;

    opt9 = G_define_option() ;
    opt9->key        = "font" ;
    opt9->type       = TYPE_STRING ;
    opt9->required   = NO ;
    opt9->options    = "cyrilc,gothgbt,gothgrt,gothitt,greekc,greekcs,greekp,"
    	"greeks,italicc,italiccs,italict,romanc,romancs,romand,"
	"romans,romant,scriptc,scripts";
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

/* Index column and number */
  	if(strcmp(col_opt->answer, "string") == 0) {
          	column = SITE_ATTR_STR;
  	}
  	else if (strcmp(col_opt->answer, "cat") == 0) {
          	column = SITE_ATTR_CAT;
  	}
  	else if (strcmp(col_opt->answer, "double") == 0) {
          	column = SITE_ATTR_DBL;
  	}
  	else if (strcmp(col_opt->answer, "coords") == 0) {
          	column = SITE_ATTR_COORD;
  	}
  	else if (strcmp(col_opt->answer, "dim") == 0) {
          	column = SITE_ATTR_DIM;
  	}
  	else {
          	G_fatal_error("Unknown attribute type!\n");
  	}

  	index = atoi(index_opt->answer) - 1;
  	if (index < 0) {
          	G_fatal_error("Index must be a positive number greater than zero!\n");
  	}

/* Default positioning hack (should fix do_labels) */
	sprintf(position, "%s %s", opt2->answer, yref_opt->answer);

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
	do_labels(infile,window, position, opt3->answer, opt4->answer, 
			opt6->answer, opt7->answer, 
			opt9->answer, column, index, mouse->answer);

	D_add_to_list(G_recreate_command()) ;

	fclose(infile) ;

	R_close_driver();

	return 0;
}
