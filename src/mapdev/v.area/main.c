/*
 *   v.area
 *
 *   Usage:  v.area [-fill] vector-file-name [color]
 *
 */

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

#ifndef GET_VAL
#define GET_VAL 0
#endif

#ifndef SET_VAL
#define SET_VAL 1
#endif

struct Cell_head window ;

int main(int argc, char *argv[])
{
    char window_name[64] ;
    int t, b, l, r ;
    char *mapset;
    char name[20];
    char color[8];
    int fill;
    int length;
    int i_flag;
    int f2;
    static int cflag = {0};
    char gisbase[256];
    char command[512];
    char Dvect_color[8];
    struct Flag *flag1, *flag2 ;
	struct Option *opt1 ;
	struct Option *opt2 ;

	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "map";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "input vector filename" ;

    opt2 = G_define_option() ;
	opt2->key        = "color";
	opt2->type       = TYPE_STRING;
	opt2->required   = NO;
	opt2->answer     = "red";
	opt2->description= "set the color";

/* Define the different flags */

	flag1 = G_define_flag() ;
	flag1->key         = 'f' ;
	flag1->description = "fill" ;


	flag2 = G_define_flag();
	flag2->key  = 'i';
	flag2->description = "subtract area of islands";

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

	fill = 0;

	if (G_parser(argc, argv) < 0)
		exit(-1);

/* Check the length of the "name" to ensure it is not too long */
	length = strlen(opt1->answer);
	if (length > 19)
	{
fprintf(stderr,"\nFile name:'%s' is LONGER than 19 chars\n\n",opt1->answer);
		 exit(-1);
	}
	strcpy(name,opt1->answer);

/* Check the length of the "color" to ensure it is not too long */
	length = strlen(opt2->answer);
	if (length > 7)
	{
		fprintf(stderr,"\nColor:  '%s' is LONGER than the allowable 7 characters.\n\n", opt2->answer);
		cflag = 1;
    }
	strcpy(color,opt2->answer);


	fill = flag1->answer;

/* Determine the "mapset" based on your order of mapsets */
    if ( ! (mapset = G_find_vector2(name,"")) )
     {
      fprintf(stderr,"\nVector file name:  '%s' NOT found.\n\n",name);
      exit(-1);
     }
      
    if (cflag)
     {
      fprintf(stderr,"\nThe default color 'red' will be used.\n\n");
     }
/* Do we calculate and subtract the inner areas (islands)? */

    f2 = (int)flag2->answer;
    proc_i_flag( SET_VAL, &f2 );
    fprintf(stderr, "Value set is %d\n", flag2->answer);

/* Check if color is "white" and fill option is "off" */
    if ( (strcmp(color,"white")==0) && (fill!=1) )
      strcpy(Dvect_color,"red");
    else
      strcpy(Dvect_color,"white");

/* Perform "d.vect" for vector file "name" */
    strcpy(gisbase,G_gisbase() );
    sprintf(command,"d.vect %s color=%s",name,Dvect_color);
    system(command);

    R_open_driver();

    if (D_get_cur_wind(window_name))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(window_name))
	G_fatal_error("Current graphics window not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting map window") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Current graphics window not settable") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting screen window") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;
    
    where_am_i(name,mapset,color,fill,Dvect_color) ;

    fprintf(stderr,"\n") ;

    R_close_driver();

    exit(1);
}
