#define MAIN
#include <string.h>
#include "display.h"
#include "raster.h"
#include "gis.h"
#include "display.h"
#include "options.h"
#include "l_proto.h"

int main(int argc,char **argv)
{
	char *mapset         ;
	char buffer[256]      ;
	char nbuf[128], ebuf[128] ;
	extern int stash_away() ;
	char window_name[64] ;
	struct Option *opt1 ;
	struct Option *opt2 ;
	struct Option *opt3 ;
	struct Option *opt5 ;
	struct Option *opt7 ;
	struct Option *opt8 ;
	struct Option *opt9 ;
	struct Option *opt10 ;
	struct Option *opt11 ;
	struct Flag *flag1 ;
	struct Flag *flag2 ;
	struct Flag *flag3 ;
	char to_str[128] ;
	char from_str[128] ;

	G_gisinit(argv[0]) ;

	G_get_window(&window) ;

	set_default_options() ;

	G_format_northing(to_northing, nbuf, G_projection()) ;
	G_format_easting (to_easting,  ebuf, G_projection()) ;
	sprintf(to_str,"%s,%s,0", ebuf, nbuf) ;

	G_format_northing(from_northing, nbuf, G_projection()) ;
	G_format_easting (from_easting,  ebuf, G_projection()) ;
	sprintf(from_str,"%s,%s,20000", ebuf, nbuf) ;

    opt2 = G_define_option() ;
    opt2->key        = "map" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = YES ;
    opt2->gisprompt  = "old,cell,raster" ;
    opt2->description= "The raster map used to generate the color" ;

    opt1 = G_define_option() ;
    opt1->key        = "elevation" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "The raster map used to generate the 3-d texture" ;

    opt3 = G_define_option() ;
    opt3->key        = "from_coordinate" ;
    opt3->key_desc   = "x,y,z" ;
    opt3->type       = TYPE_STRING ;
    opt3->required   = NO ;
    opt3->answer     = from_str ;
    opt3->description= "coordinates of view point" ;

    opt5 = G_define_option() ;
    opt5->key        = "to_coordinate" ;
    opt5->key_desc   = "x,y,z" ;
    opt5->type       = TYPE_STRING ;
    opt5->required   = NO ;
    opt5->answer     = to_str ;
    opt5->description= "coordinates of center of view" ;

    opt7 = G_define_option() ;
    opt7->key        = "exaggeration" ;
    opt7->type       = TYPE_DOUBLE ;
    opt7->required   = NO ;
    opt7->answer     = "2.0" ;
    opt7->description= "vertical exaggeration factor" ;

    opt8 = G_define_option() ;
    opt8->key        = "lines" ;
    opt8->type       = TYPE_INTEGER ;
    opt8->required   = NO ;
    opt8->answer     = "1" ;
    opt8->description= "Frequency of vector lines in output" ;

    opt9 = G_define_option() ;
    opt9->key        = "field" ;
    opt9->type       = TYPE_DOUBLE ;
    opt9->required   = NO ;
    opt9->answer     = "30" ;
    opt9->description= "Field of view" ;

    opt10 = G_define_option() ;
    opt10->key        = "color" ;
    opt10->type       = TYPE_STRING ;
    opt10->required   = NO ;
    opt10->options    = G_malloc(strlen(D_color_list()) + 6);
    sprintf (opt10->options, "%s,color", D_color_list());
    opt10->answer     = "gray" ;
    opt10->description= "Color of vector lines" ;

    opt11 = G_define_option() ;
    opt11->key        = "box" ;
    opt11->type       = TYPE_STRING ;
    opt11->required   = NO ;
    opt11->options    = G_malloc(strlen(D_color_list()) + 6);
    sprintf (opt11->options, "%s,none", D_color_list());
    opt11->answer     = "none" ;
    opt11->description= "Color of bounding box" ;

	flag1 = G_define_flag() ;
	flag1->key         = 'l' ;
	flag1->description = "Plot lines only" ;

	flag2 = G_define_flag() ;
	flag2->key         = 'n' ;
	flag2->description = "Show null elevations" ;

	flag3 = G_define_flag() ;
	flag3->key         = 'a' ;
	flag3->description = "Calculate corner elevations by averaging 4 neighbor cells" ;

	if (G_parser(argc, argv))
			exit(1);

	lines_only = flag1->answer ;
	do_null = flag2->answer ;
	do_average = flag3->answer ;

	G_scan_easting  (opt3->answers[0], &from_easting,  G_projection()) ;
	G_scan_northing (opt3->answers[1], &from_northing, G_projection()) ;
	sscanf(opt3->answers[2],"%lf",&from_height) ;

	G_scan_easting  (opt5->answers[0], &to_easting,  G_projection()) ;
	G_scan_northing (opt5->answers[1], &to_northing, G_projection()) ;
	sscanf(opt5->answers[2],"%lf",&to_height) ;

	sscanf(opt7->answer,"%lf",&exag) ;
	sscanf(opt8->answer,"%d",&line_freq) ;
	sscanf(opt9->answer,"%lf",&field) ;
	field /= 2 ;
	strcpy (elevfile, opt1->answer) ;
	strcpy (file, opt2->answer) ;

	if (! strcmp("color", opt10->answer))
		line_color = -1 ;
	else
		line_color = D_translate_color (opt10->answer) ;

	if (! strcmp("none", opt11->answer))
		box_color = -1 ;
	else
		box_color = D_translate_color (opt11->answer) ;

/*
	fprintf (stdout,"\n3-d view request:\n") ;
	fprintf (stdout,"File plotted:    %s in %s\n", file, file_mapset) ;
	fprintf (stdout,"Elevation file:  %s in %s\n", elevfile, elevfile_mapset) ;
	fprintf (stdout,"\n") ;
	fprintf (stdout,"            View from:    View to:\n") ;
	fprintf (stdout,"Easting:  %12.2lf %12.2lf\n", from_easting, to_easting) ;
	fprintf (stdout,"Northing: %12.2lf %12.2lf\n", from_northing, to_northing) ;
	fprintf (stdout,"Height:   %12.2lf %12.2lf\n", from_height, to_height) ;
	fprintf (stdout,"\n") ;
	fprintf (stdout,"line frequency:         %d\n", line_freq) ;
	fprintf (stdout,"elevation exaggeration: %4.2lf\n", exag) ;
	fprintf (stdout,"field of view:          %4.2lf\n", field) ;
*/

/* Final check for existence of files */
	mapset = G_find_cell2 (file, "") ;
	if (mapset == NULL)
	{
		sprintf(buffer, "Map: [%s] not found", file) ;
		G_fatal_error(buffer) ;
		exit(-1) ;
	}
	strcpy (file_mapset, mapset);
	mapset = G_find_cell2 (elevfile, "") ;
	if (mapset == NULL)
	{
		sprintf(buffer, "Map: [%s] not found", elevfile) ;
		G_fatal_error(buffer) ;
		exit(-1) ;
	}
	strcpy (elevfile_mapset, mapset);

	if (check_options() )
	if (0 > G_set_window(&window) )
		G_fatal_error("Inappropriate window resolution request") ;
	G_get_set_window(&window) ;

	if(PROJECTION_LL == window.proj){
	    if(360. < (window.east - window.west + 2.*window.ew_res)){
		window.east  -= window.ew_res ;
		window.west  += window.ew_res ;
		window.cols -= 2 ;
	    }	
	    if(180. < (window.north-window.south + 2.*window.ns_res)){
		window.north -= window.ns_res ;
		window.south += window.ns_res ;
		window.rows -= 2 ;
	    }
	    if (0 > G_set_window(&window) )
		G_fatal_error("Inappropriate resolution request");
	}

	/* adjust window one extra row n, s, e, w */
	window.rows += 2 ;
	window.cols += 2 ;
	window.north += window.ns_res ;
	window.south -= window.ns_res ;
	window.east  += window.ew_res ;
	window.west  -= window.ew_res ;

	if (0 > G_set_window(&window) )
		G_fatal_error("Inappropriate window resolution request") ;

/* Set up graphics */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics frame") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics frame not available") ;

	establish_view(from_easting,from_northing,from_height,to_easting,to_northing,to_height,field) ;

	threed(0);

	D_add_to_list(G_recreate_command()) ;

	R_close_driver() ;

	exit(0);
}
