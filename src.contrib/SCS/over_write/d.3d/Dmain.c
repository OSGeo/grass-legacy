#define MAIN
#include "gis.h"
#include "options.h"

main(argc, argv)  char *argv[] ;
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
	struct Flag *flag1 ;
	struct Flag *flag2 ;
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
    opt1->required   = NO ;
    opt1->answer     = "elevation" ;
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
    opt8->answer     = "10" ;
    opt8->description= "Frequency of vector lines in output" ;

    opt9 = G_define_option() ;
    opt9->key        = "field" ;
    opt9->type       = TYPE_DOUBLE ;
    opt9->required   = NO ;
    opt9->answer     = "40" ;
    opt9->description= "Field of view" ;

    opt10 = G_define_option() ;
    opt10->key        = "color" ;
    opt10->type       = TYPE_STRING ;
    opt10->required   = NO ;
    opt10->options = "color,white,red,orange,yellow,green,blue,indigo,violet,gray,black";
    opt10->answer     = "gray" ;
    opt10->description= "Color of vector lines" ;

	flag1 = G_define_flag() ;
	flag1->key         = 'l' ;
	flag1->description = "Plot lines only" ;

	flag2 = G_define_flag() ;
	flag2->key         = '0' ;
	flag2->description = "Show zero elevations" ;

	if (G_parser(argc, argv))
			exit(1);

	do_zero = flag2->answer ;
	lines_only = flag1->answer ;

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

/*
	printf("\n3-d view request:\n") ;
	printf("File plotted:    %s in %s\n", file, file_mapset) ;
	printf("Elevation file:  %s in %s\n", elevfile, elevfile_mapset) ;
	printf("\n") ;
	printf("            View from:    View to:\n") ;
	printf("Easting:  %12.2lf %12.2lf\n", from_easting, to_easting) ;
	printf("Northing: %12.2lf %12.2lf\n", from_northing, to_northing) ;
	printf("Height:   %12.2lf %12.2lf\n", from_height, to_height) ;
	printf("\n") ;
	printf("line frequency:         %d\n", line_freq) ;
	printf("elevation exaggeration: %4.2lf\n", exag) ;
	printf("field of view:          %4.2lf\n", field) ;
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
		G_fatal_error("Inappropriate 3d request") ;

	if (0 > G_set_window(&window) )
		G_fatal_error("Inappropriate window resolution request") ;

/* Set up graphics */
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;

	establish_view(from_easting,from_northing,from_height,to_easting,to_northing,to_height,field) ;

	threed(0);

	R_close_driver() ;
}
