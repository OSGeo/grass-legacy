#include "gis.h"
#include "site.h"
#include "raster.h"
#include "display.h"

#define GLOBAL
#include "options.h"
#include "local_proto.h"

/*
	[DCA]#.RANGE 
	where RANGE is one of #-#, ge#, le#, eq#
*/

int main( int argc , char **argv )
{
	char *mapset;
	char msg[200];
	char window_name[64] ;
	int i, num;
	int t, b, l, r ;
	struct Cell_head window ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4, *opt5, *opt6;
	struct Flag *do_num;

	module = G_define_module();
	module->description =
		"Displays a subset of a sites list based on "
		"site attributes.";

	opt4 = G_define_option() ;
	opt4->key        = "sitefile";
	opt4->type       = TYPE_STRING;
	opt4->required   = YES;
	opt4->gisprompt  = "old,site_lists,sites";
	opt4->description= "Name of a site file" ;

	opt1 = G_define_option() ;
	opt1->key        = "color" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO ;
	opt1->answer     = "gray" ;
	opt1->options    = D_color_list();
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
	opt3->answer     = "x" ;
	opt3->options    = "x,diamond,box,+" ;
	opt3->description= "Specify the type of the icon" ;

	opt5 = G_define_option() ;
	opt5->key        = "rules" ;
	opt5->type       = TYPE_STRING ;
	opt5->required   = NO ;
	opt5->multiple   = YES ;
	opt5->description= "qualifying rules for display: [DCA]#.RANGE\n\t\twhere RANGE is one of #-#, ge#, le#, eq#";

	opt6 = G_define_option() ;
	opt6->key        = "output";
	opt6->type       = TYPE_STRING;
	opt6->required   = NO;
	opt6->gisprompt  = "new,site_lists,sites";
	opt6->description= "Name of a site file to output drawn sites" ;

	do_num = G_define_flag ();
	do_num->key = 'n';
	do_num->description = "Output number of sites displayed";


	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */

	if (G_parser(argc, argv))
		exit(-1);

	infile = outfile = NULL;

	if(opt5->answers)
	    for (i=0; opt5->answers[i]; i++)
		if( -1 == parse_set_rules(opt5->answers[i])){
		    sprintf (msg, "Bad rule: %s", opt5->answers[i]);
		    G_fatal_error(msg);
		}

	if(opt6->answer){
	    outfile = G_sites_open_new (opt6->answer);
	    if (outfile == NULL)
	    {
		    sprintf (msg, "can't open sites file [%s]", opt6->answer);
		    G_fatal_error (msg);
	    }
	}

	color = D_translate_color(opt1->answer) ;
	if (color == 0)
	{
		fprintf (stdout,"Don't know the color %s\n", opt1->answer);
		G_usage() ;
		exit(-1);
	}

	mapset = G_find_file ("site_lists", opt4->answer, "");
	if (mapset == NULL)
	{
		sprintf (msg, "sites file [%s] not found", opt4->answer);
		G_fatal_error (msg);
	}

	infile = G_sites_open_old (opt4->answer, mapset);
	if (infile == NULL)
	{
		sprintf (msg, "can't open sites file [%s]", opt4->answer);
		G_fatal_error (msg);
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

	/* Setup driver and check important information */
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current frame") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current frame not available") ;

	/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting map region") ;

	if (G_set_window(&window) == -1)
		G_fatal_error("Current frame not settable") ;

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen frame") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

	if(outfile){ /* also write G_recreate_command & wind as comment */
	Site_head sh;
	char buf[80];

            fprintf(outfile,"# %s\n", G_recreate_command());

            fprintf(outfile,"# Current window: \n");

	    G_format_northing(window.north, buf, window.proj);
            fprintf(outfile,"# \t\t%s\n", buf);

	    G_format_easting(window.west, buf, window.proj);
            fprintf(outfile,"# %s", buf);

	    G_format_easting(window.east, buf, window.proj);
            fprintf(outfile,"\t\t%s\n", buf);

	    G_format_northing(window.south, buf, window.proj);
            fprintf(outfile,"# \t\t%s\n", buf);

	    G_site_get_head (infile, &sh);
	    G_site_put_head (outfile, &sh);
	}

	/* Do the plotting */
	R_standard_color (color) ;
	switch(type)
	{
	case TYPE_X:
		num = draw_points_x(&window) ;
		break ;
	case TYPE_PLUS:
		num = draw_points_plus(&window) ;
		break ;
	case TYPE_BOX:
		num = draw_points_box(&window) ;
		break ;
	case TYPE_DIAMOND:
		num = draw_points_diamond(&window) ;
		break ;
	}

	D_add_to_list(G_recreate_command()) ;
	R_close_driver();
        fclose(infile);
        if (outfile) fclose(outfile);

	if (do_num->answer) fprintf(stdout,"%d\n", num);

	return(0);
}
