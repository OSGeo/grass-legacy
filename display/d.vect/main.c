/*
 *   d.vect
 *
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "plot.h"
#include "local_proto.h"

int quiet = 1;

int 
main (int argc, char **argv)
{
	char *mapset ;
	char buf[128] ;
	int ret;
	int i, stat, type, area, display;
	int color, fcolor;
	char ncolist[200];
	char map_name[128] ;
	struct GModule *module;
	struct Option *map_opt, *color_opt, *fcolor_opt;
	struct Option *type_opt, *display_opt;
	struct Option *field_opt, *cat_opt, *lfield_opt;
	struct Option *lcolor_opt, *bgcolor_opt, *bcolor_opt;
	struct Option *lsize_opt, *font_opt, *xref_opt, *yref_opt;
	struct Flag   *_quiet;
	struct line_pnts *Points;
	struct cat_list *Clist;
	LATTR lattr;
        struct Map_info Map;
	
        sprintf (ncolist, "none,%s", D_color_list());
	
	module = G_define_module();
	module->description =
		"Displays GRASS vector data in the active frame on the "
		"graphics monitor.";

	map_opt = G_define_option() ;
	map_opt->key        = "map" ;
	map_opt->type       = TYPE_STRING ;
	map_opt->required   = YES ;
	map_opt->multiple   = NO ;
	map_opt->gisprompt  = "old,dig,vector" ;
	map_opt->description= "Name of existing vector map to be displayed" ;

	type_opt = G_define_option() ;
	type_opt->key        = "type" ;
	type_opt->type       = TYPE_STRING ;
	type_opt->required   = NO ;
	type_opt->multiple   = YES ;
	type_opt->answer     = "point,line,boundary,centroid" ;
	type_opt->options    = "point,line,boundary,centroid,area";
	type_opt->description= "Select type: point, line, boundary, centroid or area" ;
	
	display_opt = G_define_option() ;
	display_opt->key        = "display" ;
	display_opt->type       = TYPE_STRING ;
	display_opt->required   = NO ;
	display_opt->multiple   = YES ;
	display_opt->answer     = "shape" ;
	display_opt->options    = "shape,cat";
	display_opt->description= "Select shape or cat(egory)" ;
	
	field_opt = G_define_option() ;
	field_opt->key        = "field" ;
	field_opt->type       = TYPE_INTEGER ;
	field_opt->answer     = "1" ;
	field_opt->description= "Category field" ;
	
	cat_opt = G_define_option() ;
	cat_opt->key        = "cat" ;
	cat_opt->type       = TYPE_STRING ;
	cat_opt->description= "Categories to be displayed. "
                              "Example: cat=3,7,15-21,45" ;
	
	color_opt = G_define_option() ;
	color_opt->key        = "color" ;
	color_opt->type       = TYPE_STRING ;
	color_opt->answer     = "white" ;
	color_opt->options    = D_color_list();
	color_opt->description= "Line color" ;

	fcolor_opt = G_define_option() ;
	fcolor_opt->key        = "fcolor" ;
	fcolor_opt->type       = TYPE_STRING ;
	fcolor_opt->answer     = "white" ;
	fcolor_opt->options    = D_color_list();
	fcolor_opt->description= "Area fill color" ;

	lfield_opt = G_define_option() ;
	lfield_opt->key        = "lfield" ;
	lfield_opt->type       = TYPE_INTEGER ;
	lfield_opt->answer     = "1" ;
	lfield_opt->description= "Category label field" ;
	
	lcolor_opt = G_define_option() ;
	lcolor_opt->key        = "lcolor" ;
	lcolor_opt->type       = TYPE_STRING ;
	lcolor_opt->answer     = "white" ;
	lcolor_opt->options    = D_color_list();
	lcolor_opt->description= "Label color" ;
	
	bgcolor_opt = G_define_option() ;
	bgcolor_opt->key        = "bgcolor" ;
	bgcolor_opt->type       = TYPE_STRING ;
	bgcolor_opt->answer     = "none" ;
	bgcolor_opt->options    = ncolist;
	bgcolor_opt->description= "Label background color" ;

	bcolor_opt = G_define_option() ;
	bcolor_opt->key        = "bcolor" ;
	bcolor_opt->type       = TYPE_STRING ;
	bcolor_opt->answer     = "none" ;
	bcolor_opt->options    = ncolist;
	bcolor_opt->description= "Label border color" ;

	lsize_opt = G_define_option() ;
	lsize_opt->key        = "lsize" ;
	lsize_opt->type       = TYPE_INTEGER ;
	lsize_opt->answer     = "8" ;
	lsize_opt->description= "Label size (pixels)" ;

	font_opt = G_define_option() ;
	font_opt->key        = "font" ;
	font_opt->type       = TYPE_STRING ;
	font_opt->answer     = "romans" ;
	font_opt->options    = "cyrilc,gothgbt,gothgrt,gothitt,"
	    "greekc,greekcs,greekp,greeks,italicc,italiccs,italict,"
	    "romanc,romancs,romand,romans,romant,scriptc,scripts" ;
	font_opt->description= "Font name" ;

	xref_opt = G_define_option() ;
	xref_opt->key        = "xref" ;
	xref_opt->type       = TYPE_STRING ;
	xref_opt->answer     = "left" ;
	xref_opt->options    = "left,center,right";
	xref_opt->description= "Label horizontal justification" ;

	yref_opt = G_define_option() ;
	yref_opt->key        = "yref" ;
	yref_opt->type       = TYPE_STRING ;
	yref_opt->answer     = "center" ;
	yref_opt->options    = "top,center,bottom";
	yref_opt->description= "Label vertical justification" ;
	
	_quiet = G_define_flag ();
	_quiet->key		= 'v';
	_quiet->description	= "Run verbosely";

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	/* Read map options */
	strcpy(map_name, map_opt->answer);
	color = D_translate_color(color_opt->answer);
	quiet = !_quiet->answer;

        Clist = Vect_new_cat_list ();
	Clist->field = atoi (field_opt->answer);
	if (cat_opt->answer)
	  {
	    ret = Vect_str_to_cat_list ( cat_opt->answer, Clist);
	    if ( ret > 0 )
	        G_warning ( "%d errors in cat option\n", ret);
          }
	
	i = 0;
        type = 0; area = FALSE;
	while (type_opt->answers[i])
	  {
	    switch ( type_opt->answers[i][0] )
	      {
	        case 'p':
	            type |= DOT;
		    break;
	        case 'l':
	            type |= LINE;
		    break;
	        case 'b':
	            type |= BOUNDARY;
		    break;
	        case 'c':
	            type |= CENTROID;
		    break;
	        case 'a':
	            area = TRUE;
		    break;
	      }
	    i++;
	  }

	i = 0;
        display = 0;
	while (display_opt->answers[i])
	  {
	    switch ( display_opt->answers[i][0] )
	      {
	        case 's':
	            display |= DISP_SHAPE;
		    break;
	        case 'c':
	            display |= DISP_CAT;
		    break;
	      }
	    i++;
	  }
	
	/* Read label options */
	lattr.field = atoi (lfield_opt->answer);
	lattr.color = D_translate_color(lcolor_opt->answer);
	lattr.bgcolor = D_translate_color(bgcolor_opt->answer);
	lattr.bcolor = D_translate_color(bcolor_opt->answer);
	lattr.size = atoi(lsize_opt->answer);
	lattr.font = font_opt->answer;
	switch ( xref_opt->answer[0] )
	  {
	    case 'l':
	        lattr.xref = LLEFT;
		break;
	    case 'c':
	        lattr.xref = LCENTER;
		break;
	    case 'r':
	        lattr.xref = LRIGHT;
		break;
	  }
	switch ( yref_opt->answer[0] )
	  {
	    case 't':
	        lattr.yref = LTOP;
		break;
	    case 'c':
	        lattr.yref = LCENTER;
		break;
	    case 'b':
	        lattr.yref = LBOTTOM;
		break;
	  }
	
	/* Make sure map is available */
	mapset = G_find_vector2 (map_name, "") ; 
	
	if (mapset == NULL)
	{
		sprintf(buf,"Vector file [%s] not available", map_name);
		G_fatal_error(buf) ;
		exit(-1);
	}

        /* open vector */
        Vect_set_open_level (1);
        if (1 > Vect_open_old (&Map, map_name, mapset))
	    G_fatal_error ("Failed opening vector file"); 
	
	
	R_open_driver();

	D_setup(0);

        G_setup_plot (D_get_d_north(), D_get_d_south(), 
                      D_get_d_west(), D_get_d_east(),
                      D_move_abs, D_cont_abs);

	if (!quiet)
	     fprintf (stdout,"Plotting ... "); fflush (stdout);

	
	/* look att this later
	if (stat = plot2 (map_name, mapset, Points))
	{
		stat = plot1 (map_name, mapset, &lattr);
	}
        */

	if ( display & DISP_SHAPE )
	    stat = plot1 ( &Map, type, area, Clist, color, fcolor);

	if ( display & DISP_CAT )
	    stat = label ( &Map, type, area, Clist, &lattr);
	
	if(stat == 0)
	    D_add_to_list(G_recreate_command()) ;

	D_set_dig_name(G_fully_qualified_name(map_name, mapset));
	D_add_to_dig_list(G_fully_qualified_name(map_name, mapset));

	R_close_driver();

        if (!quiet)
	    fprintf (stdout,"Done\n");
	
	Vect_close (&Map);
	Vect_destroy_cat_list (Clist);

	exit(stat);
}


