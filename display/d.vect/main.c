/*
 *   d.vect
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "colors.h"
#include "plot.h"
#include "dbmi.h"
#include "local_proto.h"

int quiet = 1;

int 
main (int argc, char **argv)
{
	char *mapset ;
	int ret, level;
	int i, stat, type, area, display;
	int chcat = 0;
	int color, fcolor, r, g, b;
	int colornum = MAX_COLOR_NUM;
	int icon, size;
	char map_name[128] ;
	struct GModule *module;
	struct Option *map_opt, *color_opt, *fcolor_opt;
	struct Option *type_opt, *display_opt;
	struct Option *icon_opt, *size_opt;
	struct Option *where_opt;
	struct Option *field_opt, *cat_opt, *lfield_opt;
	struct Option *lcolor_opt, *bgcolor_opt, *bcolor_opt;
	struct Option *lsize_opt, *font_opt, *xref_opt, *yref_opt;
	struct Option *attrcol_opt;
	struct Flag   *_quiet, *id_flag;
	struct cat_list *Clist;
	int *cats, ncat;
	LATTR lattr;
        struct Map_info Map;
        struct field_info *fi;
        dbDriver *driver;
        dbHandle handle;
	struct Cell_head window;
	BOUND_BOX box;
	double overlap;
	
	module = G_define_module();
	module->description =
		"Displays GRASS vector data in the active frame on the "
		"graphics monitor.";

	map_opt = G_define_standard_option(G_OPT_V_MAP); 
	type_opt =  G_define_standard_option(G_OPT_V_TYPE);
	
	display_opt = G_define_option() ;
	display_opt->key        = "display" ;
	display_opt->type       = TYPE_STRING ;
	display_opt->required   = NO ;
	display_opt->multiple   = YES ;
	display_opt->answer     = "shape" ;
	display_opt->options    = "shape,cat,topo,dir,attr";
	display_opt->description= "Display" ;
	
	attrcol_opt = G_define_option() ;
	attrcol_opt->key        = "attrcol" ;
	attrcol_opt->type       = TYPE_STRING ;
	attrcol_opt->description= "Name of column to be displayed" ;
	
	icon_opt = G_define_option() ;
	icon_opt->key        = "icon" ;
	icon_opt->type       = TYPE_STRING ;
	icon_opt->required   = NO ;
	icon_opt->multiple   = NO ;
	icon_opt->answer     = "cross" ;
	icon_opt->options    = "cross,box";
	icon_opt->description= "Point and centroid symbol" ;
	
	size_opt = G_define_option() ;
	size_opt->key        = "size" ;
	size_opt->type       = TYPE_INTEGER ;
	size_opt->answer     = "8" ;
	size_opt->description= "Icon size" ;
	
	field_opt = G_define_standard_option(G_OPT_V_FIELD) ;
	cat_opt = G_define_standard_option(G_OPT_V_CATS) ;
	where_opt = G_define_standard_option(G_OPT_WHERE) ;
	
	color_opt = G_define_option() ;
	color_opt->key        = "color" ;
	color_opt->type       = TYPE_STRING ;
	color_opt->answer     = "white" ;
	color_opt->description= "Line color" ;

	fcolor_opt = G_define_option() ;
	fcolor_opt->key        = "fcolor" ;
	fcolor_opt->type       = TYPE_STRING ;
	fcolor_opt->answer     = "white" ;
	fcolor_opt->description= "Area fill color" ;

	lfield_opt = G_define_standard_option(G_OPT_V_FIELD) ;
	lfield_opt->key        = "lfield" ;
	lfield_opt->description= "Category field for labels" ;
	
	lcolor_opt = G_define_option() ;
	lcolor_opt->key        = "lcolor" ;
	lcolor_opt->type       = TYPE_STRING ;
	lcolor_opt->answer     = "white" ;
	lcolor_opt->description= "Label color" ;
	
	bgcolor_opt = G_define_option() ;
	bgcolor_opt->key        = "bgcolor" ;
	bgcolor_opt->type       = TYPE_STRING ;
	bgcolor_opt->answer     = "none" ;
	bgcolor_opt->description= "Label background color" ;

	bcolor_opt = G_define_option() ;
	bcolor_opt->key        = "bcolor" ;
	bcolor_opt->type       = TYPE_STRING ;
	bcolor_opt->answer     = "none" ;
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

	id_flag = G_define_flag ();
	id_flag->key		= 'i';
	id_flag->description	= "Use values from 'cat' option as line id";
	
	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);
	
	/* Read map options */
	strcpy(map_name, map_opt->answer);
	color = WHITE;
	if ( G_str_to_color(color_opt->answer, &r, &g, &b) ) {
	    colornum++;
	    R_reset_color (r, g, b, colornum); 
	    color = colornum;
	}
	fcolor = WHITE;
	if ( G_str_to_color(fcolor_opt->answer, &r, &g, &b) ) {
	    colornum++;
	    R_reset_color (r, g, b, colornum); 
	    fcolor = colornum;
	}
	quiet = !_quiet->answer;

	icon = G_ICON_CROSS;
	if ( strcmp (icon_opt->answer, "cross") == 0 )
	    icon = G_ICON_CROSS;
	else if ( strcmp (icon_opt->answer, "box") == 0 )
	    icon = G_ICON_BOX;

	size = atoi (size_opt->answer);

	/* Make sure map is available */
	mapset = G_find_vector2 (map_name, "") ; 
	
	if (mapset == NULL)
		G_fatal_error("Vector file [%s] not available", map_name) ;

	/* if where_opt was specified select categories from db 
	 * otherwise parse cat_opt */
        Clist = Vect_new_cat_list ();
	Clist->field = atoi (field_opt->answer);
	
	if (where_opt->answer)
	  {
            chcat = 1;  
            fi = Vect_get_field_info(map_name, mapset, Clist->field);
            if ( fi != NULL )
	      {
                driver = db_start_driver(fi->driver);
		if (driver == NULL)
		    G_fatal_error("Cannot open driver %s", fi->driver) ;
		
 	        db_init_handle (&handle);
	        db_set_handle (&handle, fi->database, NULL);
	        if (db_open_database(driver, &handle) != DB_OK)
		    G_fatal_error("Cannot open database %s", fi->database) ;
		
		ncat = db_select_int( driver, fi->table, fi->key, where_opt->answer, &cats);

		db_close_database(driver);
		db_shutdown_driver(driver);
			
	        Vect_array_to_cat_list ( cats, ncat, Clist);
	      }
	  }
        else
	  {
	    if (cat_opt->answer)
	      {
		chcat = 1;  
	        ret = Vect_str_to_cat_list ( cat_opt->answer, Clist);
	        if ( ret > 0 )
	            G_warning ( "%d errors in cat option\n", ret);
              }
	  }
	
	i = 0;
        type = 0; area = FALSE;
	while (type_opt->answers[i])
	  {
	    switch ( type_opt->answers[i][0] )
	      {
	        case 'p':
	            type |= GV_POINT;
		    break;
	        case 'l':
	            type |= GV_LINE;
		    break;
	        case 'b':
	            type |= GV_BOUNDARY;
		    break;
	        case 'c':
	            type |= GV_CENTROID;
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
	        case 't':
	            display |= DISP_TOPO;
		    break;
	        case 'd':
	            display |= DISP_DIR;
		    break;
	        case 'a':
	            display |= DISP_ATTR;
		    break;
	      }
	    i++;
	  }
	
	/* Read label options */
	lattr.field = atoi (lfield_opt->answer);
	lattr.color = WHITE;
	if ( G_str_to_color(lcolor_opt->answer, &r, &g, &b) ) {
	    colornum++;
	    R_reset_color (r, g, b, colornum); 
	    lattr.color = colornum;
	}
	lattr.bgcolor = 0;
	if ( G_str_to_color(bgcolor_opt->answer, &r, &g, &b) ) {
	    colornum++;
	    R_reset_color (r, g, b, colornum); 
	    lattr.bgcolor = colornum;
	}
	lattr.bcolor = 0;
	if ( G_str_to_color(bcolor_opt->answer, &r, &g, &b) ) {
	    colornum++;
	    R_reset_color (r, g, b, colornum); 
	    lattr.bcolor = colornum;
	}

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
	

        /* open vector */
        level = Vect_open_old (&Map, map_name, mapset);
        if ( level < 1 )
	    G_fatal_error ("Failed opening vector file"); 
	
	if (R_open_driver() != 0)
            G_fatal_error ("No graphics device selected");

	D_setup(0);

        G_setup_plot (D_get_d_north(), D_get_d_south(), 
                      D_get_d_west(), D_get_d_east(),
                      D_move_abs, D_cont_abs);

	if (!quiet)
	     fprintf (stdout,"Plotting ... "); fflush (stdout);

	G_get_set_window (&window);
	Vect_get_map_box ( &Map, &box );
	overlap =  G_window_percentage_overlap(&window, box.N, box.S, box.E, box.W);
	G_debug ( 1, "overlap = %f \n", overlap );
	if ( overlap < 1 ) 
	    Vect_set_constraint_region (&Map, window.north, window.south, 
	        window.east, window.west, PORT_DOUBLE_MAX, -PORT_DOUBLE_MAX);
         
	if ( area ) {
	    if ( level >= 2 )
	        stat = darea ( &Map, Clist, color, fcolor, chcat, (int) id_flag->answer );
	    else
		G_warning ("Cannot display areas, topology not available");
        }

	if ( display & DISP_SHAPE ) {
	    if ( id_flag->answer && level < 2 ) {
		G_warning ("Cannot display lines by id, topology not available");
	    } else {
	        stat = plot1 ( &Map, type, area, Clist, color, fcolor, chcat, icon, size, (int) id_flag->answer );
	    }
	}

        R_color(color);
	if ( display & DISP_DIR )
	    stat = dir ( &Map, type, Clist, chcat );
	
	if ( display & DISP_CAT )
	    stat = label ( &Map, type, area, Clist, &lattr, chcat);
	
	if ( display & DISP_ATTR )
	    stat = attr ( &Map, type, attrcol_opt->answer, Clist, &lattr, chcat);
	
	if ( display & DISP_TOPO ) {
	    if (level >= 2 )
	        stat = topo ( &Map, type, area, &lattr);
	    else
		G_warning ("Cannot display topology, not available");
	}
	
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


