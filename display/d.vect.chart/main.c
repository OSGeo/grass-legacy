/* ***************************************************************
 * *
 * * MODULE:       d.vect.chart
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Dispaly charts
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "colors.h"
#include "symbol.h"
#include "dbmi.h"
#include "global.h"


int 
main (int argc, char **argv)
{
    char *mapset, *p ;
    int i, j, ret, type, field, ctype, ncols;
    COLOR ocolor, *colors;
    int r, g, b;
    int size;
    double scale;
    struct GModule *module;
    struct Option *map_opt;
    struct Option *type_opt, *ctype_opt;
    struct Option *size_opt, *scale_opt;
    struct Option *field_opt;
    struct Option *ocolor_opt, *colors_opt;
    struct Option *columns_opt, *sizecol_opt;
    struct Map_info Map;
    COLOR defcols[] = { { 0,   0,   0, 255 }, /* blue */
                        { 0,   0, 255, 255 }, /* cyan */ 	
                        { 0,   0, 255,   0 }, /* green */ 	
                        { 0, 255, 255,   0 }, /* yellow */ 	
                        { 0, 255,   0,   0 }, /* red */ 	
                        { -1,  0,   0,   0 } /* END */ 	
		      };
    
    module = G_define_module();
    module->description = "Displays charts of GRASS vector data in the active frame on the "
	                  "graphics monitor.";

    map_opt = G_define_standard_option(G_OPT_V_MAP); 

    type_opt =  G_define_standard_option(G_OPT_V_TYPE);
    type_opt->answer     = "point,line,boundary,centroid";
    
    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;

    ctype_opt = G_define_option() ;
    ctype_opt->key        = "ctype" ;
    ctype_opt->type       = TYPE_STRING ;
    ctype_opt->required   = NO ;
    ctype_opt->multiple   = NO ;
    ctype_opt->answer     = "pie" ;
    ctype_opt->options    = "pie,bar";
    ctype_opt->description= "Chart type" ;
    
    columns_opt = G_define_option() ;
    columns_opt->key        = "columns" ;
    columns_opt->type       = TYPE_STRING ;
    columns_opt->required   = YES ;
    columns_opt->multiple   = YES ;
    columns_opt->description= "Attribute columns containing data" ;
    
    sizecol_opt = G_define_option() ;
    sizecol_opt->key        = "sizecol" ;
    sizecol_opt->type       = TYPE_STRING ;
    sizecol_opt->required   = NO ;
    sizecol_opt->description= "Column used for pie chart size" ;

    size_opt = G_define_option() ;
    size_opt->key        = "size" ;
    size_opt->type       = TYPE_INTEGER ;
    size_opt->answer     = "40" ;
    size_opt->description= "Size of chart (diameter for pie, total width for bar)" ;

    scale_opt = G_define_option() ;
    scale_opt->key        = "scale" ;
    scale_opt->type       = TYPE_DOUBLE ;
    scale_opt->answer     = "1" ;
    scale_opt->description= "Scale for size (to get size in pixels)" ;
    
    ocolor_opt = G_define_option() ;
    ocolor_opt->key        = "ocolor" ;
    ocolor_opt->type       = TYPE_STRING ;
    ocolor_opt->answer     = DEFAULT_FG_COLOR ;
    ocolor_opt->description= "Outline color" ;

    colors_opt = G_define_option() ;
    colors_opt->key        = "colors" ;
    colors_opt->type       = TYPE_STRING ;
    colors_opt->required   = NO ;
    colors_opt->multiple   = YES ;
    colors_opt->description= "Colors used to fill charts" ;

    G_gisinit(argv[0]) ;

    if (G_parser(argc, argv)) exit(-1);
    
    /* Read options */
    type = Vect_option_to_types ( type_opt );
    field = atoi (field_opt->answer);

    /* Outline color */
    ret =  G_str_to_color(ocolor_opt->answer, &r, &g, &b);
    if ( ret == 1 ) {
        ocolor.none = 0;
        ocolor.r  = r; ocolor.g  = g; ocolor.b  = b;
    } else if ( ret == 2 ) { /* none */
        ocolor.none = 1;
    }

    /* Count input columns */
    p = columns_opt->answer;
    ncols = 1;
    while ( ( p = strchr (  p, ',' ) ) != NULL ) { 
	ncols++; p++;
    }
    G_debug ( 3, "ncols = %d", ncols );
    
    /* Fill colors */
    colors = (COLOR *) G_malloc ( ncols * sizeof ( COLOR ) );
    /* default colors */
    j = 0;
    for ( i = 0; i < ncols; i++ ) {
        if ( defcols[j].none == -1) j = 0; 
	colors[i].none = 0;
	colors[i].r = defcols[j].r;
	colors[i].g = defcols[j].g;
	colors[i].b = defcols[j].b;
	j++;
    }
    /* user colors */
    if ( colors_opt->answers != NULL ) {
	for ( i = 0; i < ncols; i++ ) {
	    if ( colors_opt->answers[i] == NULL ) break;

	    ret =  G_str_to_color(colors_opt->answers[i], &r, &g, &b);
	    if ( ret == 1 ) {
		colors[i].none = 0;
		colors[i].r = r;
		colors[i].g = g;
		colors[i].b = b;
	    } else if ( ret == 2 ) { /* none */
		colors[i].none = 1;
	    }
	}
    }
    
    size = atoi (size_opt->answer);
    scale = atof (scale_opt->answer);

    /* Make sure map is available */
    mapset = G_find_vector2 (map_opt->answer, NULL) ; 
    if (mapset == NULL) G_fatal_error("Vector file [%s] not available", map_opt->answer) ;

    /* open vector */
    Vect_set_open_level (2);
    Vect_open_old (&Map, map_opt->answer, mapset);
    
    
    ctype = CTYPE_PIE;
    if ( ctype_opt->answer[0] == 'b' ) ctype = CTYPE_BAR;
    
    if (R_open_driver() != 0) G_fatal_error ("No graphics device selected");

    D_setup(0);

    G_setup_plot (D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
		  D_move_abs, D_cont_abs);

    ret = plot( ctype, &Map, type, field, 
	         columns_opt->answer, ncols, 
		 sizecol_opt->answer, size, scale, 
		 &ocolor, colors );

    if(ret == 0) {
	D_add_to_list(G_recreate_command()) ;
        D_set_dig_name(G_fully_qualified_name(map_opt->answer, mapset));
        D_add_to_dig_list(G_fully_qualified_name(map_opt->answer, mapset));
    }

    R_close_driver();

    Vect_close (&Map);

    exit(ret);
}

