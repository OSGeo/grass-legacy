/****************************************************************
 *
 * MODULE:       d.path
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      shortest path networking on vector map
 *               Uses the DGLib from Roberto Micarelli
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/display.h>
#include <grass/colors.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>
#include "proto.h"

int main(int argc, char **argv)
{
    struct Option *map, *afield_opt, *nfield_opt, *afcol, *abcol, *ncol, *type_opt;
    struct Option *color_opt, *hcolor_opt, *bgcolor_opt;
    struct Flag   *geo_f;
    struct GModule *module;
    char   *mapset;
    struct Map_info Map;
    int    type, afield, nfield, geo;
    struct color_rgb color, hcolor, bgcolor;
    int    r, g, b;


    /* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    module = G_define_module();
    module->keywords = _("display, networking");
    module->description = 
	_("Find shortest path for selected starting and ending node");

    map = G_define_standard_option(G_OPT_V_MAP);

    type_opt =  G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options    = "line,boundary";
    type_opt->answer     = "line,boundary";
    type_opt->description = _("Arc type");

    afield_opt = G_define_standard_option(G_OPT_V_FIELD);
    afield_opt->key = "alayer";
    afield_opt->answer = "1";
    afield_opt->description = _("Arc layer");
    
    nfield_opt = G_define_standard_option(G_OPT_V_FIELD);
    nfield_opt->key = "nlayer";
    nfield_opt->answer = "2";
    nfield_opt->description = _("Node layer");

    afcol = G_define_option() ;
    afcol->key         = "afcol" ;
    afcol->type        = TYPE_STRING ;
    afcol->required    = NO ; 
    afcol->description = _("Arc forward/both direction(s) cost column");
    
    abcol = G_define_option() ;
    abcol->key         = "abcol" ;
    abcol->type        = TYPE_STRING ;
    abcol->required    = NO ; 
    abcol->description = _("Arc backward direction cost column");
    
    ncol = G_define_option() ;
    ncol->key         = "ncol" ;
    ncol->type        = TYPE_STRING ;
    ncol->required    = NO ; 
    ncol->description = _("Node cost column");
    
    color_opt = G_define_option() ;
    color_opt->key        = "color" ;
    color_opt->type       = TYPE_STRING ;
    color_opt->answer     = DEFAULT_FG_COLOR ;
    color_opt->description= _("Original line color");
    color_opt->gisprompt  = GISPROMPT_COLOR ;
	
    hcolor_opt = G_define_option() ;
    hcolor_opt->key        = "hcolor" ;
    hcolor_opt->type       = TYPE_STRING ;
    hcolor_opt->answer     = "red" ;
    hcolor_opt->description= _("Highlight color");
    hcolor_opt->gisprompt  = GISPROMPT_COLOR ;
    
    bgcolor_opt = G_define_option() ;
    bgcolor_opt->key        = "bgcolor" ;
    bgcolor_opt->type       = TYPE_STRING ;
    bgcolor_opt->answer     = DEFAULT_BG_COLOR ;
    bgcolor_opt->description= _("Background color");
    bgcolor_opt->gisprompt  = GISPROMPT_COLOR ;

    geo_f = G_define_flag ();
    geo_f->key             = 'g';
    geo_f->description     =
	_("Use geodesic calculation for longitude-latitude locations");
    
    if(G_parser(argc,argv))
        exit(EXIT_FAILURE);

    type = Vect_option_to_types ( type_opt ); 
    afield = atoi (afield_opt->answer);
    nfield = atoi (nfield_opt->answer);

    if (R_open_driver() != 0)
       G_fatal_error (_("No graphics device selected"));

    color = G_standard_color_rgb(BLACK);
    if ( G_str_to_color(color_opt->answer, &r, &g, &b) ) {
	color.r = r;
	color.g = g;
	color.b = b;
    }

    hcolor = G_standard_color_rgb(RED);
    if ( G_str_to_color(hcolor_opt->answer, &r, &g, &b) ) {
	hcolor.r = r;
	hcolor.g = g;
	hcolor.b = b;
    }
    
    bgcolor = G_standard_color_rgb(WHITE);
    if ( G_str_to_color(bgcolor_opt->answer, &r, &g, &b) ) {
	bgcolor.r = r;
	bgcolor.g = g;
	bgcolor.b = b;
    }

    if ( geo_f->answer ) 
    {
       geo = 1; 
       if (G_projection () != PROJECTION_LL)
          G_fatal_error(_("The current projection is not longitude-latitude"));
    }
    else geo = 0;

    mapset = G_find_vector2 (map->answer, NULL); 

    if ( mapset == NULL) 
      G_fatal_error (_("Could not find input map <%s>"), map->answer);

    Vect_set_open_level(2);
    Vect_open_old (&Map, map->answer, mapset); 

    D_setup(0);

    G_setup_plot (D_get_d_north(), D_get_d_south(),
		D_get_d_west(), D_get_d_east(),
		D_move_abs, D_cont_abs);

    Vect_net_build_graph ( &Map, type , afield, nfield, afcol->answer,
			   abcol->answer, ncol->answer, geo, 0 );

    path ( &Map, &color, &hcolor, &bgcolor ); 


    R_close_driver();

    Vect_close(&Map);

    exit(EXIT_SUCCESS);
}



