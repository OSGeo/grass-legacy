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
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "Vect.h"
#include "dbmi.h"
#include "proto.h"

int main(int argc, char **argv)
{
    struct Option *map, *afield_opt, *nfield_opt, *afcol, *abcol, *ncol, *type_opt;
    struct Option *color_opt, *hcolor_opt, *bgcolor_opt;
    struct Flag   *geo_f;
    struct GModule *module;
    char   *mapset;
    struct Map_info Map;
    int    type, afield, nfield, color, hcolor, bgcolor, geo;
    int    r, g, b, colornum = MAX_COLOR_NUM;
    char **vect = NULL;
    int nvects = 0;
    char *fakestart;        

    /* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    module = G_define_module();
    module->description = 
    "Find shortest path for selected starting and ending node";

    fakestart = getenv( "GRASS_FAKE_START" );
    if ( fakestart == NULL ) {
       /* Conditionalize R_open_driver() so "help" works, open quiet as well */
       R__open_quiet();
       if (R_open_driver() != 0) {
          G_fatal_error ("No graphics device selected");
       }
       if (D_get_dig_list (&vect, &nvects) < 0)
          G_fatal_error ("No vector map shown in monitor, use d.vect first");
       else
       {
          vect = (char **)G_realloc(vect, (nvects+1)*sizeof(char *));
          vect[nvects] = NULL;
       }
       R_close_driver();
    }
    map = G_define_standard_option(G_OPT_V_MAP);

    type_opt =  G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options    = "line,boundary";
    type_opt->answer     = "line,boundary";
    type_opt->description = "Arc type";

    afield_opt = G_define_standard_option(G_OPT_V_FIELD);
    afield_opt->key = "alayer";
    afield_opt->answer = "1";
    afield_opt->description = "Arc layer";
    
    nfield_opt = G_define_standard_option(G_OPT_V_FIELD);
    nfield_opt->key = "nlayer";
    nfield_opt->answer = "2";
    nfield_opt->description = "Node layer";

    afcol = G_define_option() ;
    afcol->key         = "afcol" ;
    afcol->type        = TYPE_STRING ;
    afcol->required    = NO ; 
    afcol->description = "Arc forward/both direction(s) cost column" ;
    
    abcol = G_define_option() ;
    abcol->key         = "abcol" ;
    abcol->type        = TYPE_STRING ;
    abcol->required    = NO ; 
    abcol->description = "Arc backward direction cost column" ;
    
    ncol = G_define_option() ;
    ncol->key         = "ncol" ;
    ncol->type        = TYPE_STRING ;
    ncol->required    = NO ; 
    ncol->description = "Node cost column" ;
    
    color_opt = G_define_option() ;
    color_opt->key        = "color" ;
    color_opt->type       = TYPE_STRING ;
    color_opt->answer     = "black" ;
    color_opt->description= "Original line color" ;
	
    hcolor_opt = G_define_option() ;
    hcolor_opt->key        = "hcolor" ;
    hcolor_opt->type       = TYPE_STRING ;
    hcolor_opt->answer     = "red" ;
    hcolor_opt->description= "Highlight color" ;
    
    bgcolor_opt = G_define_option() ;
    bgcolor_opt->key        = "bgcolor" ;
    bgcolor_opt->type       = TYPE_STRING ;
    bgcolor_opt->answer     = "white" ;
    bgcolor_opt->description= "Background color" ;

    geo_f = G_define_flag ();
    geo_f->key             = 'g';
    geo_f->description     = "Use geodesic calculation for longitude-latitude locations";
    
    if(G_parser(argc,argv))
        exit(-1);

    type = Vect_option_to_types ( type_opt ); 
    afield = atoi (afield_opt->answer);
    nfield = atoi (nfield_opt->answer);

    color = BLACK;
    if ( G_str_to_color(color_opt->answer, &r, &g, &b) ) {
        colornum++;
	R_reset_color (r, g, b, colornum); 
	color = colornum;
    }

    hcolor = RED;
    if ( G_str_to_color(hcolor_opt->answer, &r, &g, &b) ) {
        colornum++;
	R_reset_color (r, g, b, colornum); 
	hcolor = colornum;
    }
    
    bgcolor = WHITE;
    if ( G_str_to_color(bgcolor_opt->answer, &r, &g, &b) ) {
        colornum++;
	R_reset_color (r, g, b, colornum); 
	bgcolor = colornum;
    }

    if ( geo_f->answer ) 
    {
       geo = 1; 
       if (G_projection () != PROJECTION_LL)
          G_fatal_error("The current projection is not longitude-latitude");
    }
    else geo = 0;
    
    mapset = G_find_vector2 (map->answer, NULL); 
      
    if ( mapset == NULL) 
      G_fatal_error ("Could not find input %s\n", map->answer);

    Vect_set_open_level(2);
    Vect_open_old (&Map, map->answer, mapset); 

    if (R_open_driver() != 0) {
    G_fatal_error ("No graphics device selected");
    Vect_close(&Map);
    }
    D_setup(0);

    G_setup_plot (D_get_d_north(), D_get_d_south(),
		D_get_d_west(), D_get_d_east(),
		D_move_abs, D_cont_abs);

    Vect_net_build_graph ( &Map, type , afield, nfield, afcol->answer, abcol->answer, ncol->answer, 
	                   geo, 0 );
    path ( &Map, color, hcolor, bgcolor ); 


    R_close_driver();

    Vect_close(&Map);

    exit(0);
}



