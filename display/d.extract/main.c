/****************************************************************
 *
 * MODULE:       d.vect.extract
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      a graphical vector extractor
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 * TODO: check if vector line really intersects selection box,
 *       and not only vector outline box intersects selection box:
 *
 *    +-------------+
 *    |   ..../^^^^^|
 *    |../          |
 *    +-------------+ vector outline box
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "Vect.h"
#include "dbmi.h"

int extract (struct Map_info *, struct Map_info *, int, int, int);

int main(int argc, char **argv)
{
    struct Option *input, *output, *type_opt;
    struct Option *color_opt, *hcolor_opt;
    struct GModule *module;
    char   *mapset;
    struct Map_info In, Out;
    int    type, color, hcolor;
    int    r, g, b, colornum = MAX_COLOR_NUM;
    char **vect = NULL;
    int nvects = 0;
    
            
    /* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    module = G_define_module();
    module->description = 
    "Select and extract vectors with mouse into new vector map";

    /* Conditionalize R_open_driver() so "help" works, open quiet as well */
    R__open_quiet();
    if (R_open_driver() != 0) {
	G_fatal_error ("No graphics device selected");
    }
    if(D_get_dig_list (&vect, &nvects) < 0)
         G_fatal_error ("No vector map shown in monitor, use d.vect first");

    input = G_define_standard_option(G_OPT_V_INPUT);
    output = G_define_standard_option(G_OPT_V_OUTPUT);

    type_opt =  G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options    = "point,line,boundary,centroid,area,face";
    type_opt->answer     = "point,line,boundary,centroid,area,face";

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
    
    if(G_parser(argc,argv))
        exit(-1);

    type = Vect_option_to_types ( type_opt ); 

    color = atoi(color_opt->answer);
    if ( G_str_to_color(color_opt->answer, &r, &g, &b) ) {
        colornum++;
	R_reset_color (r, g, b, colornum); 
	color = colornum;
    }

    hcolor = atoi(hcolor_opt->answer);
    if ( G_str_to_color(hcolor_opt->answer, &r, &g, &b) ) {
        colornum++;
	R_reset_color (r, g, b, colornum); 
	hcolor = colornum;
    }
    
    mapset = G_find_vector2 (input->answer, NULL); 
      
    if ( mapset == NULL) 
      G_fatal_error ("Could not find input %s\n", input->answer);

    Vect_set_open_level(2);
    Vect_open_old (&In, input->answer, mapset); 
    Vect_open_new (&Out, output->answer, Vect_is_3d(&In)); 


    D_setup(0);

    G_setup_plot (D_get_d_north(), D_get_d_south(),
		D_get_d_west(), D_get_d_east(),
		D_move_abs, D_cont_abs);

    extract ( &In, &Out, type, color, hcolor ); 

    R_close_driver();

    Vect_copy_tables ( &In, &Out, 0 );
    Vect_build (&Out, stdout );
    Vect_close(&In);
    Vect_close(&Out);

    exit(0);
}
