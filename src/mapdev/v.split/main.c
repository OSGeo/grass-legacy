/* ****************************************************************************
 *
 * MODULE:       v.split 
 * AUTHOR(S):    Radim Blazek
 * PURPOSE:      Split lines to segments    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#define MAIN
#include <stdio.h>
#include "gis.h"
#include "Vect.h"

int main(int argc, char **argv)
{
int    i, type, line, ltype, np;
char   *mapset;
struct Option  *in_opt, *out_opt, *type_opt;
struct Option  *lev_opt, *col_opt, *catas_opt;
struct GModule *module;
struct Map_info inmap, outmap;
struct line_pnts *Ipoints, *Opoints;
char   instruction [2000];

G_gisinit("Split lines");

module = G_define_module();
module->description = "Split lines to segments.";

in_opt = G_define_option();
in_opt->key = "input";
in_opt->description = "Input vector";
in_opt->type = TYPE_STRING;
in_opt->required = YES;

out_opt = G_define_option();
out_opt->key = "output";
out_opt->description = "Output vector";
out_opt->type = TYPE_STRING;
out_opt->required = YES;

type_opt = G_define_option();
type_opt->key = "type";
type_opt->description = "Type: line, boundary";
type_opt->type = TYPE_STRING;
type_opt->required = NO;
type_opt->multiple = YES;
type_opt->answer = "line";
type_opt->options = "line,boundary";

    /*  check args and set flags  */
    if(G_parser (argc, argv))
        exit (1);

    /* Show advertising */
    fprintf(stderr, "Split lines.\n") ;

    if ((mapset = G_find_file2 ("dig", in_opt->answer, "")) == NULL)
	G_fatal_error ("Could not find input vector %s\n", in_opt->answer);

    /* element types */
    i=0;
    type=0;
    while (type_opt->answers[i])
     {
       if ( *type_opt->answers[i] == 'l')  type |= LINE;
       else if ( *type_opt->answers[i] == 'b')  type |= AREA;
       i++;
     }
     
    if(Vect_open_old(&inmap, in_opt->answer, mapset) < 1 )
            G_fatal_error("Cannot open input."); 

    if (0 > Vect_open_new (&outmap, out_opt->answer))
    {
        Vect_close( &inmap );
	G_fatal_error ("Cannot open output");
    } 

    Ipoints = Vect_new_line_struct();
    Opoints = Vect_new_line_struct();    
    
    while (1)
    {
	ltype = Vect_read_next_line (&inmap, Ipoints);
	if ( ltype == -1 ) G_fatal_error ("error while reading file\n");
	if ( ltype == -2 ) break; /* EOF */
	
	np = Ipoints->n_points;

        if ( ltype & type ) { 	
	    for ( i = 0; i < np - 1; i++)
	    {
	        Vect_append_point ( Opoints,  Ipoints->x[i], Ipoints->y[i]);
	        Vect_append_point ( Opoints,  Ipoints->x[i+1], Ipoints->y[i+1]);
	        Vect_write_line (&outmap, (unsigned int) ltype, Opoints);
	        Vect_reset_line ( Opoints );
	    }
	} else { 
	        Vect_write_line (&outmap, (unsigned int) ltype, Ipoints);
	}
    }

    Vect_destroy_line_struct(Ipoints);
    Vect_destroy_line_struct(Opoints);    
    Vect_close( &inmap );
    Vect_close( &outmap );

    /* Copy categories and labels */
    if (G_find_file ("dig_att", in_opt->answer, mapset) != NULL)
    {
        fprintf (stderr, "Copying attributes.\n");
	G__make_mapset_element("dig_att");
        sprintf (instruction, "cp %s/%s/dig_att/%s %s/%s/dig_att/%s",
	         G_location_path (), mapset, in_opt->answer,
	         G_location_path (), G_mapset(), out_opt->answer);
	G_system (instruction);
    }
    if (G_find_file ("dig_cats", in_opt->answer, mapset) != NULL)
    {
        fprintf (stderr, "Copying categories.\n");
	G__make_mapset_element("dig_cats");
        sprintf (instruction, "cp %s/%s/dig_cats/%s %s/%s/dig_cats/%s",
	         G_location_path (), mapset, in_opt->answer,
	         G_location_path (), G_mapset(), out_opt->answer);
	G_system (instruction);
    }
    
    
    fprintf(stderr, "Done processing.\n");

    exit(0);
}

