/***************************************************************
 *
 * MODULE:       v.net
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Network maintenance 
 *               
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "proto.h"

int report ( char *in, int afield, int nfield)
{
    struct Map_info In;
    int    i, j, k, line, ltype, nnodes;
    char   *mapset;
    int    cat_line, cat_node[2];
    struct line_cats *Cats;
    int    node;
    double x, y, z;

    Cats = Vect_new_cats_struct ();
    
    mapset = G_find_vector2 (in, "");
    if (mapset == NULL) 
        G_fatal_error( "Vector file [%s] not available", in );

    Vect_set_open_level (2);
    if (2 > Vect_open_old (&In, in, mapset))
	G_fatal_error ("Topology is not available");

    /* For all lines find categories for points on nodes */ 
    for (i = 1 ; i <= Vect_get_num_lines (&In) ; i++)
      {
	ltype = Vect_read_line (&In, NULL, Cats, i);
	if ( ltype != GV_LINE)
	    continue;
	
	cat_line = 0;
	if ( !Vect_cat_get(Cats, afield, &cat_line) )
	    G_warning ("Line has no category");
	
	cat_node[0] = cat_node[1] = 0;
        for (j=0; j<2; j++) {
            if (j==0)
		Vect_get_line_nodes ( &In, i, &node, NULL);
            else
		Vect_get_line_nodes ( &In, i, NULL, &node);

	    Vect_get_node_coor (&In, node, &x, &y, &z); 
            nnodes = 0;
	    
	    for ( k=0; k < Vect_get_node_n_lines ( &In, node ); k++) {
		line = abs ( Vect_get_node_line ( &In, node, k ) ); 
		ltype = Vect_read_line (&In, NULL, Cats, line);
		if ( ltype != GV_POINT)
		    continue;
		
	        Vect_cat_get(Cats, nfield, &(cat_node[j]) );
		   
                nnodes++;
	    }
    	    if (nnodes == 0)
	        G_warning ("Point not found: %.3lf %.3lf %.3lf line category: %d\n", x, y, z, cat_line);   
	    else if (nnodes > 1)
	        G_warning ("%d points found: %.3lf %.3lf %.3lf line category: %d\n", nnodes, x, y, z, cat_line);	      
          }	  
          fprintf (stdout, "%d %d %d\n", cat_line, cat_node[0], cat_node[1]);	  
      }

    Vect_close (&In);
    
    return 0;
}
