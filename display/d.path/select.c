#include <stdlib.h> 
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "colors.h"
#include "proto.h"

#define WDTH 5

int display ( struct Map_info *Map, struct ilist *List, int color );

int path ( struct Map_info *Map, int color, int hcolor, int bgcolor )
{
    int button;
    int screen_x, screen_y ;
    double x, y, msize;
    double x1, y1, x2, y2, maxdist;
    struct ilist *AList;
    int from = 0, to = 0, node;

    AList = Vect_new_list ();

    msize = 10 * ( D_d_to_u_col(2) - D_d_to_u_col(1) ); /* do it better */ 
    G_debug (1, "msize = %f\n", msize);
    
    fprintf (stderr, "L: from  M: to R: quit\n");
    
    while ( 1 ) {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	
        x = D_d_to_u_col ((double)(screen_x));
        y = D_d_to_u_row ((double)(screen_y));
        x1 = D_d_to_u_col ((double)(screen_x-WDTH));
        y1 = D_d_to_u_row ((double)(screen_y-WDTH));
        x2 = D_d_to_u_col ((double)(screen_x+WDTH));
        y2 = D_d_to_u_row ((double)(screen_y+WDTH));
			 
        x1 = fabs ( x2 - x1 );
        y1 = fabs ( y2 - y1 );
			     
        if ( x1 > y1 ) maxdist = x1;
        else maxdist = y1;
        G_debug (1, "Maximum distance in map units = %f\n", maxdist);

	node = Vect_find_node (Map, x, y, 0, maxdist, 0);
	
	switch ( button )	{
	    case 1:
		if ( node > 0 ) {
		    if ( from > 0 ) {
	                Vect_get_node_coor ( Map, from, &x, &y, NULL); 
                        R_color(bgcolor);
		        G_plot_icon( x, y, G_ICON_BOX, 0, msize);
                    }
		    from = node;
	            Vect_get_node_coor ( Map, from, &x, &y, NULL); 
                    R_color(hcolor);
		    G_plot_icon( x, y, G_ICON_BOX, 0, msize);
	            R_flush();
		}
	        break;
	    case 2:
		if ( node > 0 ) {
		    if ( to > 0 ) {
	                Vect_get_node_coor ( Map, to, &x, &y, NULL); 
                        R_color(bgcolor);
		        G_plot_icon( x, y, G_ICON_CROSS, 0, msize);
                    }
		    to = node;
	            Vect_get_node_coor ( Map, to, &x, &y, NULL); 
                    R_color(hcolor);
		    G_plot_icon( x, y, G_ICON_CROSS, 0, msize);
	            R_flush();
		}
	        break;
	    case 3:
		if ( from > 0 ) {
		    Vect_get_node_coor ( Map, from, &x, &y, NULL); 
		    R_color(bgcolor);
		    G_plot_icon( x, y, G_ICON_BOX, 0, msize);
		}
		if ( to > 0 ) {
		    Vect_get_node_coor ( Map, to, &x, &y, NULL); 
		    R_color(bgcolor);
		    G_plot_icon( x, y, G_ICON_CROSS, 0, msize);
		}
                display ( Map, AList, color );
	        return 1;
	        break;
	}
	if ( node > 0 ) {
	    /* delete old highlight */
            display ( Map, AList, color );

	    if ( from > 0 && to > 0 && from != to ) {
		G_debug (1, "find path %d -> %d\n", from, to);
		Vect_net_shortest_path ( Map, from, to, AList);
		
                display ( Map, AList, hcolor );
	    }
	    
	}
	R_flush();
    };
    
    Vect_destroy_list ( AList );

    return 1;
}

int 
display ( struct Map_info *Map, struct ilist *List, int color )
{
    int i, j, line;
    struct line_pnts *Points;

    Points = Vect_new_line_struct ();
    R_color(color);

    for ( i = 0; i < List->n_values; i++ ) {
        line = abs(List->value[i]);
	Vect_read_line ( Map, Points, NULL, line );
	
        for( j=0; j < Points->n_points - 1; j++ ) 
            G_plot_line(Points->x[j], Points->y[j], Points->x[j+1], Points->y[j+1]);
	
    }	
    
    Vect_destroy_line_struct(Points);

    return 0;
}

