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
#include "glocale.h"

#define WDTH 5

int display ( struct Map_info *Map, struct line_pnts *, int, int, int );

int path ( struct Map_info *Map, int color, int hcolor, int bgcolor )
{
    int button, ret;
    int screen_x, screen_y ;
    double x, y, nx, ny, fx, fy, tx, ty, msize;
    double x1, y1, x2, y2, maxdist;
    struct line_pnts *Points;
    int node;
    int from_disp = 0, to_disp = 0, sp_disp = 0, from_node = 0, to_node = 0;

    Points = Vect_new_line_struct();

    msize = 10 * ( D_d_to_u_col(2.0) - D_d_to_u_col(1.0) ); /* do it better */ 
    G_debug (1, "msize = %f\n", msize);
    
    fprintf (stderr, _("L: from  M: to R: quit\n"));
    
    while ( 1 ) {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	
        x = D_d_to_u_col ((double)(screen_x));
        y = D_d_to_u_row ((double)(screen_y));
    	/* fprintf (stderr, "%f %f\n", x, y); */
	    
        x1 = D_d_to_u_col ((double)(screen_x-WDTH));
        y1 = D_d_to_u_row ((double)(screen_y-WDTH));
        x2 = D_d_to_u_col ((double)(screen_x+WDTH));
        y2 = D_d_to_u_row ((double)(screen_y+WDTH));
			 
        x1 = fabs ( x2 - x1 );
        y1 = fabs ( y2 - y1 );
			     
        if ( x1 > y1 ) maxdist = x1;
        else maxdist = y1;
        G_debug (1, "Maximum distance in map units = %f\n", maxdist);

	node = Vect_find_node (Map, x, y, 0.0, maxdist, 0);

	if ( node > 0 ) {
	    Vect_get_node_coor ( Map, node, &nx, &ny, NULL); 
    	    fprintf (stderr, _("Node %d: %f %f\n"), node, nx, ny);
	}

	if ( sp_disp ) { /* erase old */
	    /* delete old highlight */
	    
            display ( Map, Points, color, from_node, to_node );
		
	    R_color(bgcolor);
	    if ( !from_node ) 
	        G_plot_line(Points->x[0], Points->y[0], Points->x[1], Points->y[1]);

	    if ( !to_node )
	        G_plot_line(Points->x[Points->n_points-2], Points->y[Points->n_points-2], 
			    Points->x[Points->n_points-1], Points->y[Points->n_points-1]);
	}
	
	switch ( button )	{
	    case 1:
		if ( from_disp ) {
		    R_color(bgcolor);
		    G_plot_icon( fx, fy, G_ICON_BOX, 0.0, msize);
                }
		if ( node > 0 ) {
		    fx = nx;
		    fy = ny;
		    from_node = 1;
		} else {
		    fx = x;
		    fy = y;
		    from_node = 0;
		}
		R_color(hcolor);
		G_plot_icon( fx, fy, G_ICON_BOX, 0.0, msize);
		R_flush();
		from_disp = 1;
	        break;
	    case 2:
		if ( to_disp ) {
		    R_color(bgcolor);
		    G_plot_icon( tx, ty, G_ICON_CROSS, 0.0, msize);
                }
		if ( node > 0 ) {
		    tx = nx;
		    ty = ny;
		    to_node = 1;
		} else {
		    tx = x;
		    ty = y;
		    to_node = 0;
		}
		R_color(hcolor);
		G_plot_icon( tx, ty, G_ICON_CROSS, 0.0, msize);
		R_flush();
		to_disp = 1;
	        break;
	    case 3:
		if ( from_disp ) {
		    R_color(bgcolor);
		    G_plot_icon( fx, fy, G_ICON_BOX, 0.0, msize);
		}
		if ( to_disp ) {
		    R_color(bgcolor);
		    G_plot_icon( tx, ty, G_ICON_CROSS, 0.0, msize);
		}
	        return 1;
	        break;
	}
	if ( from_disp && to_disp ) {
	    double fdist, tdist, cost;
	    
	    G_debug (2, "find path %f %f -> %f %f", fx, fy, tx, ty);
	    
	    ret = Vect_net_shortest_path_coor ( Map, fx, fy, 0.0, tx, ty, 0.0, 5*maxdist, 5*maxdist,
				                &cost, Points, NULL, NULL, NULL, &fdist, &tdist );
	    if ( ret == 0 ) {
		fprintf (stdout, _("Destination unreachable\n") );
		sp_disp = 0;
	    } else { 
		fprintf (stdout, _("Costs on the network = %f\n"), cost);
		fprintf (stdout, _("  Distance to the network = %f, distance from the network = %f\n"),  
			          fdist, tdist);
		
	        display ( Map, Points, hcolor, 1, 1 );
		sp_disp = 1;
	    }
	    
	}
	R_flush();
    };
    
    return 1;
}

int 
display ( struct Map_info *Map, struct line_pnts *Points, int color, int first, int last )
{
    int i, from, to;

    R_color(color);

    if ( first ) from = 0; else from = 1;
    if ( last ) to = Points->n_points; else to = Points->n_points - 1;
    
    for( i = from; i < to - 1; i++ ) 
	G_plot_line(Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1]);
	
    return 0;
}

