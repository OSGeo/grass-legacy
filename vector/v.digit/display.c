#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "raster.h"
#include "display.h"
#include "D.h" 
#include "colors.h"
#include "global.h"
#include "proto.h"

/* --- DISLAY ---
*  For all display functions the display driver must be opened first.
*  Because some functions like erase() call other grass commands, driver is closed
*  and reopened within these functions
*/

/* Display points */
void display_points ( struct line_pnts *Points )
{
    int i;
    
    G_debug (2, "display_points()");

    for(i=1; i < Points->n_points; i++) {
        G_plot_line ( Points->x[i-1], Points->y[i-1], Points->x[i], Points->y[i]);
    }
    R_flush();
}

/* Display icon */
void display_icon ( double x, double y, int icon, double angle, int size )
{
    G_debug (2, "display_icon()");

    G_plot_icon(x, y, icon, angle, Scale * size);

    R_flush();
}

/* Display vector line 
*  color : code from SymbNumber 
*
*  ! This function doesn't check Symb[symb].on so that new digitized line is displayed even
*    if its symbology is switched off (that means incorrect and user friendly)
*/
void display_line ( int line, int color )
{
    int type, symb;
    static struct line_pnts *Points;
    static struct line_cats *Cats;
    static int first = 1;
    
    G_debug (2, "display_line(): line = %d color = %d", line, color );

    if ( first ) {
        Points = Vect_new_line_struct ();
        Cats = Vect_new_cats_struct ();
	first = 0;
    }

    if ( !Vect_line_alive ( &Map, line ) ) return;

    type =  Vect_read_line ( &Map, Points, Cats, line); 

    if ( color == SYMB_DEFAULT ) symb = LineSymb[line];
    else symb = color;

    symb_set_driver_color ( symb );
    
    if ( type & GV_POINTS ) display_icon ( Points->x[0], Points->y[0], G_ICON_CROSS, 0, 6);
    else display_points ( Points );
}

/* Redraw updated lines */
void
display_updated_lines ( int symb )
{
    int i, line;
	 
    for ( i = 0; i < Vect_get_num_updated_lines(&Map); i++ ) {
        line = Vect_get_updated_line( &Map, i );
        if ( !Vect_line_alive ( &Map, line ) ) continue;
        display_line ( line, symb );
   }
}

/* Display node, color may be given but shape and size is read from symbology table,
*  this is useful to delete (redraw by background) existing node
*  
*  color : code from SymbNumber 
*/
void display_node ( int node, int color )
{
    int symb;
    double x, y;
    
    G_debug (2, "display_node(): node = %d color = %d", node, color );

    if ( !Vect_node_alive ( &Map, node ) ) return;

    if ( color == SYMB_DEFAULT ) symb = NodeSymb[node];
    else symb = color;

    symb_set_driver_color ( symb );
    Vect_get_node_coor ( &Map, node, &x, &y, NULL);
    display_icon ( x, y, G_ICON_CROSS, 0.785, 6);
}
   
/* Redraw updated nodes */
void
display_updated_nodes ( int symb )
{
    int i, node;
	 
    if ( symb != SYMB_DEFAULT ) symb_set_driver_color ( symb );
	     
    for ( i = 0; i < Vect_get_num_updated_nodes(&Map); i++ ) {
        node = Vect_get_updated_node( &Map, i );
	if ( !Vect_node_alive ( &Map, node ) ) continue;
	if ( NodeSymb[node] == SYMB_NODE_0 ) continue;
	display_node ( node, symb );
    }
}

/* Display vector map */
void display_map ( void )
{
    int i, n, symb;
    
    G_debug (2, "display_map()");

    /* Because after resize of monitor we expect manual call to display_map()
    *  it is good to refresh D_* here */
    driver_refresh ();
    
    /* Lines */
    n = Vect_get_num_lines ( &Map );
    for(i=1; i <= n; i++) {
	symb = LineSymb[i];
	G_debug (2, "symb = %d", symb);
	if ( !Symb[symb].on ) continue;
	display_line ( i , SYMB_DEFAULT );
    }
    
    /* Nodes: first nodes with more than 1 line and than with 1 line, so that dangle cannot be hidden,
    *         nodes without lines (points, centroids, are not displayed) */
    n = Vect_get_num_nodes ( &Map );
    symb_set_driver_color ( SYMB_NODE_1 );
    for(i=1; i <= n; i++) {
	G_debug ( 2, "node = %d NodeSymb = %d", i, NodeSymb[i]); 
	if ( !Vect_node_alive ( &Map, i) ) continue;
	if ( NodeSymb[i] != SYMB_NODE_1 ) continue;
	display_node(i, NodeSymb[i] );
    }
    symb_set_driver_color ( SYMB_NODE_2 );
    for(i=1; i <= n; i++) {
	if ( !Vect_node_alive ( &Map, i) ) continue;
	if ( NodeSymb[i] != SYMB_NODE_2 ) continue;
	display_node(i, NodeSymb[i]);
    }
}

/* Display bacground */
void display_bg ( void )
{
    int i;
    
    G_debug (2, "display_bg()");

    driver_close();
    for(i=0; i < nbgcmd; i++) {
	system ( Bgcmd[i] );
    }
    driver_open();
}

/* Erase */
void display_erase ( void )
{
    char command[128];
    
    driver_close();
    sprintf(command, "d.erase color=white");
    system( command ); /* It does everything and command is registered */
    driver_open();

    /* As erase must be run after each zoom by v.digit, here is good place to reset plot.
    *  Other such place is display_map() */
    driver_refresh ();
}

/* Redraw */
void display_redraw ( void )
{
    display_erase ();
    display_bg();
    display_map();
}

