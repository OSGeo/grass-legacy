#include <stdlib.h>
#include "Vect.h"

/* dig_add_line ()
** Add new line to plus structure.
** 
**
** Returns -1 on error      
**          number of line
*/
int 
dig_add_line (struct Plus_head *plus, int type, struct line_pnts *Points, long offset){
    int  lineid, node, lp;
    char *p;
    P_LINE_2D *line;
    
    /* First look if we have space in array of pointers to lines
    *  and reallocate if necessary */
    if ( plus->n_lines >= plus->alloc_lines ) { /* array is full */
	if ( dig_alloc_lines_2d(plus,1000) == -1 )
	    return -1;
    }
    
    /* allocate line structure */
    lineid = plus->n_lines + 1;
    plus->Line_2d[lineid] = dig_alloc_line_2d();
    line = plus->Line_2d[lineid];
    
    /* Add nodes */
    G_debug ( 3, "Register node: type = %d,  %f,%f", type, Points->x[0], Points->y[0]);
    node = dig_which_node ( plus, Points->x[0], Points->y[0], 0);
    G_debug ( 3, "node = %d", node);
    if ( node == -1 ) {
	node = dig_add_node ( plus, Points->x[0], Points->y[0] );
	G_debug ( 3, "Add new node: %d", node);
    } else {
	G_debug ( 3, "Old node found: %d", node);
    }	
    line->N1 = node;
    dig_node_add_line (plus, node, lineid, Points, type );
     
    if ( type & GV_LINES ) {
	lp = Points->n_points - 1;
	G_debug ( 3, "Register node %f,%f", Points->x[lp], Points->y[lp]);
	node = dig_which_node ( plus, Points->x[lp], Points->y[lp], 0);
	G_debug ( 3, "node = %d", node);
	if ( node == -1 ) {
	    node = dig_add_node ( plus, Points->x[lp], Points->y[lp] );
	    G_debug ( 3, "Add new node: %d", node);
	} else {
	    G_debug ( 3, "Old node found: %d", node);
	}
        line->N2 = node;
        dig_node_add_line (plus, node, -lineid, Points, type );
    } else {
        line->N2 = 0;
    }

    line->type = type;
    line->offset = offset;
    line->left = 0;
    line->right = 0;
    line->N = 0;
    line->S = 0;
    line->E = 0;
    line->W = 0;
    plus->n_lines++;

    return ( lineid );
}

/* dig_line_get_area ()
** Get area number on line side
** 
** Returns area number 
**         0 no area
**         -1 error
*/
plus_t
dig_line_get_area (struct Plus_head *plus, plus_t line, int side) {
    P_LINE_2D *Line;
    
    Line = plus->Line_2d[line];
    if ( side == GV_LEFT  ) { 
	G_debug ( 3, "dig_line_get_area(): line = %d, side = %d (left), area = %d", 
		      line, side, Line->left ); 
	return (Line->left );
    }
    if ( side == GV_RIGHT ) {
	G_debug ( 3, "dig_line_get_area(): line = %d, side = %d (right), area = %d", 
		      line, side, Line->right );
       
	return (Line->right); 
    }

    return (-1);
}

/* dig_line_set_area ()
** Set area number on line side
** 
*/
int
dig_line_set_area (struct Plus_head *plus, plus_t line, int side, plus_t area ) {
    P_LINE_2D *Line;
    
    Line = plus->Line_2d[line];
    if ( side == GV_LEFT  ) { Line->left = area; }
    else if ( side == GV_RIGHT ) { Line->right = area; }

    return (1);
}

