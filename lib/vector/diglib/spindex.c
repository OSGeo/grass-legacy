#include <stdlib.h>
#include <string.h>
#include "Vect.h"


int nodecmp (char *, char *);
int nodefirst ( );

/* 
*  dig_spindex_init ()
*  initit spatial index (nodes, lines, areas, isles)
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_init ( struct Plus_head *Plus) 
{
    
    G_debug(1, "dig_spidx_init()");
    
    btree_create (&(Plus->Node_spidx), nodecmp, 1000); 

    return 1;
}

/************************* ADD NEW *********************************/
/* 
*  dig_spindex_add_node ()
*  add new node to spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_add_node ( struct Plus_head *Plus, int node, 
	               double x, double y, double z) 
{
    BTREE *B;
    double coor[3];
    
    G_debug(3, "dig_spidx_add_node(): node = %d", node );

    B = &(Plus->Node_spidx);

    coor[0] = x;
    coor[1] = y;
    coor[2] = z;
   
    btree_update (B, (char *) coor, 3 * sizeof(double), (char *) &node, sizeof(int));

    return 1;
}

/* 
*  dig_spindex_add_line ()
*  add new line to spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_add_line ( struct Plus_head *Plus, int line, BOUND_BOX *Box ) 
{
    /* TODO */

    return 0;
}

/* 
*  dig_spindex_add_area ()
*  add new area to spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_add_area ( struct Plus_head *Plus, int line, BOUND_BOX *Box ) 
{
    /* TODO */

    return 0;
}

/* 
*  dig_spindex_add_isle ()
*  add new island to spatial index 
*
*  returns 1 OK
*          0 on error      
*/

int 
dig_spidx_add_isle ( struct Plus_head *Plus, int line, BOUND_BOX *Box ) 
{
    /* TODO */

    return 0;
}

/************************* SELECT BY BOX *********************************/
/* 
*  dig_select_nodes ()
*  select nodes by bbox 
*
*  returns: number of selected nodes
*           -1 error
*/
int 
dig_select_nodes ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    BTREE  *B;
    double coor[3], *c;
    int    *node, i, j;
    
    G_debug(3, "dig_select_nodes()" );
    
    list->n_values = 0;
    
    B = &(Plus->Node_spidx);

    if (B->N <= 0) return 0;
    
    if ( box->N == box->S && box->E == box->W && box->B == box->T ) { /* point */
	coor[0] = box->W;
	coor[1] = box->S;
	coor[2] = box->B;
        if ( btree_find (B, (char *) coor, (char **) &node) ) {
            dig_list_add ( list, *node );
            return 1;
        } else {
	    return 0;
        }
    } else { /* box */
        i = nodefirst ( B, box->W, box->S, box->B );
	j = B->node[i].left; 
	if ( j > 0 )
            B->cur = j;
	else
            B->cur = i;
          
        while ( btree_next (B, (char **) &c, (char **) &node) ) {
	    if ( c[0] > box->E ) break;
	    
            if ( c[0] >= box->W && c[0] <= box->E &&
                 c[1] >= box->S && c[1] <= box->N &&
                 c[2] >= box->B && c[2] <= box->T ) 
	    {
                dig_list_add ( list, *node );
	    }
	}    
        return list->n_values;
    }

}

/* 
*  dig_find_node ()
*  find one node by coordinates 
*
*  returns: number of node
*           0 not found
*/
int 
dig_find_node ( struct Plus_head *Plus, double x, double y, double z ) 
{
    BTREE  *B;
    double coor[3];
    int    *node;
    
    G_debug(3, "dig_find_node()" );
    
    B = &(Plus->Node_spidx);

    if (B->N <= 0) return 0;
    
    coor[0] = x;
    coor[1] = y;
    coor[2] = z;
	
    if ( btree_find (B, (char *) coor, (char **) &node) )
	return *node;
    
    return 0;
}

/* 
*  dig_select_lines ()
*  select lines by box 
*
*  returns: number of selected lines
*           -1 error
*/
int 
dig_select_lines ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    /* TODO */
    return -1;
}

/* 
*  dig_select_areas ()
*  select areas by box 
*
*  returns: number of selected areas
*           -1 error
*/
int 
dig_select_areas ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    /* TODO */
    return -1;
}

/* 
*  dig_select_isles ()
*  select isles by box 
*
*  returns: number of selected isles
*           -1 error
*/
int 
dig_select_isles ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    /* TODO */
    return -1;
}

/************************* WRITE TO FILE *********************************/
/* dig_write_spidx_nodes() 
*  Write spatial index for nodes to plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_write_spidx_nodes ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/* dig_write_spidx_lines() 
*  Write spatial index for lines to plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_write_spidx_lines ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/* dig_write_spidx_areas() 
*  Write spatial index for areas to plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_write_spidx_areas ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/* dig_write_spidx_isles() 
*  Write spatial index for isles to plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_write_spidx_isles ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/************************* READ FROM FILE *********************************/
/* dig_read_spidx_nodes() 
*  Reas spatial index for nodes from plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_read_spidx_nodes ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/* dig_read_spidx_lines() 
*  Reas spatial index for lines from plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_read_spidx_lines ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/* dig_read_spidx_areas() 
*  Reas spatial index for areas from plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_read_spidx_areas ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}

/* dig_read_spidx_isles() 
*  Reas spatial index for isles from plus file.
*
*  returns: 0 OK
*          -1 error
*/
int
dig_read_spidx_isles ( FILE * fp, struct Plus_head *Plus ) 
{
    /* TODO */ 
    
    return 0;
}
/************************* LOCAL ROUTINES *********************************/
int nodecmp ( char *v1, char *v2 ) 
{
      double *c1, *c2;

      c1 = (double *) v1;
      c2 = (double *) v2;

      if ( c1[0] < c2[0] ) return -1;         /* x1 < x2 */
      else if ( c1[0] > c2[0] ) return  1;    /* x1 > x2 */
      else if ( c1[1] < c2[1] ) return -1;    /* x1 = x2 & y1 < y2 */
      else if ( c1[1] > c2[1] ) return  1;    /* x1 = x2 & y1 > y2 */
      else if ( c1[2] < c2[2] ) return -1;    /* x1 = x2 & y1 = y2 & z1 < z2 */
      else if ( c1[2] > c2[2] ) return -1;    /* x1 = x2 & y1 = y2 & z1 > z2 */
      else return 0;                         /* x1 = x2 & y1 = y2 & z1 = z2 */
    
}

/* find first node which is <= box corner or first node if no one is <=
* 
*  returns:  index of node in btree
*            0 if tree is empty
*/
int nodefirst ( BTREE *B, double bx, double by, double bz)
{
    register int q;
    int (*cmp)();
    int dir;
    double c[3];
    char  *key;
    int  last;

    if (B->N <= 0)
	return 0;
    
    c[0] = bx;
    c[1] = by;
    c[2] = bz;
    key = (char *) c;
    
    cmp = B->cmp;

    q = 1; last = 1;
    while ( 1 )
    {
	dir = (*cmp)(B->node[q].key, key) ;
	
	if (dir == 0) {
	    return q;
	} 
	
	if (dir > 0) {
	    q = B->node[q].left;             /* go left */
	}
	else 
	{
	    q = B->node[q].right;            /* go right */
	}
	
	if ( q > 0 )
	    last = q;
	else 
	    return last;
    }
    
    return 0; /* not reached */
}
