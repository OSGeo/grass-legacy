/***************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

void
dig_line_reset_updated ( struct Plus_head *Plus )
{
    Plus->n_uplines = 0;
}

void
dig_line_add_updated ( struct Plus_head *Plus, int line )
{
    int i;

    G_debug (3, "dig_line_add_updated(): line = %d", line);

    /* Check if already in list */
    for ( i = 0; i < Plus->n_uplines; i++ )
	if ( Plus->uplines[i] == line ) return;
    
    /* Alloc space if needed */
    if ( Plus->n_uplines == Plus->alloc_uplines ) {
	 Plus->alloc_uplines += 1000;
	 Plus->uplines = (int *) G_realloc ( Plus->uplines, Plus->alloc_uplines * sizeof(int) );
    }

    Plus->uplines[Plus->n_uplines] = line;
    Plus->n_uplines++;
}

void 
dig_node_reset_updated ( struct Plus_head *Plus )
{
    Plus->n_upnodes = 0;
}

void
dig_node_add_updated ( struct Plus_head *Plus, int node )
{
    int i;

    G_debug (3, "dig_node_add_updated(): node = %d", node);

    /* Check if already in list */
    for ( i = 0; i < Plus->n_upnodes; i++ )
	if ( Plus->upnodes[i] == node ) return;
    
    /* Alloc space if needed */
    if ( Plus->n_upnodes == Plus->alloc_upnodes ) {
	 Plus->alloc_upnodes += 1000;
	 Plus->upnodes = (int *) G_realloc ( Plus->upnodes, Plus->alloc_upnodes * sizeof(int) );
    }

    Plus->upnodes[Plus->n_upnodes] = node;
    Plus->n_upnodes++;
}

