/* LIBDGL -- a Directed Graph Library implementation
 * Copyright (C) 2002 Roberto Micarelli
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/*
 * best view with tabstop=4
 */


#include <stdlib.h>
#include <string.h>

#include "type.h"
#include "tree.h"
#include "helpers.h"

/*
 * helpers for parametric stack
 */
unsigned char * gngrp_mempush( unsigned char * pstack , long * istack , long size , void * pv )
{
	if ( *istack == 0 ) pstack = NULL;
	pstack = realloc( pstack , size * (1 + *istack) );
	if ( pstack == NULL ) return NULL;
	memcpy( & pstack[ (*istack) * size ] , pv , size );
	(*istack) ++;
	return pstack;
}

unsigned char * gngrp_mempop( unsigned char * pstack , long * istack , long size )
{
	if ( *istack == 0 ) return NULL;
	return & pstack[ size * (--(*istack)) ];
}

void gngrp_swapInt32Bytes( gnInt32_t * pn ) {
	unsigned char * pb = (unsigned char *) pn;
	pb[0] ^= pb[3];
	pb[3] ^= pb[0];
	pb[0] ^= pb[3];
	pb[1] ^= pb[2];
	pb[2] ^= pb[1];
	pb[1] ^= pb[2];
}	


int gngrp_node_free( gnTreeNode_s * pnode , void * pv )
{
	if ( pnode ) {
		if ( pnode->data.pv ) {
			free( pnode->data.pv );
		}
		if ( pnode->data2.pv ) {
			free( pnode->data2.pv );
		}
		free( pnode );
	}
	return 0;
}

gnTreeNode_s * gngrp__node( gnTreeNode_s * ptree , gnInt32_t nodeid )
{
	gnTreeNode_s * pnode , * pnoderet;

	if ( (pnode = gnTreeNewNode( nodeid , (gnTreeData_u)0 , (gnTreeData_u)0 )) == NULL ) return NULL;

	pnoderet = gnTreeInsert( ptree , pnode );

	if ( pnoderet != pnode ) {
		free( pnode );
		pnode = pnoderet;
	}

	return pnode;
}

