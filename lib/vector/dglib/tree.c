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

/* best view tabstop=4
 */
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "bst.c"

#include "type.h"
#include "tree.h"

typedef struct {
	struct bst_table * pbst;
	int (* pfnNodeCancel )( gnTreeNode_s * , void * );
	void * pvNodeCancel;
} gnTree_s;


static void * _tree_malloc(struct libavl_allocator * allocator, size_t libavl_size)
{
	return malloc( libavl_size );
}

static void _tree_free(struct libavl_allocator * allocator, void *libavl_block)
{
	free( libavl_block );
}

static struct libavl_allocator _tree_allocator = {
	_tree_malloc , _tree_free
};


/* node item comparison */
static int _bst_compare(const void * bst_a, const void * bst_b, void * bst_param )
{
	gnTreeNode_s * pa = (gnTreeNode_s*)bst_a;
	gnTreeNode_s * pb = (gnTreeNode_s*)bst_b;
	if ( pa->key < pb->key ) return -1;
	if ( pa->key > pb->key ) return 1;
	return 0;
}

/* node item cancel */
static void _bst_cancel(void *bst_item, void *bst_param)
{
	gnTreeNode_s * pn = (gnTreeNode_s*)bst_item;
	gnTree_s * ptree = bst_param;

	ptree->pfnNodeCancel( pn , ptree->pvNodeCancel );
}


void * gnTreeCreate( int (* pfnNodeCancel )( gnTreeNode_s * , void * ) , void * pvcancel )
{
	gnTree_s * ptree = malloc( sizeof(gnTree_s) );
	if ( ptree == NULL ) {
		return NULL;
	}
	ptree->pfnNodeCancel = pfnNodeCancel;
	ptree->pvNodeCancel = pvcancel;
	ptree->pbst = bst_create(_bst_compare, ptree, &_tree_allocator);
	if ( ptree->pbst == NULL ) {
		free( ptree );
		return NULL;
	}
	return ptree;
}

void gnTreeDestroy( void * pvtree )
{
	gnTree_s * ptree = pvtree;
	bst_destroy( ptree->pbst , _bst_cancel );
	free( pvtree );
}

void gnTreeInitNode( gnTreeNode_s * pnode , long key , gnTreeData_u data )
{
	if ( pnode )
	{
		memset( pnode , 0 , sizeof( gnTreeNode_s ) );
		pnode->key = key;
		pnode->data = data;
	}
}

void gnTreeSetNode( gnTreeNode_s * pnode , long key , gnTreeData_u data )
{
	if ( pnode )
	{
		pnode->key = key;
		pnode->data = data;
	}
}

gnTreeNode_s * gnTreeNewNode( long key , gnTreeData_u data )
{
	gnTreeNode_s * pnode;

	pnode = malloc( sizeof( gnTreeNode_s ) );
	gnTreeInitNode( pnode , key , data );
	return pnode;
}

gnTreeNode_s * gnTreeSearch ( void * pvtree , long key )
{
	gnTree_s * ptree = pvtree;
	gnTreeNode_s node , * pnode;

	node.key = key;
	pnode = bst_find( ptree->pbst , & node );
	return pnode;
}

int gnTreeInsert( void * pvtree , gnTreeNode_s *pnode )
{
	gnTree_s * ptree = pvtree;
	void ** pitem;

	pitem = bst_probe( ptree->pbst , pnode );
	if ( pitem == NULL ) return -1;
	if ( *pitem != pnode ) return -2;
	return 0;
}
	
/*
 * The caller is responsible of freeing the returned node-pointer
 */
gnTreeNode_s * gnTreeCancel( void * pvtree , gnTreeNode_s * pnode )
{
	gnTree_s * ptree = pvtree;
	gnTreeNode_s * pitem;

	pitem = bst_delete( ptree->pbst , pnode );
	return pitem;
}





#if 0


#include <unistd.h>
#include <time.h>


int main( int argc , char ** argv )
{

	long 			i , x , c = 20000 , mod;
	gnTreeData_u		data;
	gnTreeNode_s * 	proot = NULL;
	gnTreeNode_s * 	pnode;
	char 			szdata[ 64 ];
	time_t			t;

	printf( "inserting %ld nodes: " , c );
	fflush( stdout );

	mod = c / 100;

	for( i = 0 ; i < c ; i ++ )
	{
		if ( (i % 2) == 0 ) 	x = c - i;
		else			x = i;


		sprintf( szdata , "#%ld - ", x );
		t = time( 0L );
		strcat( szdata , ctime( & t ) );
		szdata[ strlen( szdata ) - 1 ] = 0;
		data.pv = strdup( szdata );

		pnode = gnTreeNewNode( x , data );

		if ( gnTreeInsert( & proot , pnode ) < 0 )
		{
			fprintf( stderr , "error inserting node %ld - key %ld\n" , i , x );
			return 1;
		}

		if ( (i % mod) == 0 ) { printf( "<%ld lev:%ld> " , i , g_level ); fflush( stdout ); }
	}
	printf( "\nEnd of insert, try to search:\n" );

	for( i=0 ; i < c ; i ++ )
	{
		/* x = rand(); */

		pnode = gnTreeSearch( proot , i );

		if ( pnode == NULL )
		{
			printf( "node %ld not found\n" , i );
		}
		else
		{
			/* printf( "%ld: <%s>\n" , i , pnode->data.pv ); */
		}
	}

	printf( "Search of %ld nodes. Done.\n" , i );

	return 0;
}

#endif
