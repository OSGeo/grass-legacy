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

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <malloc.h>
#include <errno.h>

#include "type.h"
#include "graph.h"

void gnGrpResetStats( gnGrpGraph_s * pgraph )
{
#ifdef GNGRP_STATS
	pgraph->clkAddLink = 0;
	pgraph->cAddLink = 0;
	pgraph->clkLinkTree = 0;
	pgraph->clkNodeTree = 0;
	pgraph->clkNodeHeap = 0;
	pgraph->cLinkTree = 0;
	pgraph->cNodeTree = 0;
	pgraph->cNodeHeap = 0;
#endif
}

static int _node_free( gnTreeNode_s * pnode , void * pv )
{
	if ( pnode ) {
		if ( pnode->data.pv ) {
			free( pnode->data.pv );
		}
		free( pnode );
	}
	return 0;
}

static gnTreeNode_s * __node( gnTreeNode_s * ptree , gnInt32_t nodeid )
{
	gnTreeNode_s * pnode;
	gnTreeData_u	data;

	memset( & data , 0 , sizeof( data ) );

	if ( (pnode = gnTreeNewNode( nodeid , data )) == NULL ) return NULL;
	if ( gnTreeInsert( ptree , pnode ) < 0 )
		free( pnode );
	pnode = gnTreeSearch ( ptree , nodeid );

	return pnode;
}

static int _add_link_V1  (
						gnGrpGraph_s * 	pgraph ,
						gnInt32_t *		ppar ,
						void * pvFnodeAttr ,	
						void * pvTnodeAttr ,	
						void * pvLinkAttr
						)
{
	gnInt32_t *		pfrom;
	gnInt32_t *		pto;
	gnInt32_t *		plinkarea;
	gnInt32_t *		plink;
	gnInt32_t		ilink;
	gnTreeNode_s * 	pFromNodeItem;
	gnTreeNode_s * 	pToNodeItem;
	gnTreeNode_s * 	pLinkItem;
	gnHeapData_u	HeapData;
	
	gnInt32_t lFrom		= ppar[ 0 ];
	gnInt32_t lTo		= ppar[ 1 ];
	gnInt32_t lCost		= ppar[ 2 ];
	gnInt32_t lUser		= ppar[ 3 ];
	

	if ( pgraph->Flags & 0x1 )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnFlatGraph;
		return -1;
	}

	if ( pgraph->pNodeTree == NULL ) pgraph->pNodeTree = gnTreeCreate( _node_free , pgraph );
	if ( pgraph->pNodeTree == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -1;
	}

	if ( pgraph->pLinkTree == NULL ) pgraph->pLinkTree = gnTreeCreate( _node_free , pgraph );
	if ( pgraph->pLinkTree == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -1;
	}

#ifdef GNGRP_STATS
		{
		clock_t clk = clock();
#endif

		if ( (pFromNodeItem = __node( pgraph->pNodeTree , lFrom )) == NULL ) return -1;
		if ( (pToNodeItem   = __node( pgraph->pNodeTree , lTo   )) == NULL ) return -1;

#ifdef GNGRP_STATS
		pgraph->clkNodeTree += clock() - clk;
		pgraph->cNodeTree ++;
		pgraph->cNodeTree ++;
		}
#endif

	if ( pFromNodeItem->data.pv == NULL )
	{
		if ( (pfrom = GNGRP_C_ALLOC( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -1;
		}
		memset( pfrom , 0 , GNGRP_C_SIZEOF( pgraph->NodeAttrSize ) );

		pFromNodeItem->data.pv = pfrom;
		HeapData.pv        = pfrom;

#ifdef GNGRP_STATS
		{
		clock_t clk = clock();
#endif

		if ( gnHeapInsertMin( & pgraph->NodeHeap , lFrom , HeapData ) < 0 ) return -1;

#ifdef GNGRP_STATS
		pgraph->clkNodeHeap += clock() - clk;
		pgraph->cNodeHeap ++;
		}
#endif
	}
	else
	{
		pfrom = pFromNodeItem->data.pv;
	}

	if ( pToNodeItem->data.pv == NULL )
	{
		if ( (pto = GNGRP_C_ALLOC( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -1;
		}
		memset( pto , 0 , GNGRP_C_SIZEOF( pgraph->NodeAttrSize ) );

		pToNodeItem->data.pv = pto;
		HeapData.pv      = pto;

#ifdef GNGRP_STATS
		{
		clock_t clk = clock();
#endif

		if ( gnHeapInsertMin( & pgraph->NodeHeap , lTo , HeapData ) < 0 ) return -1;

#ifdef GNGRP_STATS
		pgraph->clkNodeHeap += clock() - clk;
		pgraph->cNodeHeap ++;
		}
#endif

	}
	else
	{
		pto = pToNodeItem->data.pv;
	}

	pfrom[ GNGRP_C_STATUS ] |= GNGRP_NF_FROM;
	pto  [ GNGRP_C_STATUS ] |= GNGRP_NF_TO;

	pfrom[ GNGRP_C_NODEID ] = lFrom;
	pto  [ GNGRP_C_NODEID ] = lTo;

	pfrom[ GNGRP_C_OFFSET ] = -1;
	pto  [ GNGRP_C_OFFSET ] = -1;

	if ( pvFnodeAttr && pgraph->NodeAttrSize ) {
		memcpy( & pfrom[ GNGRP_C_ATTR ], pvFnodeAttr, pgraph->NodeAttrSize );
	}

	if ( pvTnodeAttr && pgraph->NodeAttrSize ) {
		memcpy( & pto[ GNGRP_C_ATTR ], pvTnodeAttr, pgraph->NodeAttrSize );
	}

#ifdef GNGRP_STATS
	{
	clock_t clk = clock();
#endif

	if ( (pLinkItem = __node( pgraph->pLinkTree , lFrom )) == NULL ) return -1;

#ifdef GNGRP_STATS
	pgraph->clkLinkTree += clock() - clk;
	pgraph->cLinkTree ++;
	}
#endif

	if ( pLinkItem->data.pv == NULL )
	{
		pLinkItem->data.pv = GNGRP_F_ALLOC( 1 , pgraph->LinkAttrSize );
		if ( pLinkItem->data.pv == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -1;
		}
		plinkarea = pLinkItem->data.pv;
		plinkarea[ GNGRP_F_TOCNT ] = 0;
	}
	else
	{
		plinkarea = pLinkItem->data.pv;
		pLinkItem->data.pv = GNGRP_F_REALLOC( pLinkItem->data.pv , plinkarea[ GNGRP_F_TOCNT ] + 1 , pgraph->LinkAttrSize );
		if ( pLinkItem->data.pv == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -1;
		}
		plinkarea = pLinkItem->data.pv;
	}

	ilink = plinkarea[ GNGRP_F_TOCNT ];

	plink = & plinkarea[ GNGRP_F_TOARR + ilink * GNGRP_T_WSIZE(pgraph->LinkAttrSize) ];

	plink[ GNGRP_T_OFFSET ]	= lTo;		/* will be an offset after flattening */
	plink[ GNGRP_T_COST   ]	= lCost;
	plink[ GNGRP_T_USER   ]	= lUser;

	if ( pvLinkAttr && pgraph->LinkAttrSize ) {
		memcpy( & plink[ GNGRP_T_ATTR ], pvLinkAttr, pgraph->LinkAttrSize );
	}

	plinkarea[ GNGRP_F_TOCNT ] ++;

	return 0;
}



/* BINARY SEARCH FOR A NODE
 *          +----------------+
 *   top -> |                | 0
 *          +----------------+ 
 *          |                | 1
 *          +----------------+
 *   pos -> |                | 2
 *          +----------------+
 *          |                | 3
 *          +----------------+
 *   bot -> |       X        |
 */

static gnInt32_t * _search_node_V1( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	register gnInt32_t 	top;	/* top of table */
	register gnInt32_t 	pos;	/* current position to compare */
	register gnInt32_t 	bot;	/* bottom of table */
	register gnInt32_t *pref;
	register int		cwords; /* size of a node in words of 32 bit */

	cwords = GNGRP_C_SIZEOF(pgraph->NodeAttrSize) / sizeof(gnInt32_t);
	bot    = pgraph->iNodeBuffer / GNGRP_C_SIZEOF(pgraph->NodeAttrSize);
	top    = 0;
	pos    = 0;
	pref   = (gnInt32_t*)pgraph->pNodeBuffer;

	/* perform a binary search
	 */
	while( top != bot )
	{
		pos = top + (bot - top) / 2;

		if ( pref[ pos * cwords + GNGRP_C_NODEID ] == nodeid )
		{
			break;
		}
		else if ( nodeid < pref[ pos * cwords + GNGRP_C_NODEID ] )
		{
			bot = pos;
		}
		else if ( nodeid > pref[ pos * cwords + GNGRP_C_NODEID ] )
		{
			top = pos + 1;
		}
	}

	if ( top == bot ) return NULL;

	return & pref[ pos * cwords ];
}


static int _unflatten_V1( gnGrpGraph_s * pgraph )
{
	register gnInt32_t *		pnode;
	register gnInt32_t *		pnodeto;
	register gnInt32_t *		plink;
	register gnInt32_t *		plinkto;
	register int				i , k;
	gnInt32_t					par[ 4 ];

	if ( ! (pgraph->Flags & 0x1) )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnNoFlatGraph;
		return -1;
	}

	for( i = 0 ; i < pgraph->cNode ; i ++ )
	{
		pnode = (gnInt32_t*) (pgraph->pNodeBuffer + (GNGRP_C_SIZEOF(pgraph->NodeAttrSize) * i));
		if ( pnode[ GNGRP_C_STATUS ] & GNGRP_NF_FROM )
		{
			plink = (gnInt32_t*) (pgraph->pLinkBuffer + pnode[ GNGRP_C_OFFSET ]);
			for	(
				plinkto = plink + GNGRP_F_TOARR , k = 0 ;
				k < plink[ GNGRP_F_TOCNT ] ;
				k ++ , plinkto += GNGRP_T_WSIZE(pgraph->LinkAttrSize)
				)
			{
				pnodeto = (gnInt32_t*) (pgraph->pNodeBuffer + plinkto[ GNGRP_T_OFFSET ]);
				par[ 0 ] = pnode[ GNGRP_C_NODEID ];
				par[ 1 ] = pnodeto[ GNGRP_C_NODEID ];
				par[ 2 ] = plinkto[ GNGRP_T_COST ];
				par[ 3 ] = plinkto[ GNGRP_T_USER ];

				if ( _add_link_V1 ( pgraph , par , &pnode[GNGRP_C_ATTR], &pnodeto[GNGRP_C_ATTR], &plinkto[GNGRP_T_ATTR] ) < 0 )
				{
					return -1;
				}
			}
		}
	}

	free( pgraph->pNodeBuffer );
	free( pgraph->pLinkBuffer );

	pgraph->pNodeBuffer = NULL; /* buffers only needed in flat mode */
	pgraph->pLinkBuffer = NULL;

	pgraph->Flags &= ~0x1;
	return 0;
}


static int _flatten_V1( gnGrpGraph_s * pgraph )
{
	register gnHeapNode_s * pheapnode;
	register gnTreeNode_s * ptreenode;
	register int 			i;
	register gnInt32_t *	pnode;
	register gnInt32_t *	pfrom;
	register gnInt32_t *	pflat1;
	register gnInt32_t *	pflat2;
	register gnInt32_t *	pflat3;


	if ( pgraph->Flags & 0x1 )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnFlatGraph;
		return -1;
	}

	pgraph->pNodeBuffer = NULL; /* should be already, repeated for security */
	pgraph->iNodeBuffer = 0;
	pgraph->pLinkBuffer = NULL;
	pgraph->iLinkBuffer = 0;
	pgraph->cNode       = 0;
	pgraph->cFrom       = 0;
	pgraph->cTo         = 0;
	pgraph->cArc        = 0;

	while ( (pheapnode = gnHeapExtractMin( & pgraph->NodeHeap )) != NULL )
	{
		if ( (pnode = pheapnode->value.pv) != NULL )
		{
			if ( pnode[ GNGRP_C_STATUS ] & GNGRP_NF_FROM )
			{
				pgraph->cFrom ++;

				if ( (ptreenode = gnTreeSearch( pgraph->pLinkTree , pnode[ GNGRP_C_NODEID ] )) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_TreeSearchError;
					return -1;
				}

				if ( (pfrom = ptreenode->data.pv) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
					return -1;
				}

				pgraph->pLinkBuffer = realloc(
											 pgraph->pLinkBuffer , 
											 pgraph->iLinkBuffer + GNGRP_F_SIZEOF(pfrom[ GNGRP_F_TOCNT ], pgraph->LinkAttrSize)
											 );

				if ( pgraph->pLinkBuffer == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
					return -1;
				}

				memcpy(
						pgraph->pLinkBuffer + pgraph->iLinkBuffer ,
						pfrom ,
						GNGRP_F_SIZEOF(pfrom[ GNGRP_F_TOCNT ], pgraph->LinkAttrSize)
						);

				pnode[ GNGRP_C_OFFSET ] = pgraph->iLinkBuffer;

				pgraph->iLinkBuffer += GNGRP_F_SIZEOF(pfrom[ GNGRP_F_TOCNT ], pgraph->LinkAttrSize);
			}

			if ( pnode[ GNGRP_C_STATUS ] & GNGRP_NF_TO )
			{
				pgraph->cTo ++;
			}

			pgraph->cNode ++;

			pgraph->pNodeBuffer = realloc(pgraph->pNodeBuffer, pgraph->iNodeBuffer + GNGRP_C_SIZEOF(pgraph->NodeAttrSize));

			if ( pgraph->pNodeBuffer == NULL )
			{
				pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
				return -1;
			}

			memcpy( pgraph->pNodeBuffer + pgraph->iNodeBuffer , pnode , GNGRP_C_SIZEOF(pgraph->NodeAttrSize) );
			pgraph->iNodeBuffer += GNGRP_C_SIZEOF(pgraph->NodeAttrSize);
		}
	}

	gnTreeDestroy( pgraph->pNodeTree );
	pgraph->pNodeTree = NULL;

	gnTreeDestroy( pgraph->pLinkTree );
	pgraph->pLinkTree = NULL;

	gnHeapFree( & pgraph->NodeHeap );

	/*
	 * convert to-node ids from links into offset in the node buffer
	 */
	for (
		pflat1 = (gnInt32_t*) pgraph->pNodeBuffer ;
		pflat1 < (gnInt32_t*) (pgraph->pNodeBuffer + pgraph->iNodeBuffer) ;
		pflat1 += (GNGRP_C_SIZE + pgraph->NodeAttrSize / sizeof(gnInt32_t))
		)
	{
		if ( pflat1[ GNGRP_C_STATUS ] & GNGRP_NF_FROM )
		{
			pflat2 = (gnInt32_t*) (pgraph->pLinkBuffer + pflat1[ GNGRP_C_OFFSET ]);
			for (
				 i = 0 , pflat3 = & pflat2[ GNGRP_F_TOARR ] ;
				 i < pflat2[ GNGRP_F_TOCNT ] ;
				 i ++ , pflat3 += (GNGRP_T_SIZE + pgraph->LinkAttrSize / sizeof(gnInt32_t))
				)
			{
				if ( (pnode = _search_node_V1( pgraph , pflat3[ GNGRP_T_OFFSET ] )) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
					return -1;
				}
				pflat3[ GNGRP_T_OFFSET ] = (unsigned long)pnode - (unsigned long)pgraph->pNodeBuffer;
				pgraph->cArc ++;
			}
		}
	}

	pgraph->Flags |= 0x1; /* flattened */
	return 0;
}

static int _set_nodeattr_V1( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t nodeid )
{
	gnInt32_t *			pnode;
	gnTreeNode_s * 		pnodeitem;

	if ( pgraph->Flags & 0x1 )
	{
		if ( (pnode = _search_node_V1( pgraph , nodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_NodeNotFound;
			return -1;
		}
		memcpy( & pnode[ GNGRP_C_ATTR ] , pattr , pgraph->NodeAttrSize );
	}
	else
	{
		if ( (pnodeitem = gnTreeSearch( pgraph->pNodeTree , nodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_TreeSearchError;
			return -1;
		}
		pnode = pnodeitem->data.pv;
		memcpy( & pnode[ GNGRP_C_ATTR ] , pattr , pgraph->NodeAttrSize );
	}

	return 0;
}

static void * _get_nodeattr_V1( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	gnInt32_t *			pnode;
	gnTreeNode_s * 		pnodeitem;

	if ( pgraph->NodeAttrSize == 0 ) return NULL;

	if ( pgraph->Flags & 0x1 )
	{
		if ( (pnode = _search_node_V1( pgraph , nodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_NodeNotFound;
			return NULL;
		}
		return & pnode[ GNGRP_C_ATTR ];
	}
	else
	{
		if ( (pnodeitem = gnTreeSearch( pgraph->pNodeTree , nodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_TreeSearchError;
			return NULL;
		}
		pnode = pnodeitem->data.pv;
		return & pnode[ GNGRP_C_ATTR ];
	}
}

static int _set_linkattr_V1( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	gnInt32_t *			pnode;
	gnInt32_t *			plinkarea;
	gnInt32_t *			plink;
	gnTreeNode_s * 		pitem;
	int i;

	if ( pgraph->Flags & 0x1 )
	{
		if ( (pnode = _search_node_V1( pgraph , fnodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_NodeNotFound;
			return -1;
		}
		plinkarea = (gnInt32_t*)(pgraph->pLinkBuffer + pnode[ GNGRP_C_OFFSET ]);
		for ( i = 0 ; i < plinkarea[ GNGRP_F_TOCNT ] ; i ++ ) {
			plink = & plinkarea[ GNGRP_F_TOARR ] + GNGRP_T_WSIZE(pgraph->LinkAttrSize) * i;
			if ( pgraph->pNodeBuffer[ plink[GNGRP_T_OFFSET] + GNGRP_C_NODEID ] == tnodeid ) {
				break;
			}
		}
		if ( i == plinkarea[ GNGRP_F_TOCNT ] ) {
			pgraph->iErrno = GNGRP_ERR_NodeNotFound;
			return -1;
		}	
		memcpy( & plink[ GNGRP_T_ATTR ] , pattr , pgraph->LinkAttrSize );
	}
	else
	{
		if ( (pitem = gnTreeSearch( pgraph->pLinkTree , fnodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_TreeSearchError;
			return -1;
		}
		plink = pitem->data.pv;
		memcpy( & plink[ GNGRP_T_ATTR ] , pattr , pgraph->LinkAttrSize );
	}

	return 0;
}

static void * _get_linkattr_V1( gnGrpGraph_s * pgraph , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	gnInt32_t *			pnode;
	gnInt32_t *			ptonode;
	gnInt32_t *			plinkarea;
	gnInt32_t *			plink;
	gnTreeNode_s * 		pitem;
	int i;

	if ( pgraph->Flags & 0x1 )
	{
		if ( (pnode = _search_node_V1( pgraph , fnodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
			return NULL;
		}
		plinkarea = (gnInt32_t*)(pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET(pnode));

		for (
				i = 0 , plink = GNGRP_LINKAREA_LINKARRAY_PTR(plinkarea);
				i < GNGRP_LINKAREA_LINKCOUNT(plinkarea);
				i ++ , plink += GNGRP_T_WSIZE(pgraph->LinkAttrSize)
			)
		{
			ptonode = (gnInt32_t*)(pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink));
			if ( GNGRP_NODE_ID(ptonode) == tnodeid ) {
				break;
			}
		}

		if ( i == GNGRP_LINKAREA_LINKCOUNT(plinkarea) ) {
			pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
			return NULL;
		}	

		return & plink[ GNGRP_T_ATTR ];
	}
	else
	{
		if ( (pitem = gnTreeSearch( pgraph->pLinkTree , fnodeid )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_TreeSearchError;
			return NULL;
		}
		plink = pitem->data.pv;
		return & plink[ GNGRP_T_ATTR ];
	}

	return NULL;
}


static void _dump_node_V1( gnGrpGraph_s * pgraph , FILE * f , gnInt32_t * pnode )
{
	gnInt32_t * pfrom;
	gnInt32_t * pto;
	gnInt32_t * ptonode;
	int		i;

	if ( pnode[ GNGRP_C_STATUS ] & GNGRP_NF_FROM )
	{
		fprintf( f , "NODE-FROM: id:%ld offset:%ld ->\n",
				 pnode[ GNGRP_C_NODEID ] , pnode[ GNGRP_C_OFFSET ]
				 );

		pfrom = (gnInt32_t*)& pgraph->pLinkBuffer[ pnode[ GNGRP_C_OFFSET ] ];

		for( pto = & pfrom[ GNGRP_F_TOARR ] , i = 0 ; i < pfrom[ GNGRP_F_TOCNT ] ; i ++ )
		{
			ptonode = (gnInt32_t*)& pgraph->pNodeBuffer[ pto[ GNGRP_T_OFFSET ] ];
			fprintf( f , "\tARC-TO: id:%ld cost:%ld user:%ld\n",
					 ptonode[ GNGRP_C_NODEID ],
					 pto[ GNGRP_T_COST ],
					 pto[ GNGRP_T_USER ]);

			pto += GNGRP_T_WSIZE(pgraph->LinkAttrSize);
		}
	}

	if ( pnode[ GNGRP_C_STATUS ] & GNGRP_NF_TO )
	{
		fprintf( f , "NODE-TO: id:%ld offset:%ld\n",
				 pnode[ GNGRP_C_NODEID ] , pnode[ GNGRP_C_OFFSET ]
				 );
	}
}


static void _dump_head_V1( gnGrpGraph_s * pgraph , FILE * f )
{
	fprintf( f , "--\n" );
	fprintf( f , "GRAPH FILE VERSION %d\n" ,
			 pgraph->Version );
	fprintf( f , "BYTEORDER %s\n" ,
			 (pgraph->Endian == GNGRP_ENDIAN_LITTLE)?"Little Endian":"Big Endian" );
	fprintf( f , "NODE ATTR SIZE :  %ld\n" ,
			 pgraph->NodeAttrSize );
	fprintf( f , "LINK ATTR SIZE :  %ld\n" ,
			 pgraph->LinkAttrSize );
	fprintf( f , "COUNTERS :  %ld ARCS - %ld NODES : %ld FROM %ld TO\n" ,
			 pgraph->cArc , pgraph->cNode ,
			 pgraph->cFrom , pgraph->cTo );
	fprintf( f , "OPAQUE SETTINGS (16 user defined fields):\n" );
	fprintf( f , "%10ld %10ld %10ld %10ld\n",
			 pgraph->aOpaqueSet[ 0 ], pgraph->aOpaqueSet[ 1 ],
			 pgraph->aOpaqueSet[ 2 ], pgraph->aOpaqueSet[ 3 ] );
	fprintf( f , "%10ld %10ld %10ld %10ld\n",
			 pgraph->aOpaqueSet[ 4 ], pgraph->aOpaqueSet[ 5 ],
			 pgraph->aOpaqueSet[ 6 ], pgraph->aOpaqueSet[ 7 ] );
	fprintf( f , "%10ld %10ld %10ld %10ld\n",
			 pgraph->aOpaqueSet[ 8 ], pgraph->aOpaqueSet[ 9 ],
			 pgraph->aOpaqueSet[ 10 ], pgraph->aOpaqueSet[ 11 ] );
	fprintf( f , "%10ld %10ld %10ld %10ld\n",
			 pgraph->aOpaqueSet[ 12 ], pgraph->aOpaqueSet[ 13 ],
			 pgraph->aOpaqueSet[ 14 ], pgraph->aOpaqueSet[ 15 ] );
	fprintf( f , "--\n" );
}

static gnInt32_t * _parse_arc_values_V1(
									gnGrpGraph_s * 	pgraph ,
									gnInt32_t *		ppar ,
									gnInt32_t 		lFrom , 
									gnInt32_t 		lTo , 
									gnInt32_t 		lCost , 
									gnInt32_t 		lUser
									)
{
	ppar[ 0 ] = lFrom;
	ppar[ 1 ] = lTo;
	ppar[ 2 ] = lCost;
	ppar[ 3 ] = lUser;

	return ppar;
}

/*
 * SPR (Shortest Path Report) helpers
 */
static unsigned char * _mempush( unsigned char * pstack , long * istack , long size , void * pv )
{
	if ( *istack == 0 ) pstack = NULL;
	pstack = realloc( pstack , size * (1 + *istack) );
	if ( pstack == NULL ) return NULL;
	memcpy( & pstack[ (*istack) * size ] , pv , size );
	(*istack) ++;
	return pstack;
}

static unsigned char * _mempop( unsigned char * pstack , long * istack , long size )
{
	if ( *istack == 0 ) return NULL;
	return & pstack[ size * (--(*istack)) ];
}


/*
 * Dijkstra Shortest Path 
 */
static void * _dijkstra_V1	(
							gnGrpGraph_s * 	pgraph ,
							gnInt32_t 			from ,
							gnInt32_t 			to ,
							int (*clip)		(
#ifndef GNGRP_NEWCLIP
											gnGrpGraph_s * pgraph,
											gnInt32_t * pprevlink,
											gnInt32_t * pfromnode,
											gnInt32_t * plink,
											gnInt32_t * ptonode,
											gnInt32_t * pcost,
#else /* GNGRP_NEWCLIP */
											gnGrpGraph_s * ,
											gnGrpSPClipInput_s * ,
											gnGrpSPClipOutput_s * ,
#endif /* GNGRP_NEWCLIP */
											void *
											) ,
							void * 			pvcliparg
							)
{
	gnInt32_t *					pfrom;				/* pointer to the from node (pgraph->pNodeBuffer) */
	register gnInt32_t *		pfromnode;			/* temporary from pointer */
	register gnInt32_t *		ptonode;			/* temporary to pointer */
	register gnInt32_t *		plinkarea;			/* pointer to the link (pgraph->pLinkBuffer) */
	register gnInt32_t *		plink;				/* pointer to the to-links in link  */
	register gnInt32_t *		plink_prev;			/* pointer to the previous link in path */
	register gnInt32_t *		ptofound = NULL; 	/* to node found during calculation */
	register int				i;

	/*
	 * shortest path distance temporary min heap
	 */
	gnHeapData_u 				heapvalue;
	gnHeap_s					NodeHeap;
	gnHeapNode_s *				pheapnode;

	/*
	 * shortest path visited network
	 */
	void * 			pvVisited = NULL;	
	gnTreeNode_s * 	pVisitedItem;

	/*
	 * shortest path predecessor and distance network
	 */
	void * 			pvPredist = NULL;	
	gnTreeNode_s * 	pPredistItem;
	gnInt32_t *		pPredist;

	/*
	 * args to clip()
	 */
#ifndef GNGRP_NEWCLIP
	gnInt32_t 		nTmpCost;
#else /* GNGRP_NEWCLIP */
	gnGrpSPClipInput_s 	clipInput;
	gnGrpSPClipOutput_s	clipOutput;

#endif /* GNGRP_NEWCLIP */

	if ( ! (pgraph->Flags & 0x1) )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnNoFlatGraph;
		return NULL;
	}

	gnHeapInit( & NodeHeap );

	/* create temporary networks - The use of a predist network for predecessor and
	   distance has two important results: 1) allows us not having to reset the whole graph status
	   at each call; 2) use of a specific memory area for temporary (and possibly thread-conflicting)
	   states.
	*/
	if ( (pvVisited = gnTreeCreate( _node_free , NULL )) == NULL ) goto sp_error;
	if ( (pvPredist = gnTreeCreate( _node_free , NULL )) == NULL ) goto sp_error;

	if ( (pfrom = _search_node_V1( pgraph , from )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
		goto sp_error;
	}
	pfromnode = pfrom;

	if ( (ptonode = _search_node_V1( pgraph , to )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
		goto sp_error;
	}

	if ( ! (GNGRP_NODE_STATUS(pfromnode) & GNGRP_NF_FROM) )
	{
		pgraph->iErrno = GNGRP_ERR_BadLink;
		goto sp_error;
	}

	plinkarea = (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET(pfromnode) );

	/*
	 * we now mark a visited node by inserting it in the visited network
	 */
	pVisitedItem = __node(pvVisited , GNGRP_NODE_ID(pfromnode));
	if ( pVisitedItem == NULL ) goto sp_error;


	/*
	 * now we inspect all links departing from the starting from-node
	 * - at each loop 'plink' points to the link in the link buffer
	 * - we invoke the caller's clip() and eventually skip the link (clip() != 0)
	 * - we insert a item in the predist network to set actual predecessor link as -2
	 *   (there is no precedecessor at this stage) and actual distance from the starting node
	 *   (at this stage it equals the link's cost)
	 * - we insert a item in the node min-heap (sorted on node distance), storing the offset of the
	 *   link in the link buffer.
	 */
	for	(
		i = 0,  plink = GNGRP_LINKAREA_LINKARRAY_PTR(plinkarea) ;
		i < GNGRP_LINKAREA_LINKCOUNT(plinkarea) ;
		i ++,   plink += GNGRP_T_WSIZE(pgraph->LinkAttrSize)
		)
	{
		ptonode = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) );

		if ( ! (GNGRP_NODE_STATUS(ptonode) & GNGRP_NF_TO) )
		{
			pgraph->iErrno = GNGRP_ERR_BadLink;
			goto sp_error;
		}

		/*
		 * arc clipping : no previous link in path at first round
		 */
#ifndef GNGRP_NEWCLIP
		nTmpCost = plink[GNGRP_T_COST];
#else /* GNGRP_NEWCLIP */
		clipOutput.nLinkCost = GNGRP_LINK_COST(plink);
#endif /* GNGRP_NEWCLIP */

		if ( clip )
		{
#ifndef GNGRP_NEWCLIP
			if ( clip( pgraph , NULL , pfromnode , plink , ptonode , &nTmpCost , pvcliparg ) ) continue;
#else /* GNGRP_NEWCLIP */
			clipInput.pnPrevLink 	= NULL;
			clipInput.pnNodeFrom 	= pfromnode;
			clipInput.pnLink		= plink;
			clipInput.pnNodeTo		= ptonode;
			clipInput.nFromDistance = 0;

			if ( clip( pgraph , & clipInput , & clipOutput , pvcliparg ) ) continue;
#endif /* GNGRP_NEWCLIP */
		}

		/*
		 * Setup the predist for this node
		 */
		pPredistItem = __node( pvPredist, GNGRP_NODE_ID(ptonode) );
		if ( pPredistItem == NULL ) goto sp_error;

		if ( (pPredist = pPredistItem->data.pv) == NULL ) {
			if ( (pPredist = malloc( sizeof(gnInt32_t) * 3 )) == NULL ) goto sp_error;
			pPredistItem->data.pv = pPredist;
		}

		/*
		 * '-2' = no predecessor link: used for arcs departing from starting node
		 */
		pPredist[0] = -2;

		/*
		 * store actual distance
		 */
#ifndef GNGRP_NEWCLIP
		pPredist[1] = nTmpCost;
#else /* GNGRP_NEWCLIP */
		pPredist[1] = clipOutput.nLinkCost;
#endif /* GNGRP_NEWCLIP */

		/*
		 * store real cost
		 */
#ifndef GNGRP_NEWCLIP
		pPredist[2] = nTmpCost;
#else /* GNGRP_NEWCLIP */
		pPredist[2] = clipOutput.nLinkCost;
#endif /* GNGRP_NEWCLIP */


		/* store the offset in the link buffer into the heap item */
		heapvalue.l = (gnInt32_t)plink - (gnInt32_t)pgraph->pLinkBuffer;

		if ( gnHeapInsertMin( & NodeHeap, pPredist[1] , heapvalue ) < 0 )
		{
			pgraph->iErrno = GNGRP_ERR_HeapError;
			goto sp_error;
		}
	}

	/*
	 * Now we begin extracting nodes from the min-heap. Each node extracted is
	 * the one that is actually closest to the SP starting point 'from'.
	 */
	while( (pheapnode = gnHeapExtractMin( & NodeHeap )) != NULL )
	{
		gnInt32_t fromPred , fromDist;
		/*
		 * recover the stored link pointer
		 */
		plink   	= (gnInt32_t*) (pgraph->pLinkBuffer + pheapnode->value.l);
		/*
		 * the new relative from-node is the to-node of the link
		 */
		pfromnode 	= (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) );
		/*
		 * get the linkarea of the new from-node
		 */
		plinkarea 	= (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET(pfromnode) );

		/*
		 * save actual link for later clip()
		 */
		plink_prev = plink;


		/*
		 * Dijkstra algorithm ends when the destination node 'to' is extracted from
		 * the min distance heap, that means: no other path exist in the network giving
		 * a shortest output.
		 * If this happens we jump to the epilogue in order to build a path report and return.
		 */
		if ( GNGRP_NODE_ID(pfromnode) == to )
		{
			ptofound = plink;
			goto to_found;
		}

		/*
		 * If the from-node is not marked as having departing links, then we are into a
		 * blind alley. Just give up this direction and continue looping.
		 */
		if ( ! (GNGRP_NODE_STATUS(pfromnode) & GNGRP_NF_FROM) )  continue;

		/*
		 * We do not want to explore twice the same node as a relative starting point,
		 * that's the meaning of 'visited'. We mark actual from node as 'visited' by
		 * inserting it into the visited-network. If we find actual node in the network
		 * we just give up and continue looping. Otherwise we add actual node to the network.
		 */
		pVisitedItem = gnTreeSearch( pvVisited , GNGRP_NODE_ID(pfromnode) );
		if ( pVisitedItem ) { /* found */
			continue;
		}
		/* add */
		pVisitedItem = __node( pvVisited, GNGRP_NODE_ID(pfromnode) );
		if ( pVisitedItem == NULL ) {
			goto sp_error;
		}

		/*
		 * Recover the predecessor-link-in-path and distance from the predist network
		 */
		if ( (pPredistItem = gnTreeSearch( pvPredist, GNGRP_NODE_ID(pfromnode) )) == NULL ) goto sp_error;
		if ( (pPredist = pPredistItem->data.pv) == NULL ) goto sp_error;
		fromPred = pPredist[0];
		fromDist = pPredist[1];

		/*
		 * Loop on departing links
		 */
		for (
			i = 0,  plink = GNGRP_LINKAREA_LINKARRAY_PTR(plinkarea), i = 0;
			i < GNGRP_LINKAREA_LINKCOUNT(plinkarea);
			i ++,   plink += GNGRP_T_WSIZE(pgraph->LinkAttrSize)
			)
		{
			ptonode = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) );
		
			if ( ! (GNGRP_NODE_STATUS(ptonode) & GNGRP_NF_TO) )
			{
				pgraph->iErrno = GNGRP_ERR_BadLink;
				goto sp_error;
			}
		
			/*
			 * arc clipping : we now have previous link and from distance
			 */
#ifndef GNGRP_NEWCLIP
			nTmpCost = plink[GNGRP_T_COST];
#else /* GNGRP_NEWCLIP */
			clipOutput.nLinkCost = GNGRP_LINK_COST(plink);
#endif /* GNGRP_NEWCLIP */

			if ( clip )
			{
#ifndef GNGRP_NEWCLIP
				if ( clip( pgraph , plink_prev , pfromnode , plink , ptonode , &nTmpCost , pvcliparg ) ) continue;
#else /* GNGRP_NEWCLIP */
				clipInput.pnPrevLink 	= plink_prev;
				clipInput.pnNodeFrom 	= pfromnode;
				clipInput.pnLink		= plink;
				clipInput.pnNodeTo		= ptonode;
				clipInput.nFromDistance = fromDist;

				if ( clip( pgraph , & clipInput , & clipOutput , pvcliparg ) ) continue;
#endif /* GNGRP_NEWCLIP */
			}

			if ( (pPredistItem = gnTreeSearch( pvPredist, GNGRP_NODE_ID(ptonode) )) == NULL ) {
				if ( (pPredistItem = __node( pvPredist, GNGRP_NODE_ID(ptonode) )) == NULL ) goto sp_error;
				if ( (pPredist = malloc( sizeof(gnInt32_t) * 3 )) == NULL ) goto sp_error;
				pPredistItem->data.pv = pPredist;
			}
			else {
				pPredist = pPredistItem->data.pv;
#ifndef GNGRP_NEWCLIP
				if ( pPredist[1] <= fromDist + nTmpCost ) {
#else /* GNGRP_NEWCLIP */
				if ( pPredist[1] <= fromDist + clipOutput.nLinkCost ) {
#endif /* GNGRP_NEWCLIP */
					continue;
				}
			}

			pPredist[0] = pheapnode->value.l;
#ifndef GNGRP_NEWCLIP
			pPredist[1] = fromDist + nTmpCost;
			pPredist[2] = nTmpCost;

#else /* GNGRP_NEWCLIP */
			pPredist[1] = fromDist + clipOutput.nLinkCost;
			pPredist[2] = clipOutput.nLinkCost;
#endif /* GNGRP_NEWCLIP */

			heapvalue.l = (gnInt32_t)plink - (gnInt32_t)pgraph->pLinkBuffer;

			if ( gnHeapInsertMin( & NodeHeap, pPredist[1] , heapvalue ) < 0 )
			{
				pgraph->iErrno = GNGRP_ERR_HeapError;
				goto sp_error;
			}
		}
	}

sp_error:
	gnHeapFree( & NodeHeap );
	if ( pvVisited ) gnTreeDestroy( pvVisited );
	if ( pvPredist ) gnTreeDestroy( pvPredist );
	return NULL; /* if pgraph->iErrno == 0 then path not found, else report an error condition */

to_found: /* path found - build a shortest path report */

	gnHeapFree( & NodeHeap );
	if ( pvVisited ) gnTreeDestroy( pvVisited );


	/*
	 * EPILOGUE ...
	 */
	{
		long 				istack = 0;
		unsigned char * 	pstack = NULL;
		unsigned char * 	ppop;
		gnInt32_t *			plinkto_pred;
		gnInt32_t *			ptonode_pred;
		gnGrpSPReport_s *	pSPR = NULL;
		gnGrpSPArc_s			arc;

		if ( (pSPR = malloc( sizeof( gnGrpSPReport_s ) )) == NULL )
		{
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			goto spr_error;
		}
		memset( pSPR, 0, sizeof( gnGrpSPReport_s ) );

		/*
		 * first loop: push arcs' stack in reverse order by predecessor navigation
		 */
		for(
			plink = ptofound ,
			ptonode = (gnInt32_t*)(pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) ) ;

			(pPredistItem = gnTreeSearch( pvPredist , GNGRP_NODE_ID(ptonode) )) != NULL ;

			plink = plinkto_pred ,
			ptonode = (gnInt32_t*)(pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) )
			)
		{
			pPredist = pPredistItem->data.pv;			

			if ( pPredist[0] == -2 )
			{
				ptonode_pred = pfrom;
			}
			else
			{
				plinkto_pred = (gnInt32_t*)(pgraph->pLinkBuffer + pPredist[0]);
				ptonode_pred = (gnInt32_t*)(pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plinkto_pred) ) ;
			}

			arc.From = GNGRP_C_ALLOC( pgraph->NodeAttrSize );
			arc.To   = GNGRP_C_ALLOC( pgraph->NodeAttrSize );
			arc.Link = GNGRP_T_ALLOC( pgraph->LinkAttrSize );
			if 	(
				arc.From == NULL ||
				arc.To   == NULL ||
				arc.Link == NULL
				)
			{
				if ( arc.From ) free( arc.From );
				if ( arc.To ) free( arc.To );
				if ( arc.Link ) free( arc.Link );
				goto spr_error;
			}
			memcpy( arc.From , ptonode_pred , GNGRP_C_SIZEOF( pgraph->NodeAttrSize ) );
			memcpy( arc.To   , ptonode      , GNGRP_C_SIZEOF( pgraph->NodeAttrSize ) );
			memcpy( arc.Link , plink        , GNGRP_T_SIZEOF( pgraph->LinkAttrSize ) );
			arc.Distance = pPredist[1];
			/*
			 * fix the link cost with real cost
			 */
			arc.Link[ GNGRP_T_COST ] = pPredist[2];

			if ( (pstack = _mempush( pstack , & istack , sizeof( gnGrpSPArc_s ) , & arc )) == NULL )
			{
				pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
				goto spr_error;
			}

			if ( pPredist[0] == -2 ) break;
		}

		pSPR->cArc = istack;

		if ( (pSPR->pArc = malloc( sizeof( gnGrpSPArc_s ) * pSPR->cArc )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			goto spr_error;
		}

		pSPR->distance = 0;
		for( i = 0 ; (ppop = _mempop( pstack , & istack , sizeof( gnGrpSPArc_s ) )) != NULL ; i ++ ) {
			memcpy( & pSPR->pArc[ i ] , ppop , sizeof( gnGrpSPArc_s ) );
			pSPR->distance += GNGRP_LINK_COST(pSPR->pArc[i].Link);
		}
		pSPR->from = from;
		pSPR->to = to;
		if ( pvPredist ) gnTreeDestroy( pvPredist );
		if ( pstack ) free( pstack );
		return pSPR;

spr_error:
		if ( pvPredist ) gnTreeDestroy( pvPredist );
		if ( pstack ) free( pstack );
		if ( pSPR ) gnGrpFreeSPReport( pgraph , pSPR );
		return NULL;
	}
}

static int _release_V1( gnGrpGraph_s * pgraph )
{
	pgraph->iErrno = 0;

	if ( pgraph->pNodeTree ) gnTreeDestroy( pgraph->pNodeTree );
	if ( pgraph->pLinkTree ) gnTreeDestroy( pgraph->pLinkTree );

	gnHeapFree( & pgraph->NodeHeap );

	if ( pgraph->pNodeBuffer ) free( pgraph->pNodeBuffer );
	if ( pgraph->pLinkBuffer ) free( pgraph->pLinkBuffer );

	return 0;
}


static int _write_V1( gnGrpGraph_s * pgraph , int fd )
{
	long nret , cnt , tot;	

	pgraph->iErrno = 0;

	if ( write( fd , & pgraph->Version , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->Endian , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->NodeAttrSize  , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->LinkAttrSize  , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	for ( cnt = 0 ; cnt < 16 ; cnt ++ )
	{
		if ( write( fd , & pgraph->aOpaqueSet[ cnt ] , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
		{
			pgraph->iErrno = GNGRP_ERR_Write;
			return -1;
		}
	}

	if ( write( fd , & pgraph->cNode , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->cFrom , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->cTo , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->cArc , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->iNodeBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	if ( write( fd , & pgraph->iLinkBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -1;
	}

	for( tot = 0 , cnt = pgraph->iNodeBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = write( fd , & pgraph->pNodeBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			pgraph->iErrno = GNGRP_ERR_Write;
			return -1;
		}
	}

	for( tot = 0 , cnt = pgraph->iLinkBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = write( fd , & pgraph->pLinkBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			pgraph->iErrno = GNGRP_ERR_Write;
			return -1;
		}
	}

	return 0;
}


static int _read_V1( gnGrpGraph_s * pgraph , int fd )
{
	long 		nret , cnt , tot;	
	gnByte_t 	Version , Endian;
	gnInt32_t	NodeAttrSize , LinkAttrSize;

	pgraph->iErrno = 0;

	if ( read( fd , & Version , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & Endian , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

#ifdef GN_ENDIAN_BIG
	if ( Endian != GNGRP_ENDIAN_BIG )
	{
		pgraph->iErrno = GNGRP_ERR_UnknownByteOrder;
		return -1;
	}
#else
	if ( Endian != GNGRP_ENDIAN_LITTLE )
	{
		pgraph->iErrno = GNGRP_ERR_UnknownByteOrder;
		return -1;
	}
#endif

	if ( read( fd , & NodeAttrSize , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & LinkAttrSize , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( gnGrpInitialize( pgraph, Version, NodeAttrSize, LinkAttrSize, NULL ) < 0 )
	{
		return -1;
	}

	for ( cnt = 0 ; cnt < 16 ; cnt ++ )
	{
		if ( (nret=read( fd , & pgraph->aOpaqueSet[ cnt ] , sizeof( gnInt32_t ) )) != sizeof( gnInt32_t ) )
		{
			pgraph->iErrno = GNGRP_ERR_Read;
			return -1;
		}
	}

	if ( read( fd , & pgraph->cNode , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & pgraph->cFrom , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & pgraph->cTo , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & pgraph->cArc , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & pgraph->iNodeBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( read( fd , & pgraph->iLinkBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -1;
	}

	if ( (pgraph->pNodeBuffer = malloc( pgraph->iNodeBuffer )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -1;
	}

	if ( (pgraph->pLinkBuffer = malloc( pgraph->iLinkBuffer )) == NULL )
	{
		free( pgraph->pNodeBuffer );
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -1;
	}

	for( tot = 0 , cnt = pgraph->iNodeBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = read( fd , & pgraph->pNodeBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			free( pgraph->pNodeBuffer );
			free( pgraph->pLinkBuffer );
			pgraph->iErrno = GNGRP_ERR_Read;
			return -1;
		}
	}

	for( tot = 0 , cnt = pgraph->iLinkBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = read( fd , & pgraph->pLinkBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			free( pgraph->pNodeBuffer );
			free( pgraph->pLinkBuffer );
			pgraph->iErrno = GNGRP_ERR_Read;
			return -1;
		}
	}

	pgraph->Flags |= 0x1; /* flattened */

	return 0;
}


/*
 * version 1 calls
 */
static gnGrpMethods_s _v1_methods =
{
	_release_V1,
	_write_V1,
	_read_V1,
	_parse_arc_values_V1,
	_add_link_V1,
	_search_node_V1,
	_unflatten_V1,
	_flatten_V1,
	_set_nodeattr_V1,
	_get_nodeattr_V1,
	_set_linkattr_V1,
	_get_linkattr_V1,
	_dump_head_V1,
	_dump_node_V1,
	_dijkstra_V1
};

static int _init_V1(gnGrpGraph_s * pgraph, gnInt32_t NodeAttrSize, gnInt32_t LinkAttrSize, gnInt32_t * pOpaqueSet)
{
	if ( pgraph == NULL ) return -1;

	memset( pgraph , 0 , sizeof( gnGrpGraph_s ) );

	gnHeapInit( & pgraph->NodeHeap );

	/*
	 * round attr size to the upper multiple of gnInt32_t size
	 */
	if ( NodeAttrSize % sizeof(gnInt32_t) ) NodeAttrSize += ( sizeof(gnInt32_t) - (NodeAttrSize % sizeof(gnInt32_t)) );
	if ( LinkAttrSize % sizeof(gnInt32_t) ) LinkAttrSize += ( sizeof(gnInt32_t) - (LinkAttrSize % sizeof(gnInt32_t)) );

	pgraph->Version  = 1;
	pgraph->NodeAttrSize = NodeAttrSize;
	pgraph->LinkAttrSize = LinkAttrSize;

	if ( pOpaqueSet ) memcpy( & pgraph->aOpaqueSet , pOpaqueSet , sizeof( gnInt32_t ) * 16 );
	
#ifdef GN_ENDIAN_BIG
	pgraph->Endian = GNGRP_ENDIAN_BIG;
#else
	pgraph->Endian = GNGRP_ENDIAN_LITTLE;
#endif

	pgraph->pMethods = & _v1_methods;

	return 0;
}

/*
 * THE PUBLIC API BEGINS HERE
 */

/*
 * INITIALIZE A NEW GRAPH INSTANCE
 */

int gnGrpInitialize(gnGrpGraph_s * pgraph, gnByte_t Version, gnInt32_t NodeAttrSize, gnInt32_t LinkAttrSize, gnInt32_t * pOpaqueSet)
{
	switch( Version )
	{
	case 1:
		return _init_V1( pgraph, NodeAttrSize , LinkAttrSize , pOpaqueSet );
	}
	return -(GNGRP_ERR_VersionNotSupported);
}

int gnGrpRelease( gnGrpGraph_s * pgraph )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->release )
	{
		return pgraph->pMethods->release( pgraph );
	}
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -1;
}

int gnGrpUnflatten( gnGrpGraph_s * pgraph )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->unflatten )
		return pgraph->pMethods->unflatten( pgraph );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -1;
}


int gnGrpFlatten( gnGrpGraph_s * pgraph )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->flatten )
		return pgraph->pMethods->flatten( pgraph );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -1;
}


gnInt32_t * gnGrpSearchNode( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->searchnode )
		return pgraph->pMethods->searchnode( pgraph , nodeid );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return NULL;
}


int gnGrpAddLink	(
				gnGrpGraph_s * 	pgraph ,
				gnInt32_t 		lFrom ,
				gnInt32_t 		lTo ,
				gnInt32_t 		lCost ,
				gnInt32_t 		lUser ,
				void *			pvFnodeAttr ,
				void *			pvTnodeAttr ,
				void *			pvLinkAttr
				)
{
	gnInt32_t	par[ 16 ];
	int 		nret;

#ifdef GNGRP_STATS
	clock_t clk;
	clk = clock();
	pgraph->cAddLink ++;
#endif
	
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->parsearcvalues )
	{
		pgraph->pMethods->parsearcvalues( pgraph , par , lFrom , lTo , lCost , lUser );
		if ( pgraph->pMethods->addlink )
			nret = pgraph->pMethods->addlink( pgraph , par , pvFnodeAttr , pvTnodeAttr , pvLinkAttr );
	}
	else {
		pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
		nret = -1;
	}

#ifdef GNGRP_STATS
	pgraph->clkAddLink += clock() - clk;
#endif
	return nret;
}

int gnGrpSetNodeAttr( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t nodeid )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->setnodeattr)
		return pgraph->pMethods->setnodeattr( pgraph , pattr , nodeid );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -1;
}

void * gnGrpGetNodeAttr( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->getnodeattr)
		return pgraph->pMethods->getnodeattr( pgraph , nodeid );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return NULL;
}

int gnGrpSetLinkAttr( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->setlinkattr)
		return pgraph->pMethods->setlinkattr( pgraph , pattr , fnodeid , tnodeid );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -1;
}

void * gnGrpGetLinkAttr( gnGrpGraph_s * pgraph , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->getlinkattr)
		return pgraph->pMethods->getlinkattr( pgraph , fnodeid , tnodeid );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return NULL;
}

int gnGrpWrite( gnGrpGraph_s * pgraph, int fd )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->write )
		return pgraph->pMethods->write( pgraph , fd );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -1;
}

/*
 * When we'll have more versions we'll read the first graph
 * byte reporting version number and switch on its value
 */
int gnGrpRead( gnGrpGraph_s * pgraph, int fd )
{
	return _read_V1( pgraph, fd );
}


void gnGrpDumpHead( gnGrpGraph_s * pgraph , FILE * f )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->dumphead)
		pgraph->pMethods->dumphead( pgraph , f );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
}

void gnGrpDumpNode( gnGrpGraph_s * pgraph , FILE * f , gnInt32_t * pnode )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->dumpnode)
		pgraph->pMethods->dumpnode( pgraph , f , pnode );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
}


gnGrpSPReport_s * gnGrpShortestPath	(
								 	gnGrpGraph_s * 	pgraph ,
								 	gnInt32_t 		from ,
								 	gnInt32_t 		to ,
								 	int (*clip)(
											 	gnGrpGraph_s *, /* graph pointer */
#ifndef GNGRP_NEWCLIP
											 	gnInt32_t *,	/* previous link pointer */
											 	gnInt32_t *,	/* from node pointer */
											 	gnInt32_t *,	/* this link pointer */
											 	gnInt32_t *,	/* to node pointer */
											 	gnInt32_t *,	/* real cost pointer */
#else /* GNGRP_NEWCLIP */
											 	gnGrpSPClipInput_s *,
											 	gnGrpSPClipOutput_s *,
#endif /* GNGRP_NEWCLIP */
											 	void  *			/* caller's context pointer */
											 	) ,
								 	void * 		pvcliparg		/* caller's context pointer (passed back to clip)*/
								 	)
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->shortestpath)
		return pgraph->pMethods->shortestpath( pgraph , from , to , clip , pvcliparg );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return NULL;
}

void gnGrpFreeSPReport( gnGrpGraph_s * pgraph , gnGrpSPReport_s * pSPReport )
{
	int iArc;
	if ( pSPReport )
	{
		if ( pSPReport->pArc )
		{
			for ( iArc = 0 ; iArc < pSPReport->cArc ; iArc ++ ) {
				if ( pSPReport->pArc[iArc].From ) free( pSPReport->pArc[iArc].From );
				if ( pSPReport->pArc[iArc].To ) free( pSPReport->pArc[iArc].To );
				if ( pSPReport->pArc[iArc].Link ) free( pSPReport->pArc[iArc].Link );
			}
			free( pSPReport->pArc );
		}
		free( pSPReport );
	}
}

void gnGrpSetOpaque( gnGrpGraph_s * pgraph, gnInt32_t * pOpaque )
{
	memcpy( pgraph->aOpaqueSet , pOpaque , sizeof( gnInt32_t ) * 16 );
}

gnInt32_t * gnGrpGetOpaque( gnGrpGraph_s * pgraph )
{
	return pgraph->aOpaqueSet;
}


int gnGrpErrno( gnGrpGraph_s * pgraph )
{
	return pgraph->iErrno;
}

char * gnGrpStrerror( gnGrpGraph_s * pgraph )
{
	switch( pgraph->iErrno )
	{
	case GNGRP_ERR_BadVersion:
		return "Bad Version";
	case GNGRP_ERR_BadNodeType:
		return "Bad Node Type";
	case GNGRP_ERR_MemoryExhausted:
		return "Memory Exhausted";
	case GNGRP_ERR_HeapError:
		return "Heap Error";
	case GNGRP_ERR_UndefinedMethod:
		return "Undefined Method";
	case GNGRP_ERR_Write:
		return "Write";
	case GNGRP_ERR_Read:
		return "Read";
	case GNGRP_ERR_NotSupported:
		return "Not Supported";
	case GNGRP_ERR_UnknownByteOrder:
		return "Unknown Byte Order";
	case GNGRP_ERR_NodeNotFound:
		return "Node Not Found";
	case GNGRP_ERR_FromNodeNotFound:
		return "From Node Not Found";
	case GNGRP_ERR_ToNodeNotFound:
		return "To Node Not Found";
	case GNGRP_ERR_BadLink:
		return "Bad Link";
	case GNGRP_ERR_BadOnFlatGraph:
		return "Bad Request On Flat Graph";
	case GNGRP_ERR_BadOnNoFlatGraph:
		return "Bad Request On No Flat Graph";
	case GNGRP_ERR_TreeSearchError:
		return "Tree Search Error";
	case GNGRP_ERR_UnexpectedNullPointer:
		return "Unexpected Null Pointer";
	case GNGRP_ERR_VersionNotSupported:
		return "Version Not Supported";
	}

	return "unknown graph error code";
}
