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
	pgraph->clkNodeTree = 0;
	pgraph->clkNodeHeap = 0;
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
		if ( pnode->data2.pv ) {
			free( pnode->data2.pv );
		}
		free( pnode );
	}
	return 0;
}

static gnTreeNode_s * __node( gnTreeNode_s * ptree , gnInt32_t nodeid )
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

/*
 * Add link can be performed on TREE state graph. If the state is FLAT
 * return BadOnFlatGraph error.
 */
static int _add_link_V1  (
						gnGrpGraph_s * 	pgraph ,
						gnInt32_t 		lFrom,
						gnInt32_t 		lTo,
						gnInt32_t 		lCost,
						gnInt32_t 		lUser,
						void * pvFnodeAttr ,	
						void * pvTnodeAttr ,	
						void * pvLinkAttr
						)
{
	gnInt32_t *		pfrom;
	gnInt32_t *		pto;
	gnInt32_t *		plinkarea;
	gnInt32_t *		plink;
	gnTreeNode_s * 	pFromNodeItem;
	gnTreeNode_s * 	pToNodeItem;
	gnHeapData_u	HeapData;
	
	if ( pgraph->Flags & 0x1 )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnFlatGraph;
		return -pgraph->iErrno;
	}

	if ( pgraph->pNodeTree == NULL ) pgraph->pNodeTree = gnTreeCreate( _node_free , pgraph );
	if ( pgraph->pNodeTree == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraph->iErrno;
	}

#ifdef GNGRP_STATS
		{
		clock_t clk = clock();
#endif

		if (
			(pFromNodeItem = __node( pgraph->pNodeTree , lFrom )) == NULL
			||
			(pToNodeItem   = __node( pgraph->pNodeTree , lTo   )) == NULL
		   )
		{
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}

#ifdef GNGRP_STATS
		pgraph->clkNodeTree += clock() - clk;
		pgraph->cNodeTree ++;
		pgraph->cNodeTree ++;
		}
#endif

	if ( pFromNodeItem->data.pv == NULL )
	{
		if ( (pfrom = GNGRP_NODE_ALLOC( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -1;
		}
		GNGRP_NODE_STATUS(pfrom) = 0;
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
		if ( (pto = GNGRP_NODE_ALLOC( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		GNGRP_NODE_STATUS(pto) = 0;
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


	GNGRP_NODE_STATUS(pfrom) |= GNGRP_NS_FROM;
	GNGRP_NODE_STATUS(pto)   |= GNGRP_NS_TO;

	GNGRP_NODE_ID(pfrom) = lFrom;
	GNGRP_NODE_ID(pto)   = lTo;

	GNGRP_NODE_LINKAREA_OFFSET(pfrom) = -1;
	GNGRP_NODE_LINKAREA_OFFSET(pto)   = -1;

	if ( pvFnodeAttr && pgraph->NodeAttrSize ) {
		memcpy( GNGRP_NODE_ATTR_PTR(pfrom), pvFnodeAttr, pgraph->NodeAttrSize );
	}

	if ( pvTnodeAttr && pgraph->NodeAttrSize ) {
		memcpy( GNGRP_NODE_ATTR_PTR(pto), pvTnodeAttr, pgraph->NodeAttrSize );
	}

	if ( pFromNodeItem->data2.pv == NULL )
	{
		pFromNodeItem->data2.pv = GNGRP_LINKAREA_ALLOC( 1 , pgraph->LinkAttrSize );
		if ( pFromNodeItem->data2.pv == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		plinkarea = pFromNodeItem->data2.pv;
		GNGRP_LINKAREA_LINKCOUNT(plinkarea) = 0;
	}
	else
	{
		plinkarea = pFromNodeItem->data2.pv;
		pFromNodeItem->data2.pv = GNGRP_LINKAREA_REALLOC( pFromNodeItem->data2.pv ,
												GNGRP_LINKAREA_LINKCOUNT(plinkarea) + 1 ,
												pgraph->LinkAttrSize );
		if ( pFromNodeItem->data2.pv == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		plinkarea = pFromNodeItem->data2.pv;
	}

	plink = GNGRP_LINKAREA_LINKARRAY_PTR(plinkarea) +
			GNGRP_LINKAREA_LINKCOUNT(plinkarea) * GNGRP_LINK_WSIZE(pgraph->LinkAttrSize);

	GNGRP_LINK_TONODE_OFFSET(plink)	= lTo;		/* will be an offset after flattening */
	GNGRP_LINK_COST(plink)          = lCost;
	GNGRP_LINK_USER(plink)          = lUser;

	if ( pvLinkAttr && pgraph->LinkAttrSize ) {
		memcpy( GNGRP_LINK_ATTR_PTR(plink), pvLinkAttr, pgraph->LinkAttrSize );
	}

	GNGRP_LINKAREA_LINKCOUNT(plinkarea) ++;

	return 0;
}



/*
 * if graph is FLAT perform a binary search in the pNodeBuffer
 * if graph is TREE search the node in the pNodeTree avl
 */
static gnInt32_t * _get_node_V1( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	register gnInt32_t 		top;	/* top of table */
	register gnInt32_t 		pos;	/* current position to compare */
	register gnInt32_t 		bot;	/* bottom of table */
	register gnInt32_t *	pref;
	register int			cwords; /* size of a node in words of 32 bit */
	register gnTreeNode_s * ptreenode;
			 gnInt32_t		id;

	if ( pgraph->Flags & 0x1 ) {
		cwords = GNGRP_NODE_WSIZE(pgraph->NodeAttrSize);
		bot    = pgraph->iNodeBuffer / GNGRP_NODE_SIZEOF(pgraph->NodeAttrSize);
		top    = 0;
		pos    = 0;
		pref   = (gnInt32_t*)pgraph->pNodeBuffer;

		/* perform a binary search
	 	*/
		while( top != bot ) {
			pos = top + (bot - top) / 2;
			id = GNGRP_NODE_ID(& pref[pos * cwords]);
			if ( id == nodeid ) {
				break;
			}
			else if ( nodeid < id ) {
				bot = pos;
			}
			else if ( nodeid > id ) {
				top = pos + 1;
			}
		}
		if ( top == bot ) {
			pgraph->iErrno = GNGRP_ERR_NodeNotFound;
			return NULL;
		}
		return & pref[ pos * cwords ];
	}
	else {
		ptreenode = gnTreeSearch( pgraph->pNodeTree , nodeid );
		if ( ptreenode && ptreenode->data.pv ) {
			return ptreenode->data.pv;
		}
		pgraph->iErrno = GNGRP_ERR_NodeNotFound;
		return NULL;
	}
}

/*
 * if graph is FLAT retrieve the link area from the pLinkBuffer
 * if graph is TREE retrieve the node from the pNodeTree avl and return data2.pv component
 */
static gnInt32_t * _get_linkarea_V1( gnGrpGraph_s * pgraph , gnInt32_t * pnode )
{
	gnTreeNode_s * 	ptreenode;
	gnInt32_t * 	plinkarea;

	if ( pnode == NULL ) {
		pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		return NULL;
	}
	if ( pgraph->Flags & 0x1 ) {
		plinkarea = (gnInt32_t*)(pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET(pnode));
		return plinkarea;
	}
	else {
		ptreenode = gnTreeSearch( pgraph->pNodeTree , GNGRP_NODE_ID(pnode) );
		if ( ptreenode && ptreenode->data2.pv ) {
			return ptreenode->data2.pv;
		}
		pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		return NULL;
	}
}

/*
 * The GNGRP_LINK_TONODE_OFFSET() of a link reports a different value depending on
 * the state of the graph:
 * FLAT - the offset of the to-node in the pNodeBuffer
 * TREE - the to-node id
 */
static gnInt32_t * _get_link_V1( gnGrpGraph_s * pgraph , gnInt32_t * pfromnode , gnInt32_t tonodeid )
{
	gnInt32_t * 	plinkarea;
	gnInt32_t * 	plink;
	gnInt32_t * 	ptonode;
	int				ilink;

	plinkarea = _get_linkarea_V1( pgraph , pfromnode );
	if ( plinkarea == NULL ) return NULL;
	for ( ilink = 0 ; ilink < GNGRP_LINKAREA_LINKCOUNT(plinkarea) ; ilink ++ ) {
		plink = GNGRP_LINKAREA_LINK_PTR(plinkarea , ilink , pgraph->LinkAttrSize);
		if ( pgraph->Flags & 0x1 ) {
			ptonode = (gnInt32_t*)(pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink));
			if ( GNGRP_NODE_ID(ptonode) == tonodeid ) return plink;
		}
		else {
			if ( GNGRP_LINK_TONODE_OFFSET(plink) == tonodeid ) return plink;
		}
	}
	pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
	return NULL;
}

static int _unflatten_V1( gnGrpGraph_s * pgraph )
{
	register gnInt32_t *		pnode;
	register gnInt32_t *		pnodeto;
	register gnInt32_t *		plinkarea;
	register gnInt32_t *		plinkto;
	register int				k;
	int							nret;

	if ( ! (pgraph->Flags & 0x1) )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnTreeGraph;
		return -pgraph->iErrno;
	}

	/*
	 * unflag it now to avoid _add_link_V1() failure
	 */
	pgraph->Flags &= ~0x1;

	GNGRP_FOREACH_NODE(pgraph,pnode)
	{
		if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_FROM )
		{
			plinkarea = (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET(pnode));

			for	(
				k = 0 , plinkto = GNGRP_LINKAREA_LINK_PTR(plinkarea, 0, pgraph->LinkAttrSize) ;
				k < GNGRP_LINKAREA_LINKCOUNT(plinkarea) ;
				k ++ , plinkto = GNGRP_LINKAREA_LINK_PTR(plinkarea, k, pgraph->LinkAttrSize)
				)
			{
				pnodeto = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plinkto));

				nret = _add_link_V1(
							pgraph ,
							GNGRP_NODE_ID(pnode),
							GNGRP_NODE_ID(pnodeto),
							GNGRP_LINK_COST(plinkto),
							GNGRP_LINK_USER(plinkto),
							GNGRP_NODE_ATTR_PTR(pnode),
							GNGRP_NODE_ATTR_PTR(pnodeto),
							GNGRP_LINK_ATTR_PTR(plinkto)
							);

				if ( nret < 0 ) {
					if ( pgraph->pNodeTree ) gnTreeDestroy( pgraph->pNodeTree );
					pgraph->pNodeTree = NULL;
					gnHeapFree( & pgraph->NodeHeap );
					pgraph->Flags |= 0x1; /* keep it flat */
					return nret;
				}
			}
		}
	}

	/* move away flat-state data
	 */
	free( pgraph->pNodeBuffer );
	free( pgraph->pLinkBuffer );
	pgraph->pNodeBuffer = NULL;
	pgraph->pLinkBuffer = NULL;
	return 0;
}


static int _flatten_V1( gnGrpGraph_s * pgraph )
{
	register gnHeapNode_s * pheapnode;
	register gnTreeNode_s * ptreenode;
	register int 			i;
	register gnInt32_t *	pnode;
	register gnInt32_t *	plinkarea;
	register gnInt32_t *	pflat1;
	register gnInt32_t *	pflat2;
	register gnInt32_t *	pflat3;


	if ( pgraph->Flags & 0x1 )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnFlatGraph;
		return -pgraph->iErrno;
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
			if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_FROM )
			{
				pgraph->cFrom ++;

				if ( (ptreenode = gnTreeSearch( pgraph->pNodeTree , GNGRP_NODE_ID(pnode) )) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_TreeSearchError;
					return -pgraph->iErrno;
				}

				if ( (plinkarea = ptreenode->data2.pv) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
					return -pgraph->iErrno;
				}

				pgraph->pLinkBuffer = realloc(
					 pgraph->pLinkBuffer , 
					 pgraph->iLinkBuffer + GNGRP_LINKAREA_SIZEOF(GNGRP_LINKAREA_LINKCOUNT(plinkarea), pgraph->LinkAttrSize)
					 );

				if ( pgraph->pLinkBuffer == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
					return -pgraph->iErrno;
				}

				memcpy(
						pgraph->pLinkBuffer + pgraph->iLinkBuffer ,
						plinkarea ,
						GNGRP_LINKAREA_SIZEOF(GNGRP_LINKAREA_LINKCOUNT(plinkarea), pgraph->LinkAttrSize)
						);

				GNGRP_NODE_LINKAREA_OFFSET(pnode) = pgraph->iLinkBuffer;

				pgraph->iLinkBuffer += GNGRP_LINKAREA_SIZEOF(GNGRP_LINKAREA_LINKCOUNT(plinkarea), pgraph->LinkAttrSize);
			}

			if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_TO )
			{
				pgraph->cTo ++;
			}

			pgraph->cNode ++;

			pgraph->pNodeBuffer = realloc(pgraph->pNodeBuffer, pgraph->iNodeBuffer + GNGRP_NODE_SIZEOF(pgraph->NodeAttrSize));

			if ( pgraph->pNodeBuffer == NULL )
			{
				pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
				return -pgraph->iErrno;
			}

			memcpy( pgraph->pNodeBuffer + pgraph->iNodeBuffer , pnode , GNGRP_NODE_SIZEOF(pgraph->NodeAttrSize) );
			pgraph->iNodeBuffer += GNGRP_NODE_SIZEOF(pgraph->NodeAttrSize);
		}
	}

	if ( pgraph->pNodeTree ) gnTreeDestroy( pgraph->pNodeTree );
	pgraph->pNodeTree = NULL;

	gnHeapFree( & pgraph->NodeHeap );

	pgraph->Flags |= 0x1; /* flattened */

	/*
	 * convert to-node ids from links into offset in the node buffer
	 */
	for (
		pflat1 = (gnInt32_t*) pgraph->pNodeBuffer ;
		pflat1 < (gnInt32_t*) (pgraph->pNodeBuffer + pgraph->iNodeBuffer) ;
		pflat1 += GNGRP_NODE_WSIZE(pgraph->NodeAttrSize)
		)
	{
		if ( GNGRP_NODE_STATUS(pflat1) & GNGRP_NS_FROM )
		{
			pflat2 = (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET(pflat1));
			for (
				 i = 0 , pflat3 = GNGRP_LINKAREA_LINK_PTR(pflat2, 0, pgraph->LinkAttrSize) ;
				 i < GNGRP_LINKAREA_LINKCOUNT(pflat2) ;
				 i ++ , pflat3 = GNGRP_LINKAREA_LINK_PTR(pflat2, i, pgraph->LinkAttrSize)
				)
			{
				if ( (pnode = _get_node_V1( pgraph , GNGRP_LINK_TONODE_OFFSET(pflat3))) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
					return -pgraph->iErrno;
				}
				GNGRP_LINK_TONODE_OFFSET(pflat3) = (unsigned long)pnode - (unsigned long)pgraph->pNodeBuffer;
				pgraph->cArc ++;
			}
		}
	}

	return 0;
}

static int _set_nodeattr_V1( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t nodeid )
{
	gnInt32_t *			pnode;

	if ( (pnode = _get_node_V1( pgraph , nodeid )) == NULL ) {
		pgraph->iErrno = GNGRP_ERR_NodeNotFound;
		return -pgraph->iErrno;
	}
	memcpy( GNGRP_NODE_ATTR_PTR(pnode) , pattr , pgraph->NodeAttrSize );
	return 0;
}

static void * _get_nodeattr_V1( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	gnInt32_t *			pnode;

	if ( (pnode = _get_node_V1( pgraph , nodeid )) == NULL ) {
		pgraph->iErrno = GNGRP_ERR_NodeNotFound;
		return NULL;
	}
	return GNGRP_NODE_ATTR_PTR(pnode);
}

static int _set_linkattr_V1( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	gnInt32_t *			pnode;
	gnInt32_t *			plink;

	if ( (pnode = _get_node_V1( pgraph , fnodeid )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
		return -pgraph->iErrno;
	}

	if ( (plink = _get_link_V1( pgraph , pnode , tnodeid )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
		return -pgraph->iErrno;
	}

	memcpy( GNGRP_LINK_ATTR_PTR(plink) , pattr , pgraph->LinkAttrSize );
	return 0;
}

static void * _get_linkattr_V1( gnGrpGraph_s * pgraph , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	gnInt32_t *			pnode;
	gnInt32_t *			plink;

	if ( (pnode = _get_node_V1( pgraph , fnodeid )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
		return NULL;
	}

	if ( (plink = _get_link_V1( pgraph , pnode , tnodeid )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
		return NULL;
	}

	return GNGRP_LINK_ATTR_PTR(plink);
}

int gnGrpScan(
					gnGrpGraph_s * pgraph ,
					int (*push)( gnGrpGraph_s * pgraph , gnInt32_t * pnode , void * pvarg ) ,
					void * pvarg
					)
{
	gnInt32_t * pnode;
	int			nret;

	if ( push == NULL ) {
		pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		return -pgraph->iErrno;
	}

	if ( !(pgraph->Flags & 0x1) ) {
		pgraph->iErrno = GNGRP_ERR_BadOnTreeGraph;
		return -pgraph->iErrno;
	}

	GNGRP_FOREACH_NODE(pgraph, pnode) {
		if ( (nret=push( pgraph, pnode, pvarg )) ) return nret;	
	}

	return 0;
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
#ifndef GNGRP_NEWCLIP
							int (*clip)		(
											gnGrpGraph_s * pgraph,
											gnInt32_t * pprevlink,
											gnInt32_t * pfromnode,
											gnInt32_t * plink,
											gnInt32_t * ptonode,
											gnInt32_t * pcost,
											void *
											) ,
							void * 				pvcliparg
#else /* GNGRP_NEWCLIP */
							gnGrpSPClip_fn		fnClip,
							void * 				pvClipArg
#endif /* GNGRP_NEWCLIP */
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
		pgraph->iErrno = GNGRP_ERR_BadOnTreeGraph;
		return NULL;
	}

	gnHeapInit( & NodeHeap );

	/* create temporary networks - The use of a predist network for predecessor and
	   distance has two important results: 1) allows us not having to reset the whole graph status
	   at each call; 2) use of a stack memory area for temporary (and otherwise possibly thread-conflicting)
	   states.
	*/
	if ( (pvVisited = gnTreeCreate( _node_free , NULL )) == NULL ) goto sp_error;
	if ( (pvPredist = gnTreeCreate( _node_free , NULL )) == NULL ) goto sp_error;

	if ( (pfrom = _get_node_V1( pgraph , from )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
		goto sp_error;
	}
	pfromnode = pfrom;

	if ( (ptonode = _get_node_V1( pgraph , to )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
		goto sp_error;
	}

	if ( ! (GNGRP_NODE_STATUS(pfromnode) & GNGRP_NS_FROM) )
	{
		pgraph->iErrno = 0;
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
		i ++,   plink += GNGRP_LINK_WSIZE(pgraph->LinkAttrSize)
		)
	{
		ptonode = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) );

		if ( ! (GNGRP_NODE_STATUS(ptonode) & GNGRP_NS_TO) )
		{
			pgraph->iErrno = GNGRP_ERR_BadLink;
			goto sp_error;
		}

		/*
		 * arc clipping : no previous link in path at first round
		 */
#ifndef GNGRP_NEWCLIP
		nTmpCost = GNGRP_LINK_COST(plink);
#else /* GNGRP_NEWCLIP */
		clipOutput.nLinkCost = GNGRP_LINK_COST(plink);
#endif /* GNGRP_NEWCLIP */

#ifndef GNGRP_NEWCLIP
		if ( clip )
		{
			if ( clip( pgraph , NULL , pfromnode , plink , ptonode , &nTmpCost , pvcliparg ) ) continue;
#else /* GNGRP_NEWCLIP */
		if ( fnClip )
		{
			clipInput.pnPrevLink 	= NULL;
			clipInput.pnNodeFrom 	= pfromnode;
			clipInput.pnLink		= plink;
			clipInput.pnNodeTo		= ptonode;
			clipInput.nFromDistance = 0;

			if ( fnClip( pgraph , & clipInput , & clipOutput , pvClipArg ) ) continue;
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
		if ( ! (GNGRP_NODE_STATUS(pfromnode) & GNGRP_NS_FROM) )  continue;

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
			i ++,   plink += GNGRP_LINK_WSIZE(pgraph->LinkAttrSize)
			)
		{
			ptonode = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink) );
		
			if ( ! (GNGRP_NODE_STATUS(ptonode) & GNGRP_NS_TO) )
			{
				pgraph->iErrno = GNGRP_ERR_BadLink;
				goto sp_error;
			}
		
			/*
			 * arc clipping : we now have previous link and from distance
			 */
#ifndef GNGRP_NEWCLIP
			nTmpCost = GNGRP_LINK_COST(plink);
#else /* GNGRP_NEWCLIP */
			clipOutput.nLinkCost = GNGRP_LINK_COST(plink);
#endif /* GNGRP_NEWCLIP */

#ifndef GNGRP_NEWCLIP
			if ( clip )
			{
				if ( clip( pgraph , plink_prev , pfromnode , plink , ptonode , &nTmpCost , pvcliparg ) ) continue;
#else /* GNGRP_NEWCLIP */
			if ( fnClip )
			{
				clipInput.pnPrevLink 	= plink_prev;
				clipInput.pnNodeFrom 	= pfromnode;
				clipInput.pnLink		= plink;
				clipInput.pnNodeTo		= ptonode;
				clipInput.nFromDistance = fromDist;

				if ( fnClip( pgraph , & clipInput , & clipOutput , pvClipArg ) ) continue;
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

			arc.From = GNGRP_NODE_ALLOC( pgraph->NodeAttrSize );
			arc.To   = GNGRP_NODE_ALLOC( pgraph->NodeAttrSize );
			arc.Link = GNGRP_LINK_ALLOC( pgraph->LinkAttrSize );
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
			memcpy( arc.From , ptonode_pred , GNGRP_NODE_SIZEOF( pgraph->NodeAttrSize ) );
			memcpy( arc.To   , ptonode      , GNGRP_NODE_SIZEOF( pgraph->NodeAttrSize ) );
			memcpy( arc.Link , plink        , GNGRP_LINK_SIZEOF( pgraph->LinkAttrSize ) );
			arc.Distance = pPredist[1];
			/*
			 * fix the link cost with real cost
			 */
			GNGRP_LINK_COST(arc.Link) = pPredist[2];

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

/*
 * Build the depth-first spanning tree of 'pgraphIn' with vertex 'nNodeId' into 'pgraphOut'
 * - As usual this algorithm applies to a FLAT state pgraphIn
 * - pgraphOut must have been previously initialized by the caller and is returned in TREE state
 * - I prefer using a iterative approach with a stack for 'waiting links' instead of recursion: it's
 *   cheaper to stack 8 bytes for each link than the whole function stack
 * - The visited network is passed by the caller because this function can be used for two purposes:
 *   1. generate a single spanning tree (gnGrpDepthSpanning)
 *   2. part of a loop for generating connected-components of the graph (gnGrpDepthComponents)
 */
static int _depthfirst_spanning_V1(
						gnGrpGraph_s * pgraphIn ,
						gnGrpGraph_s * pgraphOut ,
						gnInt32_t nNodeId ,
						void * pvVisited ,
						gnGrpSpanClip_fn	fnClip ,
						void *				pvClipArg
						)
{
	gnInt32_t * 	pfrom;
	gnInt32_t * 	pto;
	gnInt32_t * 	plinkarea;
	gnInt32_t * 	plink;
	gnInt32_t 		id[2] , * pid;
	long 			istack = 0;
	unsigned char *	pstack = NULL;
	int				nret;
	gnGrpSpanClipInput_s	clipInput;


	if ( ! (pgraphIn->Flags & 0x1) ) {
		pgraphIn->iErrno = GNGRP_ERR_BadOnTreeGraph; return -pgraphIn->iErrno;
	}

	if ( (pfrom = gnGrpGetNode( pgraphIn, nNodeId )) == NULL ) {
		pgraphIn->iErrno = GNGRP_ERR_FromNodeNotFound; goto dfs_error;
	}

	if ( ! (GNGRP_NODE_STATUS(pfrom) & GNGRP_NS_FROM) ) {
		return 0;
	}

	plinkarea = GNGRP_LINKBUFFER_SHIFT(pgraphIn, GNGRP_NODE_LINKAREA_OFFSET(pfrom));

	GNGRP_FOREACH_LINK(pgraphIn, plinkarea, plink) {
		id[0] = GNGRP_NODEBUFFER_OFFSET(pgraphIn, pfrom);
	   	id[1] = GNGRP_LINKBUFFER_OFFSET(pgraphIn, plink);
		if ( (pstack = _mempush( pstack , & istack , sizeof( gnInt32_t ) * 2 , id )) == NULL ) {
			pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
		}
	}

	if ( __node(pvVisited , GNGRP_NODE_ID(pfrom)) == NULL ) {
		pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
	}

	while( (pid = (gnInt32_t*)_mempop( pstack , & istack , sizeof(gnInt32_t)  * 2)) != NULL )
	{
		pfrom  = (gnInt32_t*)(pgraphIn->pNodeBuffer + pid[0]);
		plink  = (gnInt32_t*)(pgraphIn->pLinkBuffer + pid[1]);
		pto    = (gnInt32_t*)(pgraphIn->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET(plink));

		if ( gnTreeSearch( pvVisited , GNGRP_NODE_ID(pto) ) ) { /* already visited */
			continue;
		}

		if ( fnClip ) {
			clipInput.pnNodeFrom = pfrom;
			clipInput.pnLink     = plink;
			clipInput.pnNodeTo   = pto;
			if ( fnClip( pgraphIn, pgraphOut, & clipInput, NULL, pvClipArg ) ) continue;
		}

		if ( __node(pvVisited , GNGRP_NODE_ID(pto)) == NULL ) {
			pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
		}

		/* add this link */
		nret = _add_link_V1( pgraphOut,
					GNGRP_NODE_ID(pfrom),
					GNGRP_NODE_ID(pto),
					GNGRP_LINK_COST(plink),
					GNGRP_LINK_USER(plink),
					GNGRP_NODE_ATTR_PTR(pfrom),
					GNGRP_NODE_ATTR_PTR(pto),
					GNGRP_LINK_ATTR_PTR(plink)
					);

		if ( nret < 0 ) {
			goto dfs_error;
		}

		if ( GNGRP_NODE_STATUS(pto) & GNGRP_NS_FROM ) {
			plinkarea = GNGRP_LINKBUFFER_SHIFT(pgraphIn, GNGRP_NODE_LINKAREA_OFFSET(pto));

			GNGRP_FOREACH_LINK(pgraphIn, plinkarea, plink) {
				id[0] = GNGRP_NODEBUFFER_OFFSET(pgraphIn, pto);
	   			id[1] = GNGRP_LINKBUFFER_OFFSET(pgraphIn, plink);
				if ( (pstack = _mempush( pstack , & istack , sizeof( gnInt32_t ) * 2 , id )) == NULL ) {
					pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
				}
			}
		}
	}

	if ( pstack ) free( pstack );
	return 0;

dfs_error:
	if ( pstack ) free( pstack );
	return -pgraphIn->iErrno;
}


static int _release_V1( gnGrpGraph_s * pgraph )
{
	pgraph->iErrno = 0;

	if ( pgraph->pNodeTree ) gnTreeDestroy( pgraph->pNodeTree );

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
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->Endian , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->NodeAttrSize  , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->LinkAttrSize  , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	for ( cnt = 0 ; cnt < 16 ; cnt ++ )
	{
		if ( write( fd , & pgraph->aOpaqueSet[ cnt ] , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
		{
			pgraph->iErrno = GNGRP_ERR_Write;
			return -pgraph->iErrno;
		}
	}

	if ( write( fd , & pgraph->cNode , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->cFrom , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->cTo , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->cArc , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->iNodeBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	if ( write( fd , & pgraph->iLinkBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Write;
		return -pgraph->iErrno;
	}

	for( tot = 0 , cnt = pgraph->iNodeBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = write( fd , & pgraph->pNodeBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			pgraph->iErrno = GNGRP_ERR_Write;
			return -pgraph->iErrno;
		}
	}

	for( tot = 0 , cnt = pgraph->iLinkBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = write( fd , & pgraph->pLinkBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			pgraph->iErrno = GNGRP_ERR_Write;
			return -pgraph->iErrno;
		}
	}

	return 0;
}


static void _swapInt32Bytes( gnInt32_t * pn ) {
	unsigned char * pb = (unsigned char *) pn;
	pb[0] ^= pb[3];
	pb[3] ^= pb[0];
	pb[0] ^= pb[3];
	pb[1] ^= pb[2];
	pb[2] ^= pb[1];
	pb[1] ^= pb[2];
}	

static int _read_V1( gnGrpGraph_s * pgraph , int fd )
{
	long 		nret , cnt , tot;	
	gnByte_t 	Endian;
	gnInt32_t	NodeAttrSize , LinkAttrSize;
	int			i, cn, fSwap;
	gnInt32_t *	pn;

	if ( read( fd , & Endian , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}

	fSwap = 0;
#ifdef GN_ENDIAN_BIG
	if ( Endian == GNGRP_ENDIAN_LITTLE ) fSwap = 1;
#else
	if ( Endian == GNGRP_ENDIAN_BIG    ) fSwap = 1;
#endif

	if ( read( fd , & NodeAttrSize , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & NodeAttrSize );

	if ( read( fd , & LinkAttrSize , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & LinkAttrSize );

	if ( (nret = gnGrpInitialize( pgraph, 1, NodeAttrSize, LinkAttrSize, NULL )) < 0 )
	{
		return nret;
	}

	for ( cnt = 0 ; cnt < 16 ; cnt ++ )
	{
		if ( (nret=read( fd , & pgraph->aOpaqueSet[ cnt ] , sizeof( gnInt32_t ) )) != sizeof( gnInt32_t ) )
		{
			pgraph->iErrno = GNGRP_ERR_Read;
			return -pgraph->iErrno;
		}
		if ( fSwap ) _swapInt32Bytes( & pgraph->aOpaqueSet[ cnt ] );
	}

	if ( read( fd , & pgraph->cNode , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & pgraph->cNode );

	if ( read( fd , & pgraph->cFrom , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & pgraph->cFrom );

	if ( read( fd , & pgraph->cTo , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & pgraph->cTo );

	if ( read( fd , & pgraph->cArc , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & pgraph->cArc );

	if ( read( fd , & pgraph->iNodeBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & pgraph->iNodeBuffer );

	if ( read( fd , & pgraph->iLinkBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) _swapInt32Bytes( & pgraph->iLinkBuffer );

	if ( (pgraph->pNodeBuffer = malloc( pgraph->iNodeBuffer )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraph->iErrno;
	}

	if ( (pgraph->pLinkBuffer = malloc( pgraph->iLinkBuffer )) == NULL )
	{
		free( pgraph->pNodeBuffer );
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraph->iErrno;
	}

	for( tot = 0 , cnt = pgraph->iNodeBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = read( fd , & pgraph->pNodeBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			free( pgraph->pNodeBuffer );
			free( pgraph->pLinkBuffer );
			pgraph->iErrno = GNGRP_ERR_Read;
			return -pgraph->iErrno;
		}
	}
	if ( fSwap ) {
		pn = (gnInt32_t*) pgraph->pNodeBuffer;
		cn = pgraph->iNodeBuffer / sizeof(gnInt32_t);
		for ( i = 0 ; i < cn ; i ++ ) {
			_swapInt32Bytes( & pn[i] );
		}
	}

	for( tot = 0 , cnt = pgraph->iLinkBuffer ; tot < cnt ; tot += nret )
	{
		if ( (nret = read( fd , & pgraph->pLinkBuffer[ tot ] , cnt - tot )) <= 0 )
		{
			free( pgraph->pNodeBuffer );
			free( pgraph->pLinkBuffer );
			pgraph->iErrno = GNGRP_ERR_Read;
			return -pgraph->iErrno;
		}
	}
	if ( fSwap ) {
		pn = (gnInt32_t*) pgraph->pLinkBuffer;
		cn = pgraph->iLinkBuffer / sizeof(gnInt32_t);
		for ( i = 0 ; i < cn ; i ++ ) {
			_swapInt32Bytes( & pn[i] );
		}
	}

	pgraph->Flags |= 0x1; /* flat-state */
	return 0;
}


/*
 * version 1 calls
 */
static gnGrpIOMethods_s _v1_methods =
{
	_write_V1,
	_read_V1
};

static int _init_V1(gnGrpGraph_s * pgraph, gnInt32_t NodeAttrSize, gnInt32_t LinkAttrSize, gnInt32_t * pOpaqueSet)
{
	if ( pgraph == NULL ) {
		return -GNGRP_ERR_UnexpectedNullPointer;
	}

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
	pgraph->iErrno = GNGRP_ERR_VersionNotSupported;
	return -pgraph->iErrno;
}

int gnGrpRelease( gnGrpGraph_s * pgraph )
{
	return _release_V1( pgraph );
}

int gnGrpUnflatten( gnGrpGraph_s * pgraph )
{
	return _unflatten_V1( pgraph );
}


int gnGrpFlatten( gnGrpGraph_s * pgraph )
{
	return _flatten_V1( pgraph );
}


gnInt32_t * gnGrpGetNode( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	return _get_node_V1( pgraph , nodeid );
}

gnInt32_t * gnGrpGetLinkArea( gnGrpGraph_s * pgraph , gnInt32_t * pnode )
{
	return _get_linkarea_V1( pgraph , pnode );
}

gnInt32_t * gnGrpGetLink( gnGrpGraph_s * pgraph , gnInt32_t * pfromnode , gnInt32_t tnodeid )
{
	return _get_link_V1( pgraph, pfromnode, tnodeid );
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
	int 		nret;

#ifdef GNGRP_STATS
	clock_t clk;
	clk = clock();
	pgraph->cAddLink ++;
#endif
	
	nret = _add_link_V1( pgraph, lFrom, lTo, lCost, lUser, pvFnodeAttr, pvTnodeAttr, pvLinkAttr );

#ifdef GNGRP_STATS
	pgraph->clkAddLink += clock() - clk;
#endif
	return nret;
}

int gnGrpSetNodeAttr( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t nodeid )
{
	return _set_nodeattr_V1( pgraph , pattr , nodeid );
}

void * gnGrpGetNodeAttr( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	return _get_nodeattr_V1( pgraph , nodeid );
}

int gnGrpSetLinkAttr( gnGrpGraph_s * pgraph , void * pattr , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	return _set_linkattr_V1( pgraph, pattr, fnodeid, tnodeid );
}

void * gnGrpGetLinkAttr( gnGrpGraph_s * pgraph , gnInt32_t fnodeid , gnInt32_t tnodeid )
{
	return _get_linkattr_V1( pgraph, fnodeid, tnodeid );
}

int gnGrpWrite( gnGrpGraph_s * pgraph, int fd )
{
	pgraph->iErrno = 0;
	if ( pgraph->pMethods->write )
		return pgraph->pMethods->write( pgraph , fd );
	pgraph->iErrno = GNGRP_ERR_UndefinedMethod;
	return -pgraph->iErrno;
}

int gnGrpRead( gnGrpGraph_s * pgraph, int fd )
{
	gnByte_t bVersion;

	if ( read( fd , & bVersion , 1 ) != 1 )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}

	switch( bVersion ) {
	case 1:
		return _read_V1( pgraph, fd );
	default:
		pgraph->iErrno = GNGRP_ERR_VersionNotSupported;
		return -pgraph->iErrno;
	}
}


gnGrpSPReport_s * gnGrpShortestPath	(
								 	gnGrpGraph_s * 	pgraph ,
								 	gnInt32_t 		from ,
								 	gnInt32_t 		to ,
#ifndef GNGRP_NEWCLIP
								 	int (*clip)(
											 	gnGrpGraph_s *, /* graph pointer */
											 	gnInt32_t *,	/* previous link pointer */
											 	gnInt32_t *,	/* from node pointer */
											 	gnInt32_t *,	/* this link pointer */
											 	gnInt32_t *,	/* to node pointer */
											 	gnInt32_t *,	/* real cost pointer */
											 	void  *			/* caller's context pointer */
											 	),
								 	void * 		pvcliparg		/* caller's context pointer (passed back to clip)*/
#else /* GNGRP_NEWCLIP */
									gnGrpSPClip_fn	fnClip,
									void *			pvClipArg
#endif /* GNGRP_NEWCLIP */
								 	)
{
#ifndef GNGRP_NEWCLIP
	return _dijkstra_V1( pgraph, from, to, clip, pvcliparg );
#else /* GNGRP_NEWCLIP */
	return _dijkstra_V1( pgraph, from, to, fnClip, pvClipArg );
#endif /* GNGRP_NEWCLIP */
}


int	gnGrpDepthSpanning	(
						gnGrpGraph_s *  	pgraphInput,
						gnGrpGraph_s *  	pgraphOutput,
						gnInt32_t			nVertexNode,
						gnGrpSpanClip_fn	fnClip,
						void *				pvClipArg
						)
{
	int nret;
	void * pvVisited;

	if ( gnGrpGet_LinkCount(pgraphInput) == 0 ) { /* no span */
		pgraphInput->iErrno = 0;
		return 0;
	}

	nret = gnGrpInitialize( pgraphOutput,
			gnGrpGet_Version(pgraphInput),
			gnGrpGet_NodeAttrSize(pgraphInput),
			gnGrpGet_LinkAttrSize(pgraphInput), 
			gnGrpGet_Opaque(pgraphInput) );

	if ( nret < 0 ) return nret;

	if ( (pvVisited = gnTreeCreate( _node_free , NULL )) == NULL ) {
		pgraphInput->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraphInput->iErrno;
	}

	nret = _depthfirst_spanning_V1( pgraphInput, pgraphOutput, nVertexNode , pvVisited , fnClip , pvClipArg );

	gnTreeDestroy( pvVisited );

	return nret;
}

int gnGrpDepthComponents(
						gnGrpGraph_s *  	pgraphInput,
						gnGrpGraph_s *  	pgraphComponents,
						int					cgraphComponents,
						gnGrpSpanClip_fn	fnClip,
						void *				pvClipArg
						)
{
	int i, nret;
	void * pvVisited;
	gnInt32_t * pvertex , * pnode;

	if ( gnGrpGet_LinkCount(pgraphInput) == 0 ) { /* no span */
		pgraphInput->iErrno = 0;
		return 0;
	}

	if ( (pvVisited = gnTreeCreate( _node_free , NULL )) == NULL ) {
		pgraphInput->iErrno = GNGRP_ERR_MemoryExhausted; goto error;
	}

	/*
	 * choose a vertex to start from
	 */
	pvertex = NULL;
	GNGRP_FOREACH_NODE(pgraphInput, pnode) {
		if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_FROM ) {
			pvertex = pnode;
			break;
		}
	}

	if ( pvertex == NULL ) {
		pgraphInput->iErrno = GNGRP_ERR_UnexpectedNullPointer; goto error;
	}

	for (
		i = 0;
		i < cgraphComponents && pvertex;
		i++
		)
	{
		nret = gnGrpInitialize( & pgraphComponents[i],
				gnGrpGet_Version(pgraphInput),
				gnGrpGet_NodeAttrSize(pgraphInput),
				gnGrpGet_LinkAttrSize(pgraphInput), 
				gnGrpGet_Opaque(pgraphInput) );

		if ( nret < 0 ) goto error;

		nret = _depthfirst_spanning_V1(pgraphInput, & pgraphComponents[i], GNGRP_NODE_ID(pvertex), pvVisited, fnClip, pvClipArg);
		if ( nret < 0 ) goto error;
		
		/*
		 * select next unvisited vertex
		 */
		pvertex = NULL;
		GNGRP_FOREACH_NODE(pgraphInput, pnode) {
			if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_FROM ) {
				if ( gnTreeSearch( pvVisited, GNGRP_NODE_ID(pnode) ) == NULL ) {
					pvertex = pnode;
					break;
				}
			}
		}
	}

	gnTreeDestroy(pvVisited);
	return i;

error:
	gnTreeDestroy(pvVisited);
	return -pgraphInput->iErrno;
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
		return "Operation Not Supported On Flat-State Graph";
	case GNGRP_ERR_BadOnTreeGraph:
		return "Operation Not Supported On Tree-State Graph";
	case GNGRP_ERR_TreeSearchError:
		return "Tree Search Error";
	case GNGRP_ERR_UnexpectedNullPointer:
		return "Unexpected Null Pointer";
	case GNGRP_ERR_VersionNotSupported:
		return "Version Not Supported";
	}

	return "unknown graph error code";
}

/*
 * gnGrpGraph_s hiders
 */
int	gnGrpGet_Version		(gnGrpGraph_s *  pgraph) {
	return pgraph->Version;
}
int	gnGrpGet_Endianess		(gnGrpGraph_s *  pgraph) {
	return pgraph->Endian;
}
int	gnGrpGet_NodeAttrSize	(gnGrpGraph_s *  pgraph) {
	return pgraph->NodeAttrSize;
}
int	gnGrpGet_LinkAttrSize	(gnGrpGraph_s *  pgraph) {
	return pgraph->LinkAttrSize;
}
int	gnGrpGet_NodeCount		(gnGrpGraph_s *  pgraph) {
	return pgraph->cNode;
}
int	gnGrpGet_FromNodeCount	(gnGrpGraph_s *  pgraph) {
	return pgraph->cFrom;
}
int	gnGrpGet_ToNodeCount	(gnGrpGraph_s *  pgraph) {
	return pgraph->cTo;
}
int	gnGrpGet_LinkCount		(gnGrpGraph_s *  pgraph) {
	return pgraph->cArc;
}
int	gnGrpGet_GraphState		(gnGrpGraph_s *  pgraph) {
	return pgraph->Flags;
}
gnInt32_t * gnGrpGet_Opaque	(gnGrpGraph_s *  pgraph) {
	return pgraph->aOpaqueSet;
}
void gnGrpSet_Opaque( gnGrpGraph_s * pgraph, gnInt32_t * pOpaque ) {
	memcpy( pgraph->aOpaqueSet , pOpaque , sizeof( gnInt32_t ) * 16 );
}





