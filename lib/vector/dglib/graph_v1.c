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
#include "helpers.h"
#include "graph.h"
#include "graph_v1.h"


int gngrp_add_node_V1(
						gnGrpGraph_s *  pgraph,
						gnInt32_t       lNodeId,
						void * 			pvNodeAttr,	
						gnInt32_t 		nFlags
					)
{
	gnTreeNode_s * 	pNodeItem;
	gnHeapData_u	HeapData;
	gnInt32_t *		pnode;

	if ( pgraph->Flags & GNGRP_GS_FLAT )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnFlatGraph;
		return -pgraph->iErrno;
	}

	if ( pgraph->pNodeTree == NULL ) pgraph->pNodeTree = gnTreeCreate( gngrp_node_free , pgraph );
	if ( pgraph->pNodeTree == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraph->iErrno;
	}

	if ( (pNodeItem = gngrp__node( pgraph->pNodeTree , lNodeId )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraph->iErrno;
	}


	if ( pNodeItem->data.pv == NULL )
	{
		if ( (pnode = GNGRP_NODE_ALLOC_v1( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		memset( pnode, 0, GNGRP_NODE_SIZEOF_v1(pgraph->NodeAttrSize) );
		GNGRP_NODE_STATUS_v1(pnode) = GNGRP_NS_ALONE;
		pNodeItem->data.pv = pnode;
		HeapData.pv = pnode;

		if ( gnHeapInsertMin( & pgraph->NodeHeap , lNodeId , HeapData ) < 0 ) {
			return -1;
		}
	}
	else
	{
		/* node already exists */
		pgraph->iErrno = GNGRP_ERR_NodeAlreadyExist;
		return -pgraph->iErrno;
	}
	return 0;
}


/*
 * Add link can be performed on TREE state graph. If the state is FLAT
 * return BadOnFlatGraph error.
 */
int gngrp_add_link_V1 (
						gnGrpGraph_s * 	pgraph ,
						gnInt32_t 		lFrom,
						gnInt32_t 		lTo,
						gnInt32_t 		lCost,
						gnInt32_t 		lUser,
						void * pvFnodeAttr ,	
						void * pvTnodeAttr ,	
						void * pvLinkAttr ,
						gnInt32_t 		nFlags
						)
{
	gnInt32_t *		pfrom;
	gnInt32_t *		pto;
	gnInt32_t *		plinkarea;
	gnInt32_t *		plink;
	gnTreeNode_s * 	pFromNodeItem;
	gnTreeNode_s * 	pToNodeItem;
	gnHeapData_u	HeapData;

	if ( pgraph->Flags & GNGRP_GS_FLAT )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnFlatGraph;
		return -pgraph->iErrno;
	}

	if ( pgraph->pNodeTree == NULL ) pgraph->pNodeTree = gnTreeCreate( gngrp_node_free , pgraph );
	if ( pgraph->pNodeTree == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraph->iErrno;
	}

#ifdef GNGRP_STATS
		{
		clock_t clk = clock();
#endif

		if (
			(pFromNodeItem = gngrp__node( pgraph->pNodeTree , lFrom )) == NULL
			||
			(pToNodeItem   = gngrp__node( pgraph->pNodeTree , lTo   )) == NULL
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
		if ( (pfrom = GNGRP_NODE_ALLOC_v1( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -1;
		}
		GNGRP_NODE_STATUS_v1(pfrom) = 0;
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
		if ( (pto = GNGRP_NODE_ALLOC_v1( pgraph->NodeAttrSize )) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		GNGRP_NODE_STATUS_v1(pto) = 0;
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


	GNGRP_NODE_STATUS_v1(pfrom) |= GNGRP_NS_FROM;
	GNGRP_NODE_STATUS_v1(pto)   |= GNGRP_NS_TO;

	if ( GNGRP_NODE_STATUS_v1(pfrom) & GNGRP_NS_ALONE ) GNGRP_NODE_STATUS_v1(pfrom) &= ~GNGRP_NS_ALONE;
	if ( GNGRP_NODE_STATUS_v1(pto  ) & GNGRP_NS_ALONE ) GNGRP_NODE_STATUS_v1(pto  ) &= ~GNGRP_NS_ALONE;

	GNGRP_NODE_ID_v1(pfrom) = lFrom;
	GNGRP_NODE_ID_v1(pto)   = lTo;

	GNGRP_NODE_LINKAREA_OFFSET_v1(pfrom) = -1;
	GNGRP_NODE_LINKAREA_OFFSET_v1(pto)   = -1;

	if ( pvFnodeAttr && pgraph->NodeAttrSize ) {
		memcpy( GNGRP_NODE_ATTR_PTR_v1(pfrom), pvFnodeAttr, pgraph->NodeAttrSize );
	}

	if ( pvTnodeAttr && pgraph->NodeAttrSize ) {
		memcpy( GNGRP_NODE_ATTR_PTR_v1(pto), pvTnodeAttr, pgraph->NodeAttrSize );
	}

	if ( pFromNodeItem->data2.pv == NULL )
	{
		pFromNodeItem->data2.pv = GNGRP_LINKAREA_ALLOC_v1( 1 , pgraph->LinkAttrSize );
		if ( pFromNodeItem->data2.pv == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		plinkarea = pFromNodeItem->data2.pv;
		GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea) = 0;
	}
	else
	{
		plinkarea = pFromNodeItem->data2.pv;
		pFromNodeItem->data2.pv = GNGRP_LINKAREA_REALLOC_v1( pFromNodeItem->data2.pv ,
												GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea) + 1 ,
												pgraph->LinkAttrSize );
		if ( pFromNodeItem->data2.pv == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			return -pgraph->iErrno;
		}
		plinkarea = pFromNodeItem->data2.pv;
	}

	plink = GNGRP_LINKAREA_LINKARRAY_PTR_v1(plinkarea) +
			GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea) * GNGRP_LINK_WSIZE_v1(pgraph->LinkAttrSize);

	GNGRP_LINK_TONODE_OFFSET_v1(plink)	= lTo;		/* will be an offset after flattening */
	GNGRP_LINK_COST_v1(plink)          = lCost;
	GNGRP_LINK_USER_v1(plink)          = lUser;

	if ( pvLinkAttr && pgraph->LinkAttrSize ) {
		memcpy( GNGRP_LINK_ATTR_PTR_v1(plink), pvLinkAttr, pgraph->LinkAttrSize );
	}

	GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea) ++;

	if ( nFlags & GNGRP_STRONGCONNECT ) {
		return gngrp_add_link_V1(pgraph,lTo,lFrom,lCost,lUser,pvFnodeAttr,pvTnodeAttr,pvLinkAttr,nFlags & ~GNGRP_STRONGCONNECT);
	}

	return 0;
}



/*
 * if graph is FLAT perform a binary search in the pNodeBuffer
 * if graph is TREE search the node in the pNodeTree avl
 */
gnInt32_t * gngrp_get_node_V1( gnGrpGraph_s * pgraph , gnInt32_t nodeid )
{
	register gnInt32_t 		top;	/* top of table */
	register gnInt32_t 		pos;	/* current position to compare */
	register gnInt32_t 		bot;	/* bottom of table */
	register gnInt32_t *	pref;
	register int			cwords; /* size of a node in words of 32 bit */
	register gnTreeNode_s * ptreenode;
			 gnInt32_t		id;

	if ( pgraph->Flags & 0x1 ) {
		cwords = GNGRP_NODE_WSIZE_v1(pgraph->NodeAttrSize);
		bot    = pgraph->iNodeBuffer / GNGRP_NODE_SIZEOF_v1(pgraph->NodeAttrSize);
		top    = 0;
		pos    = 0;
		pref   = (gnInt32_t*)pgraph->pNodeBuffer;

		/* perform a binary search
	 	*/
		while( top != bot ) {
			pos = top + (bot - top) / 2;
			id = GNGRP_NODE_ID_v1(& pref[pos * cwords]);
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
gnInt32_t * gngrp_getnode_outlinkarea_V1( gnGrpGraph_s * pgraph , gnInt32_t * pnode )
{
	gnTreeNode_s * 	ptreenode;
	gnInt32_t * 	plinkarea;

	if ( pnode == NULL ) {
		pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		return NULL;
	}

	if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_ALONE) {
		pgraph->iErrno = GNGRP_ERR_NodeIsAComponent;
		return NULL;
	}

	if ( pgraph->Flags & 0x1 ) {
		plinkarea = (gnInt32_t*)(pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET_v1(pnode));
		return plinkarea;
	}
	else {
		ptreenode = gnTreeSearch( pgraph->pNodeTree , GNGRP_NODE_ID_v1(pnode) );
		if ( ptreenode && ptreenode->data2.pv ) {
			return ptreenode->data2.pv;
		}
		pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		return NULL;
	}
}

int gngrp_unflatten_V1( gnGrpGraph_s * pgraph )
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
	 * unflag it now to avoid gngrp_add_link_V1() failure
	 */
	pgraph->Flags &= ~0x1;

	GNGRP_FOREACH_NODE_v1(pgraph,pnode)
	{
		if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_FROM )
		{
			plinkarea = (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET_v1(pnode));

			for	(
				k = 0 , plinkto = GNGRP_LINKAREA_LINK_PTR_v1(plinkarea, 0, pgraph->LinkAttrSize) ;
				k < GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea) ;
				k ++  , plinkto = GNGRP_LINKAREA_LINK_PTR_v1(plinkarea, k, pgraph->LinkAttrSize)
				)
			{
				pnodeto = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET_v1(plinkto));

				nret = gngrp_add_link_V1( pgraph ,
							GNGRP_NODE_ID_v1(pnode),
							GNGRP_NODE_ID_v1(pnodeto),
							GNGRP_LINK_COST_v1(plinkto),
							GNGRP_LINK_USER_v1(plinkto),
							GNGRP_NODE_ATTR_PTR_v1(pnode),
							GNGRP_NODE_ATTR_PTR_v1(pnodeto),
							GNGRP_LINK_ATTR_PTR_v1(plinkto),
							0 );

				if ( nret < 0 ) {
					goto error;
				}
			}
		}
		else if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_ALONE ) {
			nret = gngrp_add_node_V1( pgraph, 
						GNGRP_NODE_ID_v1(pnode),
						GNGRP_NODE_ATTR_PTR_v1(pnode),
						0 );
			if ( nret < 0 ) {
				goto error;
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

error:
	if ( pgraph->pNodeTree ) gnTreeDestroy( pgraph->pNodeTree );
	pgraph->pNodeTree = NULL;
	gnHeapFree( & pgraph->NodeHeap );
	pgraph->Flags |= 0x1; /* keep it flat */
	return nret;
}


int gngrp_flatten_V1( gnGrpGraph_s * pgraph )
{
	gnHeapNode_s   			heapnode;
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

	pgraph->pNodeBuffer = NULL; /* should be already */
	pgraph->iNodeBuffer = 0;
	pgraph->pLinkBuffer = NULL;
	pgraph->iLinkBuffer = 0;
	pgraph->cNode       = 0;
	pgraph->cFrom       = 0;
	pgraph->cTo         = 0;
	pgraph->cArc        = 0;

	while ( gnHeapExtractMin( & pgraph->NodeHeap , & heapnode ) == 1 )
	{
		if ( (pnode = heapnode.value.pv) != NULL )
		{
			if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_FROM )
			{
				pgraph->cFrom ++;

				if ( (ptreenode = gnTreeSearch( pgraph->pNodeTree , GNGRP_NODE_ID_v1(pnode) )) == NULL )
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
					 pgraph->iLinkBuffer + GNGRP_LINKAREA_SIZEOF_v1(GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea), pgraph->LinkAttrSize)
					 );

				if ( pgraph->pLinkBuffer == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
					return -pgraph->iErrno;
				}

				memcpy(
						pgraph->pLinkBuffer + pgraph->iLinkBuffer ,
						plinkarea ,
						GNGRP_LINKAREA_SIZEOF_v1(GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea), pgraph->LinkAttrSize)
						);

				GNGRP_NODE_LINKAREA_OFFSET_v1(pnode) = pgraph->iLinkBuffer;

				pgraph->iLinkBuffer += GNGRP_LINKAREA_SIZEOF_v1(GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea), pgraph->LinkAttrSize);
			}

			if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_TO )
			{
				pgraph->cTo ++;
			}

			if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_ALONE )
			{
				pgraph->cAlone ++;
			}

			pgraph->cNode ++;

			pgraph->pNodeBuffer = realloc(pgraph->pNodeBuffer, pgraph->iNodeBuffer + GNGRP_NODE_SIZEOF_v1(pgraph->NodeAttrSize));

			if ( pgraph->pNodeBuffer == NULL )
			{
				pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
				return -pgraph->iErrno;
			}

			memcpy( pgraph->pNodeBuffer + pgraph->iNodeBuffer , pnode , GNGRP_NODE_SIZEOF_v1(pgraph->NodeAttrSize) );
			pgraph->iNodeBuffer += GNGRP_NODE_SIZEOF_v1(pgraph->NodeAttrSize);
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
		pflat1 += GNGRP_NODE_WSIZE_v1(pgraph->NodeAttrSize)
		)
	{
		if ( GNGRP_NODE_STATUS_v1(pflat1) & GNGRP_NS_FROM )
		{
			pflat2 = (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET_v1(pflat1));
			for (
				 i = 0 , pflat3 = GNGRP_LINKAREA_LINK_PTR_v1(pflat2, 0, pgraph->LinkAttrSize) ;
				 i < GNGRP_LINKAREA_LINKCOUNT_v1(pflat2) ;
				 i ++ , pflat3 = GNGRP_LINKAREA_LINK_PTR_v1(pflat2, i, pgraph->LinkAttrSize)
				)
			{
				if ( (pnode = gngrp_get_node_V1( pgraph , GNGRP_LINK_TONODE_OFFSET_v1(pflat3))) == NULL )
				{
					pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
					return -pgraph->iErrno;
				}
				GNGRP_LINK_TONODE_OFFSET_v1(pflat3) = (unsigned long)pnode - (unsigned long)pgraph->pNodeBuffer;
				pgraph->cArc ++;
			}
		}
	}

	return 0;
}

/*
 * SHORTEST PATH CACHE
 */
int gngrp_initialize_sp_cache_V1( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache ) {
	pCache->nFromNode = 0;
	pCache->pvVisited = NULL;
	pCache->pvPredist = NULL;
	gnHeapInit( & pCache->NodeHeap );
	if ( (pCache->pvVisited = gnTreeCreate( gngrp_node_free , NULL )) == NULL ) return -1;
	if ( (pCache->pvPredist = gnTreeCreate( gngrp_node_free , NULL )) == NULL ) return -1;
	return 0;
}

void gngrp_release_sp_cache_V1( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache ) {
	if ( pCache->pvVisited ) gnTreeDestroy( pCache->pvVisited );
	if ( pCache->pvPredist ) gnTreeDestroy( pCache->pvPredist );
	gnHeapFree( & pCache->NodeHeap );
}

gnGrpSPReport_s * gngrp_sp_cache_report( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache, gnInt32_t nFrom, gnInt32_t nTo )
{
	gnTreeNode_s * 	pPredistItem;
	gnInt32_t *		pPredist;
	gnInt32_t *		plink;
	gnInt32_t *		ptonode;
	gnGrpSPArc_s	arc;
	long 			i, istack = 0;
	unsigned char *	pstack = NULL;
	unsigned char *	ppop;
	gnGrpSPReport_s * pReport;

	if ( pCache->nFromNode != nFrom ) {
		pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
		return NULL;
	}

	if ( gnTreeSearch(pCache->pvVisited, nTo) == NULL ) {
		pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
		return NULL;
	}

	for (
			pPredistItem = gnTreeSearch(pCache->pvPredist, nTo) ;
			pPredistItem ;
			pPredistItem = gnTreeSearch(pCache->pvPredist, pPredist[0])
		)
	{
		pPredist = pPredistItem->data.pv;
		if ( pPredist[1] < 0 ) {
			pgraph->iErrno = GNGRP_ERR_BadLink;
			goto spr_error;
		}

		plink   = GNGRP_LINKBUFFER_SHIFT_v1(pgraph, pPredist[1]),
		ptonode = GNGRP_NODEBUFFER_SHIFT_v1(pgraph, GNGRP_LINK_TONODE_OFFSET_v1(plink)) ;

		if ( (arc.Link = GNGRP_LINK_ALLOC_v1( pgraph->LinkAttrSize )) == NULL ) goto spr_error;
		arc.From = pPredist[0];
		arc.To   = GNGRP_NODE_ID_v1(ptonode);
		arc.Distance = pPredist[3];
		memcpy( arc.Link, plink, GNGRP_LINK_SIZEOF_v1( pgraph->LinkAttrSize ) );
		GNGRP_LINK_COST_v1(arc.Link) = pPredist[2];

		if ( (pstack = gngrp_mempush(pstack, & istack, sizeof(gnGrpSPArc_s), & arc)) == NULL ) {
			pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
			goto spr_error;
		}

		if ( arc.From == nFrom ) break;
	}

	if ( pPredistItem == NULL ) {
		pgraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		goto spr_error;
	}

	if ( (pReport = malloc( sizeof( gnGrpSPReport_s ) )) == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		goto spr_error;
	}
	memset( pReport, 0, sizeof( gnGrpSPReport_s ) );

	pReport->cArc = istack;

	if ( (pReport->pArc = malloc( sizeof( gnGrpSPArc_s ) * pReport->cArc )) == NULL ) {
		pgraph->iErrno = GNGRP_ERR_MemoryExhausted;
		goto spr_error;
	}

	pReport->distance = 0;

	for( i = 0 ; (ppop = gngrp_mempop( pstack , & istack , sizeof( gnGrpSPArc_s ) )) != NULL ; i ++ ) {
		memcpy( & pReport->pArc[ i ] , ppop , sizeof( gnGrpSPArc_s ) );
		pReport->distance += GNGRP_LINK_COST_v1(pReport->pArc[i].Link);
	}

	pReport->from = nFrom;
	pReport->to   = nTo;

	if ( pstack ) free( pstack );

	return pReport;

spr_error:
	if (pstack) free(pstack);
	if (pReport) gnGrpFreeSPReport(pgraph, pReport);

	return NULL;
}


/*
 * Dijkstra Shortest Path 
 */
void * gngrp_dijkstra_V1	(
							gnGrpGraph_s * 		pgraph ,
							gnInt32_t 			from ,
							gnInt32_t 			to ,
							gnGrpSPClip_fn		fnClip,
							void * 				pvClipArg,
							gnGrpSPCache_s * 	pCache
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

	gnGrpSPCache_s spCache;

	/*
	 * shortest path distance temporary min heap
	 */
	gnHeapData_u 				heapvalue;
	gnHeapNode_s 				heapnode;

	/*
	 * shortest path visited network
	 */
	gnTreeNode_s * 	pVisitedItem;

	/*
	 * shortest path predecessor and distance network
	 */
	gnTreeNode_s * 	pPredistItem;
	gnInt32_t *		pPredist;

	/*
	 * args to clip()
	 */
	gnGrpSPClipInput_s 	clipInput;
	gnGrpSPClipOutput_s	clipOutput;

	/*
	 * return value
	 */
	gnGrpSPReport_s *	pReport = NULL;


	if ( ! (pgraph->Flags & 0x1) )
	{
		pgraph->iErrno = GNGRP_ERR_BadOnTreeGraph;
		return NULL;
	}

	/* initialize the heap and create temporary networks - The use of a predist network for predecessor and
	   distance has two important results: 1) allows us not having to reset the whole graph status
	   at each call; 2) use of a stack memory area for temporary (and otherwise possibly thread-conflicting)
	   states.
	*/
	if ( pCache == NULL ) {
		pCache = & spCache;
		gngrp_initialize_sp_cache_V1( pgraph, pCache );
		pCache->nFromNode = from;
	}
	else {
		if ( (pReport = gngrp_sp_cache_report( pgraph, pCache, from, to )) != NULL ) {
			return pReport;
		}
		if ( pgraph->iErrno == GNGRP_ERR_FromNodeNotFound ) {
			gngrp_release_sp_cache_V1( pgraph, pCache );
			gngrp_initialize_sp_cache_V1( pgraph, pCache );
			pCache->nFromNode = from;
		}
		else if ( pgraph->iErrno != GNGRP_ERR_ToNodeNotFound ) {
			goto sp_error;
		}
	}

	/*
	 * reset error status after using the cache
	 */
	pgraph->iErrno = 0;

	if ( (pfrom = gngrp_get_node_V1( pgraph , from )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_FromNodeNotFound;
		goto sp_error;
	}
	pfromnode = pfrom;

	if ( (ptonode = gngrp_get_node_V1( pgraph , to )) == NULL )
	{
		pgraph->iErrno = GNGRP_ERR_ToNodeNotFound;
		goto sp_error;
	}

	if ( !(GNGRP_NODE_STATUS_v1(pfromnode) & GNGRP_NS_FROM) ||
		  (GNGRP_NODE_STATUS_v1(pfromnode) & GNGRP_NS_ALONE) ||
		  (GNGRP_NODE_STATUS_v1(ptonode) & GNGRP_NS_ALONE) )
	{
		goto sp_error;
	}

	plinkarea = (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET_v1(pfromnode) );

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
		i = 0,  plink = GNGRP_LINKAREA_LINKARRAY_PTR_v1(plinkarea) ;
		i < GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea) ;
		i ++,   plink += GNGRP_LINK_WSIZE_v1(pgraph->LinkAttrSize)
		)
	{
		ptonode = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET_v1(plink) );

		if ( ! (GNGRP_NODE_STATUS_v1(ptonode) & GNGRP_NS_TO) )
		{
			pgraph->iErrno = GNGRP_ERR_BadLink;
			goto sp_error;
		}

		/*
		 * arc clipping : no previous link in path at first round
		 */
		clipOutput.nLinkCost = GNGRP_LINK_COST_v1(plink);

		if ( fnClip )
		{
			clipInput.pnPrevLink 	= NULL;
			clipInput.pnNodeFrom 	= pfromnode;
			clipInput.pnLink		= plink;
			clipInput.pnNodeTo		= ptonode;
			clipInput.nFromDistance = 0;

			if ( fnClip( pgraph , & clipInput , & clipOutput , pvClipArg ) ) continue;
		}

		/*
		 * Setup the predist for this node
		 */
		pPredistItem = gngrp__node( pCache->pvPredist, GNGRP_NODE_ID_v1(ptonode) );
		if ( pPredistItem == NULL ) goto sp_error;

		if ( (pPredist = pPredistItem->data.pv) == NULL ) {
			if ( (pPredist = malloc( sizeof(gnInt32_t) * 4 )) == NULL ) goto sp_error;
			pPredistItem->data.pv = pPredist;
		}
		pPredist[0] = from;	/* previous node */
		pPredist[1] = GNGRP_LINKBUFFER_OFFSET_v1(pgraph,plink);	/* previous link */
		pPredist[2] = clipOutput.nLinkCost;	/* current real cost */
		pPredist[3] = clipOutput.nLinkCost;	/* distance */

		/* store the link offset into the min-distance heap */
		heapvalue.l = pPredist[1];
		/*printf( "add node %ld to heap with distance %ld\n", GNGRP_NODE_ID_v1(ptonode), pPredist[3] );*/
		if ( gnHeapInsertMin( & pCache->NodeHeap, pPredist[3] , heapvalue ) < 0 )
		{
			pgraph->iErrno = GNGRP_ERR_HeapError;
			goto sp_error;
		}
	}

	/*
	 * Now we begin extracting nodes from the min-heap. Each node extracted is
	 * the one that is actually closest to the SP starting point 'from'.
	 */
	while( gnHeapExtractMin( & pCache->NodeHeap , &heapnode) == 1 )
	{
		gnInt32_t fromDist;
		/*
		 * recover the stored link pointer
		 */
		plink   	= (gnInt32_t*) (pgraph->pLinkBuffer + heapnode.value.l);
		/*
		 * the new relative from-node is the to-node of the link
		 */
		pfromnode 	= (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET_v1(plink) );
		/*
		 * get the linkarea of the new from-node
		 */
		plinkarea 	= (gnInt32_t*) (pgraph->pLinkBuffer + GNGRP_NODE_LINKAREA_OFFSET_v1(pfromnode) );

		/*
		 * We do not want to explore twice the same node as a relative starting point,
		 * that's the meaning of 'visited'. We mark actual from node as 'visited' by
		 * inserting it into the visited-network. If we find actual node in the network
		 * we just give up and continue looping. Otherwise we add actual node to the network.
		 */
		if ( (pVisitedItem = gnTreeSearch( pCache->pvVisited , GNGRP_NODE_ID_v1(pfromnode))) == NULL ) {
			if ( gngrp__node( pCache->pvVisited, GNGRP_NODE_ID_v1(pfromnode) ) == NULL ) {
				goto sp_error;
			}
		}

		/*
		 * Dijkstra algorithm ends when the destination node 'to' is extracted from
		 * the min distance heap, that means: no other path exist in the network giving
		 * a shortest output.
		 * If this happens we jump to the epilogue in order to build a path report and return.
		 */
		if ( GNGRP_NODE_ID_v1(pfromnode) == to )
		{
			ptofound = plink;
			goto to_found;
		}

		if ( pVisitedItem ) {
			continue;
		}

		/*
		 * If the from-node is not marked as having departing links, then we are into a
		 * blind alley. Just give up this direction and continue looping.
		 */
		if ( ! (GNGRP_NODE_STATUS_v1(pfromnode) & GNGRP_NS_FROM) )  continue;


		/*
		 * save actual link for later clip()
		 */
		plink_prev = plink;

		/*
		 * Recover the from node distance from the predist network
		 */
		if ( (pPredistItem = gnTreeSearch( pCache->pvPredist, GNGRP_NODE_ID_v1(pfromnode) )) == NULL ) goto sp_error;
		if ( (pPredist = pPredistItem->data.pv) == NULL ) goto sp_error;
		fromDist = pPredist[3];

		/*
		 * Loop on departing links
		 */
		for (
			i = 0,  plink = GNGRP_LINKAREA_LINKARRAY_PTR_v1(plinkarea), i = 0;
			i < GNGRP_LINKAREA_LINKCOUNT_v1(plinkarea);
			i ++,   plink += GNGRP_LINK_WSIZE_v1(pgraph->LinkAttrSize)
			)
		{
			ptonode = (gnInt32_t*) (pgraph->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET_v1(plink) );
		
			if ( ! (GNGRP_NODE_STATUS_v1(ptonode) & GNGRP_NS_TO) )
			{
				pgraph->iErrno = GNGRP_ERR_BadLink;
				goto sp_error;
			}
		
			/*
			 * arc clipping : we now have previous link and from distance
			 */
			clipOutput.nLinkCost = GNGRP_LINK_COST_v1(plink);

			if ( fnClip )
			{
				clipInput.pnPrevLink 	= plink_prev;
				clipInput.pnNodeFrom 	= pfromnode;
				clipInput.pnLink		= plink;
				clipInput.pnNodeTo		= ptonode;
				clipInput.nFromDistance = fromDist;

				if ( fnClip( pgraph , & clipInput , & clipOutput , pvClipArg ) ) continue;
			}

			if ( (pPredistItem = gnTreeSearch( pCache->pvPredist, GNGRP_NODE_ID_v1(ptonode) )) == NULL ) {
				if ( (pPredistItem = gngrp__node( pCache->pvPredist, GNGRP_NODE_ID_v1(ptonode) )) == NULL ) goto sp_error;
				if ( (pPredist = malloc( sizeof(gnInt32_t) * 4 )) == NULL ) goto sp_error;
				pPredistItem->data.pv = pPredist;
			}
			else {
				pPredist = pPredistItem->data.pv;
				if ( pPredist[3] <= fromDist + clipOutput.nLinkCost ) {
					continue;
				}
			}
			pPredist[0] = GNGRP_NODE_ID_v1(pfromnode);	/* previous node */
			pPredist[1] = GNGRP_LINKBUFFER_OFFSET_v1(pgraph,plink);	/* previous link */
			pPredist[2] = clipOutput.nLinkCost;	/* current real cost */
			pPredist[3] = fromDist + clipOutput.nLinkCost;	/* distance */

			/*printf( "add node %ld to heap with distance %ld\n", GNGRP_NODE_ID_v1(ptonode), pPredist[3] );*/
			heapvalue.l = pPredist[1];
			if ( gnHeapInsertMin( & pCache->NodeHeap, pPredist[3] , heapvalue ) < 0 )
			{
				pgraph->iErrno = GNGRP_ERR_HeapError;
				goto sp_error;
			}
		}
	}

sp_error:
	if ( pCache == &spCache ) {
		gngrp_release_sp_cache_V1( pgraph, pCache );
	}
	return NULL; /* if pgraph->iErrno == 0 then path not found, else report an error condition */

to_found: /* path found - build a shortest path report */

	pReport = gngrp_sp_cache_report( pgraph, pCache, from, to );
	if ( pCache == &spCache ) {
		gngrp_release_sp_cache_V1( pgraph, pCache );
	}
	return pReport;
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
int gngrp_depthfirst_spanning_V1(
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

	/*
	 * the simplest case is when vertex node is alone or has no outgoing links, the result
	 * of the spanning is a graph having only one node
	 */
	if ( GNGRP_NODE_STATUS_v1(pfrom) & GNGRP_NS_ALONE ||
		 (!(GNGRP_NODE_STATUS_v1(pfrom) & GNGRP_NS_FROM) && GNGRP_NODE_STATUS_v1(pfrom) & GNGRP_NS_TO) ) {
		nret = gngrp_add_node_V1(
					pgraphOut,
					GNGRP_NODE_ID_v1(pfrom),
					GNGRP_NODE_ATTR_PTR_v1(pfrom),
					0
				);
		if ( nret < 0 ) {
			goto dfs_error;
		}
		return 0;
	}

	if ( GNGRP_NODE_STATUS_v1(pfrom) & GNGRP_NS_FROM ) {
		plinkarea = GNGRP_LINKBUFFER_SHIFT_v1(pgraphIn, GNGRP_NODE_LINKAREA_OFFSET_v1(pfrom));
		GNGRP_FOREACH_LINK_v1(pgraphIn, plinkarea, plink) {
			id[0] = GNGRP_NODEBUFFER_OFFSET_v1(pgraphIn, pfrom);
	   		id[1] = GNGRP_LINKBUFFER_OFFSET_v1(pgraphIn, plink);
			if ( (pstack = gngrp_mempush( pstack , & istack , sizeof( gnInt32_t ) * 2 , id )) == NULL ) {
				pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
			}
		}
		if ( gngrp__node(pvVisited , GNGRP_NODE_ID_v1(pfrom)) == NULL ) {
			pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
		}
	}

	while( (pid = (gnInt32_t*)gngrp_mempop( pstack , & istack , sizeof(gnInt32_t)  * 2)) != NULL )
	{
		pfrom  = (gnInt32_t*)(pgraphIn->pNodeBuffer + pid[0]);
		plink  = (gnInt32_t*)(pgraphIn->pLinkBuffer + pid[1]);
		pto = (gnInt32_t*)(pgraphIn->pNodeBuffer + GNGRP_LINK_TONODE_OFFSET_v1(plink));

		if ( gnTreeSearch( pvVisited , GNGRP_NODE_ID_v1(pto) ) ) { /* already visited */
			continue;
		}

		if ( fnClip ) {
			clipInput.pnNodeFrom = pfrom;
			clipInput.pnLink     = plink;
			clipInput.pnNodeTo   = pto;
			if ( fnClip( pgraphIn, pgraphOut, & clipInput, NULL, pvClipArg ) ) continue;
		}

		if ( gngrp__node(pvVisited , GNGRP_NODE_ID_v1(pto)) == NULL ) {
			pgraphIn->iErrno = GNGRP_ERR_MemoryExhausted; goto dfs_error;
		}

		/* add this link */
		nret = gngrp_add_link_V1( pgraphOut,
					GNGRP_NODE_ID_v1(pfrom),
					GNGRP_NODE_ID_v1(pto),
					GNGRP_LINK_COST_v1(plink),
					GNGRP_LINK_USER_v1(plink),
					GNGRP_NODE_ATTR_PTR_v1(pfrom),
					GNGRP_NODE_ATTR_PTR_v1(pto),
					GNGRP_LINK_ATTR_PTR_v1(plink),
					0
					);

		if ( nret < 0 ) {
			goto dfs_error;
		}

		if ( GNGRP_NODE_STATUS_v1(pto) & GNGRP_NS_FROM ) {
			plinkarea = GNGRP_LINKBUFFER_SHIFT_v1(pgraphIn, GNGRP_NODE_LINKAREA_OFFSET_v1(pto));

			GNGRP_FOREACH_LINK_v1(pgraphIn, plinkarea, plink) {
				id[0] = GNGRP_NODEBUFFER_OFFSET_v1(pgraphIn, pto);
   				id[1] = GNGRP_LINKBUFFER_OFFSET_v1(pgraphIn, plink);
				if ( (pstack = gngrp_mempush( pstack , & istack , sizeof( gnInt32_t ) * 2 , id )) == NULL ) {
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


int gngrp_release_V1( gnGrpGraph_s * pgraph )
{
	pgraph->iErrno = 0;

	if ( pgraph->pNodeTree ) gnTreeDestroy( pgraph->pNodeTree );

	gnHeapFree( & pgraph->NodeHeap );

	if ( pgraph->pNodeBuffer ) free( pgraph->pNodeBuffer );
	if ( pgraph->pLinkBuffer ) free( pgraph->pLinkBuffer );

	return 0;
}


int gngrp_write_V1( gnGrpGraph_s * pgraph , int fd )
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

	if ( write( fd , & pgraph->cAlone , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
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


int gngrp_read_V1( gnGrpGraph_s * pgraph , int fd )
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
	if ( fSwap ) gngrp_swapInt32Bytes( & NodeAttrSize );

	if ( read( fd , & LinkAttrSize , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & LinkAttrSize );

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
		if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->aOpaqueSet[ cnt ] );
	}

	if ( read( fd , & pgraph->cNode , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->cNode );

	if ( read( fd , & pgraph->cFrom , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->cFrom );

	if ( read( fd , & pgraph->cTo , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->cTo );

	if ( read( fd , & pgraph->cAlone , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->cAlone );

	if ( read( fd , & pgraph->cArc , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->cArc );

	if ( read( fd , & pgraph->iNodeBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->iNodeBuffer );

	if ( read( fd , & pgraph->iLinkBuffer , sizeof( gnInt32_t ) ) != sizeof( gnInt32_t ) )
	{
		pgraph->iErrno = GNGRP_ERR_Read;
		return -pgraph->iErrno;
	}
	if ( fSwap ) gngrp_swapInt32Bytes( & pgraph->iLinkBuffer );

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
			gngrp_swapInt32Bytes( & pn[i] );
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
			gngrp_swapInt32Bytes( & pn[i] );
		}
	}

	pgraph->Flags |= 0x1; /* flat-state */
	return 0;
}
