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
#ifdef GNGRP_V2
#include "graph_v2.h"
#endif


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

int gnGrpInitialize(gnGrpGraph_s * pGraph, gnByte_t Version, gnInt32_t NodeAttrSize, gnInt32_t LinkAttrSize, gnInt32_t * pOpaqueSet)
{
	if ( pGraph == NULL ) {
		return -GNGRP_ERR_UnexpectedNullPointer;
	}
	switch( Version )
	{
	case 1:
#ifdef GNGRP_V2
	case 2:
#endif
		memset( pGraph , 0 , sizeof( gnGrpGraph_s ) );
		gnHeapInit( & pGraph->NodeHeap );
		/*
	   	 * round attr size to the upper multiple of gnInt32_t size
	 	 */
		if ( NodeAttrSize % sizeof(gnInt32_t) ) NodeAttrSize += ( sizeof(gnInt32_t) - (NodeAttrSize % sizeof(gnInt32_t)) );
		if ( LinkAttrSize % sizeof(gnInt32_t) ) LinkAttrSize += ( sizeof(gnInt32_t) - (LinkAttrSize % sizeof(gnInt32_t)) );
		pGraph->Version  = Version;
		pGraph->NodeAttrSize = NodeAttrSize;
		pGraph->LinkAttrSize = LinkAttrSize;
		if ( pOpaqueSet ) memcpy( & pGraph->aOpaqueSet , pOpaqueSet , sizeof( gnInt32_t ) * 16 );
#ifdef GN_ENDIAN_BIG
		pGraph->Endian = GNGRP_ENDIAN_BIG;
#else
		pGraph->Endian = GNGRP_ENDIAN_LITTLE;
#endif
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_VersionNotSupported;
	return -pGraph->iErrno;
}

int gnGrpRelease( gnGrpGraph_s * pGraph )
{
	switch( pGraph->Version ) {
	case 1: return gngrp_release_V1(pGraph);
#ifdef GNGRP_V2
	case 2: return gngrp_release_V2(pGraph);
#endif
	}
	pGraph->iErrno = GNGRP_ERR_BadVersion;
	return -pGraph->iErrno;
}

int gnGrpUnflatten( gnGrpGraph_s * pGraph )
{
	switch( pGraph->Version ) {
	case 1: return gngrp_unflatten_V1(pGraph);
#ifdef GNGRP_V2
	case 2: return gngrp_unflatten_V2(pGraph);
#endif
	}
	pGraph->iErrno = GNGRP_ERR_BadVersion;
	return -pGraph->iErrno;
}


int gnGrpFlatten( gnGrpGraph_s * pGraph )
{
	switch( pGraph->Version ) {
	case 1: return gngrp_flatten_V1(pGraph);
#ifdef GNGRP_V2
	case 2: return gngrp_flatten_V2(pGraph);
#endif
	}
	pGraph->iErrno = GNGRP_ERR_BadVersion;
	return -pGraph->iErrno;
}


gnInt32_t * gnGrpGetNode(gnGrpGraph_s * pGraph , gnInt32_t nNodeId)
{
	switch( pGraph->Version ) {
	case 1: return gngrp_get_node_V1(pGraph, nNodeId);
#ifdef GNGRP_V2
	case 2: return gngrp_get_node_V2(pGraph, nNodeId);
#endif
	}
	pGraph->iErrno = GNGRP_ERR_BadVersion;
	return NULL;
}

gnInt32_t *	gnGrpGetNode_OutLinkarea(gnGrpGraph_s * pGraph, gnInt32_t * pnNode)
{
	if ( pnNode) {
		switch( pGraph->Version ) {
		case 1: return gngrp_getnode_outlinkarea_V1(pGraph, pnNode);
#ifdef GNGRP_V2
		case 2: return gngrp_getnode_outlinkarea_V2(pGraph, pnNode);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	return NULL;
}

gnInt32_t *	gnGrpGetNode_InLinkarea(gnGrpGraph_s * pGraph, gnInt32_t * pnNode)
{
	if ( pnNode) {
		switch( pGraph->Version ) {
		case 1: pGraph->iErrno = GNGRP_ERR_NotSupported; return NULL;
#ifdef GNGRP_V2
		case 2: return gngrp_getnode_inlinkarea_V2(pGraph, pnNode);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	return NULL;
}



/*
 * Given that node id can be negative, only iErrno can report a error,
 * thus it is initialized to zero
 */
gnInt32_t	gnGrpGetNode_Id(gnGrpGraph_s * pGraph, gnInt32_t * pnNode)
{
	pGraph->iErrno = 0;
	if (pnNode) {
		switch( pGraph->Version ) {
		case 1: return GNGRP_NODE_ID_v1(pnNode);
#ifdef GNGRP_V2
		case 2: return GNGRP_NODE_ID_v2(pnNode);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}


gnInt32_t	gnGrpGetNode_Status(gnGrpGraph_s * pGraph, gnInt32_t * pnNode)
{
	pGraph->iErrno = 0;
	if (pnNode) {
		switch( pGraph->Version ) {
		case 1: return GNGRP_NODE_STATUS_v1(pnNode);
#ifdef GNGRP_V2
		case 2: return GNGRP_NODE_STATUS_v2(pnNode);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}


gnInt32_t *	gnGrpGetNode_Attr(gnGrpGraph_s * pGraph, gnInt32_t * pnNode)
{
	if ( pnNode) {
		switch( pGraph->Version ) {
		case 1: return GNGRP_NODE_ATTR_PTR_v1(pnNode);
#ifdef GNGRP_V2
		case 2: return GNGRP_NODE_ATTR_PTR_v2(pnNode);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}


int	gnGrpSetNode_Attr(gnGrpGraph_s * pGraph, gnInt32_t * pnNode, gnInt32_t * pnAttr)
{
	if ( pnNode) {
		switch( pGraph->Version ) {
		case 1: memcpy(GNGRP_NODE_ATTR_PTR_v1(pnNode), pnAttr, pGraph->NodeAttrSize);
				return 0;
#ifdef GNGRP_V2
		case 2: memcpy(GNGRP_NODE_ATTR_PTR_v2(pnNode), pnAttr, pGraph->NodeAttrSize);
				return 0;
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return -pGraph->iErrno;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return -pGraph->iErrno;
}


gnInt32_t gnGrpGetOutLink_Count(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea)
{
	pGraph->iErrno = 0;
	if ( pnOutLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			return GNGRP_LINKAREA_LINKCOUNT_v1(pnOutLinkarea);
#ifdef GNGRP_V2
		case 2:
			return GNGRP_LINKAREA_LINKCOUNT_v2(pnOutLinkarea);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}

gnInt32_t *	gnGrpGetOutLink_ByNode(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea, gnInt32_t nToNode)
{
	gnInt32_t * pnLink;

	if ( pnOutLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			GNGRP_FOREACH_LINK_v1(pGraph, pnOutLinkarea, pnLink) {
				if ( GNGRP_LINK_TONODE_ID_v1(pGraph,pnLink) == nToNode ) return pnLink;
			}	
			pGraph->iErrno = GNGRP_ERR_LinkNotFound;
			return NULL;
#ifdef GNGRP_V2
		case 2:
			GNGRP_FOREACH_LINK_v2(pGraph, pnOutLinkarea, pnLink) {
				if ( GNGRP_LINK_TONODE_ID_v2(pGraph,pnLink) == nToNode ) return pnLink;
			}	
			pGraph->iErrno = GNGRP_ERR_LinkNotFound;
			return NULL;
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}


gnInt32_t *	gnGrpGetOutLink_ByUserId(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea, gnInt32_t nUserId)
{
	gnInt32_t * pnLink;

	if ( pnOutLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			GNGRP_FOREACH_LINK_v1(pGraph, pnOutLinkarea, pnLink) {
				if ( GNGRP_LINK_USER_v1(pnLink) == nUserId ) return pnLink;
			}	
			pGraph->iErrno = GNGRP_ERR_LinkNotFound;
			return NULL;
#ifdef GNGRP_V2
		case 2:
			GNGRP_FOREACH_LINK_v2(pGraph, pnOutLinkarea, pnLink) {
				if ( GNGRP_LINK_USER_v2(pnLink) == nUserId ) return pnLink;
			}	
			pGraph->iErrno = GNGRP_ERR_LinkNotFound;
			return NULL;
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}


gnInt32_t *	gnGrpGetOutLink_ByIndex(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea, int iLink)
{
	if ( pnOutLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			return GNGRP_LINKAREA_LINK_PTR_v1(pnOutLinkarea,iLink,pGraph->LinkAttrSize);
#ifdef GNGRP_V2
		case 2:
			return GNGRP_LINKAREA_LINK_PTR_v2(pnOutLinkarea,iLink,pGraph->LinkAttrSize);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}

gnInt32_t gnGrpGetInLink_Count(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea)
{
	pGraph->iErrno = 0;
	if ( pnInLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			pGraph->iErrno = GNGRP_ERR_NotSupported;
			return 0;
#ifdef GNGRP_V2
		case 2:
			return GNGRP_LINKAREA_INLINKCOUNT_v2(pnInLinkarea);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}


gnInt32_t *	gnGrpGetInLink_ByNode(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea, gnInt32_t nFromNode)
{

	if ( pnInLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			pGraph->iErrno = GNGRP_ERR_NotSupported;
			return NULL;
#ifdef GNGRP_V2
		case 2:
			GNGRP_FOREACH_INLINK_v2(pGraph, pnLinkarea, pnLink) {
				if ( GNGRP_LINK_FROMNODE_ID_v2(pnLink) == nFromNode ) return pnLink;
			}	
			pGraph->iErrno = GNGRP_ERR_LinkNotFound;
			return NULL;
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}


gnInt32_t *	gnGrpGetInLink_ByUserId(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea, gnInt32_t nUserId)
{

	if ( pnInLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			pGraph->iErrno = GNGRP_ERR_NotSupported;
			return NULL;
#ifdef GNGRP_V2
		case 2:
			GNGRP_FOREACH_INLINK_v2(pGraph, pnLinkarea, pnLink) {
				if ( GNGRP_LINK_USER_v2(pnLink) == nUserId ) return pnLink;
			}	
			pGraph->iErrno = GNGRP_ERR_LinkNotFound;
			return NULL;
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}

gnInt32_t *	gnGrpGetInLink_ByIndex(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea, int iLink)
{
	if ( pnInLinkarea ) {
		switch( pGraph->Version ) {
		case 1:
			pGraph->iErrno = GNGRP_ERR_NotSupported;
			return NULL;
#ifdef GNGRP_V2
		case 2:
			return GNGRP_LINKAREA_INLINK_PTR_v2(pnInLinkarea,iLink,pGraph->LinkAttrSize);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}

gnInt32_t gnGrpGetLink_Cost( gnGrpGraph_s * pGraph , gnInt32_t * pnLink )
{
	pGraph->iErrno = 0;
	if (pnLink) {
		switch(pGraph->Version) {
		case 1: return GNGRP_LINK_COST_v1(pnLink);
#ifdef GNGRP_V2
		case 2: return GNGRP_LINK_COST_v2(pnLink);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}

gnInt32_t gnGrpGetLink_UserId( gnGrpGraph_s * pGraph , gnInt32_t * pnLink )
{
	pGraph->iErrno = 0;
	if (pnLink) {
		switch(pGraph->Version) {
		case 1: return GNGRP_LINK_USER_v1(pnLink);
#ifdef GNGRP_V2
		case 2: return GNGRP_LINK_USER_v2(pnLink);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}

gnInt32_t * gnGrpGetLink_FromNode( gnGrpGraph_s * pGraph , gnInt32_t * pnLink )
{
	if (pnLink) {
		switch(pGraph->Version) {
#ifdef GNGRP_V2
		case 2:
			return GNGRP_LINK_FROMNODE_v2(pnLink);
#endif
		case 1:
			pGraph->iErrno = GNGRP_ERR_NotSupported;
			return NULL;
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}

gnInt32_t * gnGrpGetLink_ToNode( gnGrpGraph_s * pGraph , gnInt32_t * pnLink )
{
	if (pnLink) {
		switch(pGraph->Version) {
		case 1:
			if ( pGraph->Flags & GNGRP_GS_FLAT ) {
				return GNGRP_NODEBUFFER_SHIFT_v1(pGraph,GNGRP_LINK_TONODE_OFFSET_v1(pnLink));
			}
			else {
				return gngrp_get_node_V1(pGraph, GNGRP_LINK_TONODE_OFFSET_v1(pnLink));
			}
#ifdef GNGRP_V2
		case 2:
			if ( pGraph->Flags & GNGRP_GS_FLAT ) {
				return GNGRP_NODEBUFFER_SHIFT_v2(pGraph,GNGRP_LINK_TONODE_OFFSET_v2(pnLink));
			}
			else {
				return gngrp_get_node_V2(pGraph, GNGRP_LINK_TONODE_OFFSET_v2(pnLink));
			}
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return 0;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return 0;
}

gnInt32_t * gnGrpGetLink_Attr( gnGrpGraph_s * pGraph , gnInt32_t * pnLink )
{
	if (pnLink) {
		switch(pGraph->Version) {
		case 1: return GNGRP_LINK_ATTR_PTR_v1(pnLink);
#ifdef GNGRP_V2
		case 2: return GNGRP_LINK_ATTR_PTR_v2(pnLink);
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return NULL;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return NULL;
}

int gnGrpSetLink_Attr( gnGrpGraph_s * pGraph , gnInt32_t * pnAttr , gnInt32_t * pnLink )
{
	if (pnLink) {
		switch( pGraph->Version ) {
		case 1: memcpy(GNGRP_LINK_ATTR_PTR_v1(pnLink), pnAttr, pGraph->LinkAttrSize);
				return 0;
#ifdef GNGRP_V2
		case 2: memcpy(GNGRP_LINK_ATTR_PTR_v2(pnLink), pnAttr, pGraph->LinkAttrSize);
				return 0;
#endif
		}
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		return -pGraph->iErrno;
	}
	pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
	return -pGraph->iErrno;
}



int gnGrpNodeScan(
			gnGrpGraph_s * pGraph ,
			gnGrpScan_fn fnScan ,
			void * pvArg
			)
{
	gnInt32_t * pnNode;
	int			nRet;

	if ( fnScan == NULL ) {
		pGraph->iErrno = GNGRP_ERR_UnexpectedNullPointer;
		return -pGraph->iErrno;
	}

	if ( !(pGraph->Flags & 0x1) ) {
		pGraph->iErrno = GNGRP_ERR_BadOnTreeGraph;
		return -pGraph->iErrno;
	}

	switch( pGraph->Version ) {
	case 1:
		GNGRP_FOREACH_NODE_v1(pGraph, pnNode) {
			if ( (nRet = fnScan( pGraph, pnNode, pvArg )) ) return nRet;	
		}
		return 0;
#ifdef GNGRP_V2
	case 2:
		GNGRP_FOREACH_NODE_v2(pGraph, pnNode) {
			if ( (nRet = fnScan( pGraph, pnNode, pvArg )) ) return nRet;	
		}
		return 0;
#endif
	}
	pGraph->iErrno = GNGRP_ERR_BadVersion;
	return -pGraph->iErrno;
}



int gnGrpAddLink(
				gnGrpGraph_s * 	pGraph ,
				gnInt32_t 		nFrom ,
				gnInt32_t 		nTo ,
				gnInt32_t 		nCost ,
				gnInt32_t 		nUser
				)
{
	int 		nRet;
#ifdef GNGRP_STATS
	clock_t clk;
	clk = clock();
	pGraph->cAddLink ++;
#endif
	switch( pGraph->Version ) {
	case 1:
		nRet = gngrp_add_link_V1(pGraph, nFrom, nTo, nCost, nUser, NULL, NULL, NULL, 0);
		break;
#ifdef GNGRP_V2
	case 2:
		nRet = gngrp_add_link_V2(pGraph, nFrom, nTo, nCost, nUser, NULL, NULL, NULL, 0);
		break;
#endif
	default:
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		nRet = -pGraph->iErrno;
		break;
	}
#ifdef GNGRP_STATS
	pGraph->clkAddLink += clock() - clk;
#endif
	return nRet;
}

int gnGrpAddLinkX	(
				gnGrpGraph_s * 	pGraph ,
				gnInt32_t 		nFrom ,
				gnInt32_t 		nTo ,
				gnInt32_t 		nCost ,
				gnInt32_t 		nUser ,
				void *			pvFnodeAttr ,
				void *			pvTnodeAttr ,
				void *			pvLinkAttr ,
				gnInt32_t		nFlags
				)
{
	int 		nRet;
#ifdef GNGRP_STATS
	clock_t clk;
	clk = clock();
	pGraph->cAddLink ++;
#endif
	switch( pGraph->Version ) {
	case 1:
		nRet = gngrp_add_link_V1(pGraph, nFrom, nTo, nCost, nUser, pvFnodeAttr, pvTnodeAttr, pvLinkAttr, nFlags);
		break;
#ifdef GNGRP_V2
	case 2:
		nRet = gngrp_add_link_V2(pGraph, nFrom, nTo, nCost, nUser, pvFnodeAttr, pvTnodeAttr, pvLinkAttr, nFlags);
		break;
#endif
	default:
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		nRet = -pGraph->iErrno;
		break;
	}
#ifdef GNGRP_STATS
	pGraph->clkAddLink += clock() - clk;
#endif
	return nRet;
}

int gnGrpAddNode(
				gnGrpGraph_s * 	pGraph ,
				gnInt32_t 		nNodeId ,
				void *			pvNodeAttr ,
				gnInt32_t		nFlags
				)
{
	int 		nRet;
	switch( pGraph->Version ) {
	case 1:
		nRet = gngrp_add_node_V1(pGraph, nNodeId, pvNodeAttr, nFlags);
		break;
#ifdef GNGRP_V2
	case 2:
		nRet = gngrp_add_node_V2(pGraph, nNodeId, pvNodeAttr, nFlags);
		break;
#endif
	default:
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		nRet = -pGraph->iErrno;
		break;
	}
	return nRet;
}

int gnGrpWrite( gnGrpGraph_s * pGraph, int fd )
{
	int nRet;

	switch( pGraph->Version ) {
	case 1:
		nRet = gngrp_write_V1(pGraph, fd);
		break;
#ifdef GNGRP_V2
	case 2:
		nRet = gngrp_write_V2(pGraph, fd);
		break;
#endif
	default:
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		nRet = -pGraph->iErrno;
		break;
	}
	return nRet;
}

int gnGrpRead( gnGrpGraph_s * pGraph, int fd )
{
	gnByte_t bVersion;
	int	nRet;

	if ( read( fd , & bVersion , 1 ) != 1 ) {
		pGraph->iErrno = GNGRP_ERR_Read;
		nRet = -pGraph->iErrno;
	}
	else {
		switch( bVersion ) {
		case 1:
			nRet = gngrp_read_V1( pGraph, fd );
			break;
#ifdef GNGRP_V2
		case 2:
			nRet = gngrp_read_V2( pGraph, fd );
			break;
#endif
		default:
			pGraph->iErrno = GNGRP_ERR_VersionNotSupported;
			nRet = -pGraph->iErrno;
			break;
		}
	}
	return nRet;
}


gnGrpSPReport_s * gnGrpShortestPath	(
								 	gnGrpGraph_s * 	 pGraph,
								 	gnInt32_t 		 nFrom,
								 	gnInt32_t 		 nTo,
									gnGrpSPClip_fn	 fnClip,
									void *			 pvClipArg,
									gnGrpSPCache_s * pCache
								 	)
{
	gnGrpSPReport_s * pRet;

	pRet = NULL;
	switch( pGraph->Version ) {
	case 1:
		pRet = gngrp_dijkstra_V1( pGraph, nFrom, nTo, fnClip, pvClipArg, pCache );
		break;
#ifdef GNGRP_V2
	case 2:
		pRet = gngrp_dijkstra_V2( pGraph, nFrom, nTo, fnClip, pvClipArg, pCache );
		break;
#endif
	default:
		pGraph->iErrno = GNGRP_ERR_BadVersion;
		break;
	}
	return pRet;
}


int	gnGrpDepthSpanning	(
						gnGrpGraph_s *  	pgraphInput,
						gnGrpGraph_s *  	pgraphOutput,
						gnInt32_t			nVertexNode,
						gnGrpSpanClip_fn	fnClip,
						void *				pvClipArg
						)
{
	int nRet;
	void * pvVisited;

	if ( gnGrpGet_LinkCount(pgraphInput) == 0 ) { /* no span */
		pgraphInput->iErrno = 0;
		return 0;
	}

#ifndef GNGRP_V2
	if ( pgraphInput->Version == 2 ) {
		pgraphInput->iErrno = GNGRP_ERR_BadVersion;
		return -pgraphInput->iErrno;
	}
#endif

	nRet = gnGrpInitialize( pgraphOutput,
			gnGrpGet_Version(pgraphInput),
			gnGrpGet_NodeAttrSize(pgraphInput),
			gnGrpGet_LinkAttrSize(pgraphInput), 
			gnGrpGet_Opaque(pgraphInput) );

	if ( nRet < 0 ) return nRet;

	if ( (pvVisited = gnTreeCreate( gngrp_node_free , NULL )) == NULL ) {
		pgraphInput->iErrno = GNGRP_ERR_MemoryExhausted;
		return -pgraphInput->iErrno;
	}

	switch( pgraphInput->Version ) {
	case 1:
		nRet = gngrp_depthfirst_spanning_V1( pgraphInput, pgraphOutput, nVertexNode , pvVisited , fnClip , pvClipArg );
		break;
#ifdef GNGRP_V2
	case 2:
		nRet = gngrp_depthfirst_spanning_V2( pgraphInput, pgraphOutput, nVertexNode , pvVisited , fnClip , pvClipArg );
		break;
#endif
	default:
		pgraphInput->iErrno = GNGRP_ERR_BadVersion;
		nRet = -pgraphInput->iErrno;
		break;
	}
	gnTreeDestroy( pvVisited );
	if ( nRet < 0 ) {
		gnGrpRelease( pgraphOutput );
	}
	return nRet;
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

#ifndef GNGRP_V2
	if ( pgraphInput->Version == 2 ) {
		pgraphInput->iErrno = GNGRP_ERR_BadVersion;
		return -pgraphInput->iErrno;
	}
#endif

	if ( (pvVisited = gnTreeCreate( gngrp_node_free , NULL )) == NULL ) {
		pgraphInput->iErrno = GNGRP_ERR_MemoryExhausted; goto error;
	}

	/*
	 * choose a vertex to start from
	 */
	pvertex = NULL;
	switch(pgraphInput->Version) {
	case 1:
		GNGRP_FOREACH_NODE_v1(pgraphInput, pnode) {
			if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_FROM ) {
				pvertex = pnode;
				break;
			}
		}
		break;
#ifdef GNGRP_V2
	case 2:
		GNGRP_FOREACH_NODE_v2(pgraphInput, pnode) {
			if ( GNGRP_NODE_STATUS_v2(pnode) & GNGRP_NS_FROM ) {
				pvertex = pnode;
				break;
			}
		}
		break;
#endif
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

		switch( pgraphInput->Version ) {
		case 1:
			nret = gngrp_depthfirst_spanning_V1(pgraphInput, & pgraphComponents[i], GNGRP_NODE_ID_v1(pvertex), pvVisited, fnClip, pvClipArg);
			if ( nret < 0 ) goto error;
			break;
#ifdef GNGRP_V2
		case 2:
			nret = gngrp_depthfirst_spanning_V2(pgraphInput, & pgraphComponents[i], GNGRP_NODE_ID_v1(pvertex), pvVisited, fnClip, pvClipArg);
			if ( nret < 0 ) goto error;
			break;
#endif
		default:
			pgraphInput->iErrno = GNGRP_ERR_BadVersion;
			nret = -pgraphInput->iErrno;
			goto error;
		}
		
		/*
		 * select next unvisited vertex
		 */
		switch( pgraphInput->Version ) {
		case 1:
			pvertex = NULL;
			GNGRP_FOREACH_NODE_v1(pgraphInput, pnode) {
				if ( GNGRP_NODE_STATUS_v1(pnode) & GNGRP_NS_FROM ) {
					if ( gnTreeSearch( pvVisited, GNGRP_NODE_ID_v1(pnode) ) == NULL ) {
						pvertex = pnode;
						break;
					}
				}
			}
			break;
#ifdef GNGRP_V2
		case 2:
			pvertex = NULL;
			GNGRP_FOREACH_NODE_v2(pgraphInput, pnode) {
				if ( GNGRP_NODE_STATUS_v2(pnode) & GNGRP_NS_FROM ) {
					if ( gnTreeSearch( pvVisited, GNGRP_NODE_ID_v2(pnode) ) == NULL ) {
						pvertex = pnode;
						break;
					}
				}
			}
			break;
#endif
		default:
			pgraphInput->iErrno = GNGRP_ERR_BadVersion;
			nret = -pgraphInput->iErrno;
			goto error;
		}
	}

	gnTreeDestroy(pvVisited);
	return i;

error:
	gnTreeDestroy(pvVisited);
	return nret;
}

void gnGrpFreeSPReport( gnGrpGraph_s * pgraph , gnGrpSPReport_s * pSPReport )
{
	int iArc;
	if ( pSPReport )
	{
		if ( pSPReport->pArc )
		{
			for ( iArc = 0 ; iArc < pSPReport->cArc ; iArc ++ ) {
				if ( pSPReport->pArc[iArc].Link ) free( pSPReport->pArc[iArc].Link );
			}
			free( pSPReport->pArc );
		}
		free( pSPReport );
	}
}

int gnGrpInitializeSPCache( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache ) {
	return gngrp_initialize_sp_cache_V1( pgraph, pCache );
}

void gnGrpReleaseSPCache( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache ) {
	gngrp_release_sp_cache_V1( pgraph, pCache );
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
	case GNGRP_ERR_LinkNotFound:
		return "Link Not Found";
	case GNGRP_ERR_NodeAlreadyExist:
		return "Node Already Exist";
	case GNGRP_ERR_NodeIsAComponent:
		return "Node Is A Component";
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
gnInt32_t gnGrpGet_NodeSize		(gnGrpGraph_s *  pgraph) {
	switch(pgraph->Version) {
	case 1:
		return GNGRP_NODE_SIZEOF_v1( pgraph->NodeAttrSize );
#ifdef GNGRP_V2
	case 2:
		return GNGRP_NODE_SIZEOF_v2( pgraph->NodeAttrSize );
#endif
	}
	pgraph->iErrno = GNGRP_ERR_BadVersion;
	return -pgraph->iErrno;
}
gnInt32_t gnGrpGet_LinkSize		(gnGrpGraph_s *  pgraph) {
	switch(pgraph->Version) {
	case 1:
		return GNGRP_LINK_SIZEOF_v1( pgraph->NodeAttrSize );
#ifdef GNGRP_V2
	case 2:
		return GNGRP_LINK_SIZEOF_v2( pgraph->NodeAttrSize );
#endif
	}
	pgraph->iErrno = GNGRP_ERR_BadVersion;
	return -pgraph->iErrno;
}




