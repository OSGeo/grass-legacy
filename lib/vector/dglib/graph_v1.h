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
 * best view tabstop=4
 */

#ifndef _GN_GRAPH_V1_H_
#define _GN_GRAPH_V1_H_

#ifdef GNGRP_STATS
#include <time.h>
#endif

__BEGIN_DECLS

/*
 * Node macros - addresses in a flat node
 */
#define GNGRP_IN_NODEID_v1					0
#define GNGRP_IN_STATUS_v1 					1
#define GNGRP_IN_TO_OFFSET_v1 				2
#define GNGRP_IN_ATTR_v1					3
#define GNGRP_IN_SIZE_v1					GNGRP_IN_ATTR_v1

#define GNGRP_NODE_SIZEOF_v1( nattr  ) 	 	(sizeof( gnInt32_t ) * GNGRP_IN_SIZE_v1 + (nattr) )
#define GNGRP_NODE_WSIZE_v1( nattr )		(GNGRP_NODE_SIZEOF_v1( nattr ) / sizeof(gnInt32_t) )
#define GNGRP_NODE_ALLOC_v1( nattr )   		(malloc( GNGRP_NODE_SIZEOF_v1( nattr ) ) )

#define GNGRP_NODE_ID_v1(p)					((p)[GNGRP_IN_NODEID_v1])
#define GNGRP_NODE_STATUS_v1(p)				((p)[GNGRP_IN_STATUS_v1])
#define GNGRP_NODE_LINKAREA_OFFSET_v1(p)		((p)[GNGRP_IN_TO_OFFSET_v1])
#define GNGRP_NODE_ATTR_PTR_v1(p)				((p) + GNGRP_IN_ATTR_v1)

/*
 * LinkArea macros - addresses in a flat link-area
 */
#define GNGRP_ILA_TOCNT_v1						0
#define GNGRP_ILA_SIZE_v1						1
#define GNGRP_ILA_TOARR_v1						GNGRP_ILA_SIZE_v1

#define GNGRP_LINKAREA_SIZEOF_v1(C, lattr)  	(sizeof( gnInt32_t ) * (GNGRP_ILA_SIZE_v1) + GNGRP_LINK_SIZEOF_v1(lattr) * (C))
#define GNGRP_LINKAREA_WSIZE_v1(C, lattr)		(GNGRP_LINKAREA_SIZEOF_v1(C, lattr) / sizeof(gnInt32_t)))
#define GNGRP_LINKAREA_ALLOC_v1(C, lattr)   	(malloc(GNGRP_LINKAREA_SIZEOF_v1(C, lattr)))
#define GNGRP_LINKAREA_REALLOC_v1(P, C, lattr)	(realloc(P , GNGRP_LINKAREA_SIZEOF_v1(C, lattr)))

#define GNGRP_LINKAREA_LINKCOUNT_v1(p)			((p)[GNGRP_ILA_TOCNT_v1])
#define GNGRP_LINKAREA_LINKARRAY_PTR_v1(p)		((p) + GNGRP_ILA_TOARR_v1)
#define GNGRP_LINKAREA_LINK_PTR_v1(p,i,C)		(((p) + GNGRP_ILA_TOARR_v1) + (i) * GNGRP_LINK_WSIZE_v1(C))
#define GNGRP_LINKAREA_BUFFER_OFFSET_v1(pgrp,pla)	((gnInt32_t)pla - (gnInt32_t)(pgrp)->pLinkBuffer)

/*
 * Link macros - addresses in a flat link
 */
#define GNGRP_IL_TO_OFFSET_v1					0
#define GNGRP_IL_COST_v1						1
#define GNGRP_IL_USER_v1						2
#define GNGRP_IL_ATTR_v1						3
#define GNGRP_IL_SIZE_v1						GNGRP_IL_ATTR_v1

#define GNGRP_LINK_SIZEOF_v1( lattr ) 			(sizeof( gnInt32_t ) * GNGRP_IL_SIZE_v1 + (lattr))
#define GNGRP_LINK_WSIZE_v1( lattr ) 			(GNGRP_LINK_SIZEOF_v1( lattr ) / sizeof( gnInt32_t ))
#define GNGRP_LINK_ALLOC_v1( lattr )  			(malloc( GNGRP_LINK_SIZEOF_v1( lattr ) ))

#define GNGRP_LINK_TONODE_OFFSET_v1(p)			((p)[GNGRP_IL_TO_OFFSET_v1])
#define GNGRP_LINK_COST_v1(p)					((p)[GNGRP_IL_COST_v1])
#define GNGRP_LINK_USER_v1(p)					((p)[GNGRP_IL_USER_v1])
#define GNGRP_LINK_ATTR_PTR_v1(p)				((p) + GNGRP_IL_ATTR_v1)
#define GNGRP_LINK_TONODE_ID_v1(pgrp,pl)		((pgrp->Flags&1)?\
												GNGRP_NODE_ID_v1(pgrp->pNodeBuffer+GNGRP_LINK_TONODE_OFFSET_v1(pl)):\
												GNGRP_LINK_TONODE_OFFSET_v1(pl))

/*
 * Scan a node buffer
 */
#define GNGRP_FOREACH_NODE_v1(pgrp,pn)			for((pn)=(gnInt32_t*)(pgrp)->pNodeBuffer;\
												(pn)<(gnInt32_t*)((pgrp)->pNodeBuffer+(pgrp)->iNodeBuffer);\
												(pn)+=GNGRP_NODE_WSIZE_v1((pgrp)->NodeAttrSize))
/*
 * Scan a linkarea
 */
#define GNGRP_FOREACH_LINK_v1(pgrp,pla,pl)		for((pl)=GNGRP_LINKAREA_LINKARRAY_PTR_v1(pla);\
												(pl)<(pla)+GNGRP_LINK_WSIZE_v1((pgrp)->LinkAttrSize)*GNGRP_LINKAREA_LINKCOUNT_v1(pla);\
												(pl)+=GNGRP_LINK_WSIZE_v1((pgrp)->LinkAttrSize))
/*
 * Node Buffer Utilities
 */
#define GNGRP_NODEBUFFER_SHIFT_v1(pgrp,o)		((gnInt32_t*)((pgrp)->pNodeBuffer + (o)))
#define GNGRP_NODEBUFFER_OFFSET_v1(pgrp,p)		((gnInt32_t)p - (gnInt32_t)(pgrp)->pNodeBuffer)

/*
 * Link Buffer Utilities
 */
#define GNGRP_LINKBUFFER_SHIFT_v1(pgrp,o)		((gnInt32_t*)((pgrp)->pLinkBuffer + (o)))
#define GNGRP_LINKBUFFER_OFFSET_v1(pgrp,pl)		((gnInt32_t)pl - (gnInt32_t)(pgrp)->pLinkBuffer)




int 		gngrp_add_node_V1(
			gnGrpGraph_s *  pgraph,
			gnInt32_t       lNodeId,
			void * 			pvNodeAttr,	
			gnInt32_t 		nFlags
			);
int 		gngrp_add_link_V1 (
			gnGrpGraph_s * 	pgraph ,
			gnInt32_t 		lFrom,
			gnInt32_t 		lTo,
			gnInt32_t 		lCost,
			gnInt32_t 		lUser,
			void * pvFnodeAttr ,	
			void * pvTnodeAttr ,	
			void * pvLinkAttr ,
			gnInt32_t 		nFlags
			);
gnInt32_t * gngrp_get_node_V1( gnGrpGraph_s * pgraph , gnInt32_t nodeid );
gnInt32_t * gngrp_getnode_outlinkarea_V1( gnGrpGraph_s * pgraph , gnInt32_t * pnode );
int 		gngrp_unflatten_V1( gnGrpGraph_s * pgraph );
int 		gngrp_flatten_V1( gnGrpGraph_s * pgraph );
int			gngrp_dijkstra_V1	(
			gnGrpGraph_s * 		pgraph ,
			gnGrpSPReport_s **	ppReport ,
			gnInt32_t *			pDistance ,
			gnInt32_t 			from ,
			gnInt32_t 			to ,
			gnGrpSPClip_fn		fnClip,
			void * 				pvClipArg,
			gnGrpSPCache_s *	pCache
			);
int 		gngrp_depthfirst_spanning_V1(
			gnGrpGraph_s * pgraphIn ,
			gnGrpGraph_s * pgraphOut ,
			gnInt32_t nNodeId ,
			void * pvVisited ,
			gnGrpSpanClip_fn	fnClip ,
			void *				pvClipArg
			);
int 		gngrp_release_V1( gnGrpGraph_s * pgraph );
int 		gngrp_write_V1( gnGrpGraph_s * pgraph , int fd );
int 		gngrp_read_V1( gnGrpGraph_s * pgraph , int fd );
int 		gngrp_initialize_sp_cache_V1( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache, gnInt32_t nFromNode );
void 		gngrp_release_sp_cache_V1( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache );

__END_DECLS
#endif
