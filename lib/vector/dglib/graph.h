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

#ifndef _GN_GRAPH_H_
#define _GN_GRAPH_H_

#ifdef GNGRP_STATS
#include <time.h>
#endif

#include "heap.h"
#include "tree.h"

__BEGIN_DECLS

/*
 * Node macros - addresses in a flat node
 */
#define GNGRP_IN_NODEID						0
#define GNGRP_IN_STATUS 					1
#define GNGRP_IN_OFFSET 					2
#define GNGRP_IN_ATTR						3
#define GNGRP_IN_SIZE						GNGRP_IN_ATTR

#define GNGRP_NODE_SIZEOF( nattr  ) 	 	(sizeof( gnInt32_t ) * GNGRP_IN_SIZE + (nattr) )
#define GNGRP_NODE_WSIZE( nattr )			(GNGRP_NODE_SIZEOF( nattr ) / sizeof(gnInt32_t) )
#define GNGRP_NODE_ALLOC( nattr )   		(malloc( GNGRP_NODE_SIZEOF( nattr ) ) )

#define GNGRP_NODE_ID(p)					((p)[GNGRP_IN_NODEID])
#define GNGRP_NODE_STATUS(p)				((p)[GNGRP_IN_STATUS])
#define GNGRP_NODE_LINKAREA_OFFSET(p)		((p)[GNGRP_IN_OFFSET])
#define GNGRP_NODE_ATTR_PTR(p)				((p) + GNGRP_IN_ATTR)

/*
 * LinkArea macros - addresses in a flat link-area
 */
#define GNGRP_ILA_TOCNT						0
#define GNGRP_ILA_SIZE						1
#define GNGRP_ILA_TOARR						GNGRP_ILA_SIZE

#define GNGRP_LINKAREA_SIZEOF(C, lattr)  	(sizeof( gnInt32_t ) * (GNGRP_ILA_SIZE) + GNGRP_LINK_SIZEOF(lattr) * (C))
#define GNGRP_LINKAREA_WSIZE(C, lattr)		(GNGRP_LINKAREA_SIZEOF(C, lattr) / sizeof(gnInt32_t)))
#define GNGRP_LINKAREA_ALLOC(C, lattr)   	(malloc(GNGRP_LINKAREA_SIZEOF(C, lattr)))
#define GNGRP_LINKAREA_REALLOC(P, C, lattr)	(realloc(P , GNGRP_LINKAREA_SIZEOF(C, lattr)))

#define GNGRP_LINKAREA_LINKCOUNT(p)			((p)[GNGRP_ILA_TOCNT])
#define GNGRP_LINKAREA_LINKARRAY_PTR(p)		((p) + GNGRP_ILA_TOARR)
#define GNGRP_LINKAREA_LINK_PTR(p,i,C)		(((p) + GNGRP_ILA_TOARR) + (i) * GNGRP_LINK_WSIZE(C))
#define GNGRP_LINKAREA_BUFFER_OFFSET(pgrp,pla)	((gnInt32_t)pla - (gnInt32_t)(pgrp)->pLinkBuffer)

/*
 * Link macros - addresses in a flat link
 */
#define GNGRP_IL_OFFSET						0
#define GNGRP_IL_COST						1
#define GNGRP_IL_USER						2
#define GNGRP_IL_ATTR						3
#define GNGRP_IL_SIZE						GNGRP_IL_ATTR

#define GNGRP_LINK_SIZEOF( lattr ) 			(sizeof( gnInt32_t ) * GNGRP_IL_SIZE + (lattr))
#define GNGRP_LINK_WSIZE( lattr ) 			(GNGRP_LINK_SIZEOF( lattr ) / sizeof( gnInt32_t ))
#define GNGRP_LINK_ALLOC( lattr )  			(malloc( GNGRP_LINK_SIZEOF( lattr ) ))

#define GNGRP_LINK_TONODE_OFFSET(p)			((p)[GNGRP_IL_OFFSET])
#define GNGRP_LINK_COST(p)					((p)[GNGRP_IL_COST])
#define GNGRP_LINK_USER(p)					((p)[GNGRP_IL_USER])
#define GNGRP_LINK_ATTR_PTR(p)				((p) + GNGRP_IL_ATTR)
#define GNGRP_LINK_TONODE_ID(pgrp,pl)		((pgrp->Flags&1)?\
												GNGRP_NODE_ID(pgrp->pNodeBuffer+GNGRP_LINK_TONODE_OFFSET(pl)):\
												GNGRP_LINK_TONODE_OFFSET(pl))

/*
 * Scan a node buffer
 */
#define GNGRP_FOREACH_NODE(pgrp,pn)		for((pn)=(gnInt32_t*)(pgrp)->pNodeBuffer;\
												(pn)<(gnInt32_t*)((pgrp)->pNodeBuffer+(pgrp)->iNodeBuffer);\
												(pn)+=GNGRP_NODE_WSIZE((pgrp)->NodeAttrSize))
/*
 * Scan a linkarea
 */
#define GNGRP_FOREACH_LINK(pgrp,pla,pl)	for((pl)=GNGRP_LINKAREA_LINKARRAY_PTR(pla);\
												(pl)<(pla)+GNGRP_LINK_WSIZE((pgrp)->LinkAttrSize)*GNGRP_LINKAREA_LINKCOUNT(pla);\
												(pl)+=GNGRP_LINK_WSIZE((pgrp)->LinkAttrSize))
/*
 * Node Buffer Utilities
 */
#define GNGRP_NODEBUFFER_SHIFT(pgrp,o)		((gnInt32_t*)((pgrp)->pNodeBuffer + (o)))
#define GNGRP_NODEBUFFER_OFFSET(pgrp,p)		((gnInt32_t)p - (gnInt32_t)(pgrp)->pNodeBuffer)

/*
 * Link Buffer Utilities
 */
#define GNGRP_LINKBUFFER_SHIFT(pgrp,o)		((gnInt32_t*)((pgrp)->pLinkBuffer + (o)))
#define GNGRP_LINKBUFFER_OFFSET(pgrp,pl)	((gnInt32_t)pl - (gnInt32_t)(pgrp)->pLinkBuffer)



/*
 * Graph State bitmask - returned by gnGrpGet_GraphState() function
 */
#define GNGRP_GS_FLAT			0x1		/* otherwise is TREE */


/*
 * Node Status bitmask - returned by GNGRP_NODE_STATUS() macro
 */
#define GNGRP_NS_FROM			0x1		/* node exists as 'from' (static) */
#define GNGRP_NS_TO				0x2		/* node exists as 'to' (static) */


/*
 * Endianess Values - returned by gnGrpGet_Endianess() function
 */
#define GNGRP_ENDIAN_BIG		1
#define GNGRP_ENDIAN_LITTLE		2

/*
 * Shortest Path clip function takes a pointer to
 * gnGrpSPClipInput_s and gnGrpSPClipOutput_s
 */
typedef struct _gnGrpSPClipInput
{
	gnInt32_t * 	pnPrevLink;
	gnInt32_t * 	pnNodeFrom;
	gnInt32_t * 	pnLink;
	gnInt32_t * 	pnNodeTo;
	gnInt32_t		nFromDistance;

} gnGrpSPClipInput_s;

typedef struct _gnGrpSPClipOutput
{
	gnInt32_t		nLinkCost;

} gnGrpSPClipOutput_s;


/*
 * Spanning clip definitions
 */
typedef struct _gnGrpSpanClipInput
{
	gnInt32_t *		pnNodeFrom;
	gnInt32_t *		pnLink;
	gnInt32_t *		pnNodeTo;

} gnGrpSpanClipInput_s;

typedef struct _gnGrpSpanClipOutput
{
	gnInt32_t *		pnReserved;

} gnGrpSpanClipOutput_s;


/*
 * Forward declaration of gnGrpGraph structure
 */
struct _gnGrpGraph;

/*
 * Virtual graph API
 */
typedef struct _gnGrpIOMethods
{
	int 		(*write)			(
									struct _gnGrpGraph *	pgraph ,
									int						fd
									);

	int			(*read)				(
									struct _gnGrpGraph *	pgraph ,
									int						fd
									);
}
gnGrpIOMethods_s;


/*
 * The graph context
 */
typedef struct _gnGrpGraph
{
	int					iErrno;
	gnByte_t			Version;
	gnByte_t			Endian;
	gnInt32_t			NodeAttrSize;
	gnInt32_t			LinkAttrSize;
	gnTreeNode_s *		pNodeTree;
	gnHeap_s	   		NodeHeap;
	gnByte_t * 			pNodeBuffer;
	gnInt32_t			iNodeBuffer;
	gnByte_t * 			pLinkBuffer;
	gnInt32_t			iLinkBuffer;
	gnInt32_t			cNode;
	gnInt32_t			cFrom;
	gnInt32_t			cTo;
	gnInt32_t			cArc;
	gnInt32_t			Flags;
	gnInt32_t			aOpaqueSet[ 16 ];
	gnGrpIOMethods_s *	pMethods;

/* so far statistics are only computed by gnGrpAddLink() */
#ifdef GNGRP_STATS
	clock_t				clkAddLink;  /* cycles spent during the last addlink execution */
	int					cAddLink;    /* # of calls to gnGrpAddLink() */
	clock_t				clkNodeTree; /* cycles spent in accessing the node binary tree */
	clock_t				clkNodeHeap; /* cycles spent in accessing the node min heap */
	int					cNodeTree;   /* # of probes in the node tree */
	int					cNodeHeap;   /* # of probes in the node heap */
#endif
}
gnGrpGraph_s;

/*
 * Shortest Path clip function type
 */
typedef int (*gnGrpSPClip_fn)(gnGrpGraph_s *, gnGrpSPClipInput_s *, gnGrpSPClipOutput_s *, void *);

/*
 * Spanning clip function type
 */
typedef int (*gnGrpSpanClip_fn)(gnGrpGraph_s *, gnGrpGraph_s *, gnGrpSpanClipInput_s *, gnGrpSpanClipOutput_s *, void *);


/*
 * An ARC defined as : from-node, to-node, arc-cost, to-node-distance (from a starting node), user
 */
typedef struct _gnGrpSPArc
{
	gnInt32_t *	From;
	gnInt32_t *	To;
	gnInt32_t *	Link;
	gnInt32_t	Distance;
}
gnGrpSPArc_s;

/*
 * Shortest Path Report
 */
typedef struct _gnGrpSPReport
{
	gnInt32_t		from;
	gnInt32_t		to;
	gnInt32_t		distance;
	gnInt32_t		cArc;
	gnGrpSPArc_s *	pArc;
}
gnGrpSPReport_s;


/*
 * Error codes returned by gnGrpError
 */
#define GNGRP_ERR_BadVersion 			1
#define GNGRP_ERR_BadNodeType 			2
#define GNGRP_ERR_MemoryExhausted 		3
#define GNGRP_ERR_HeapError 			4
#define GNGRP_ERR_UndefinedMethod 		5
#define GNGRP_ERR_Write 				6
#define GNGRP_ERR_Read 					7
#define GNGRP_ERR_NotSupported 			8
#define GNGRP_ERR_UnknownByteOrder 		9
#define GNGRP_ERR_FromNodeNotFound 		10
#define GNGRP_ERR_ToNodeNotFound 		11
#define GNGRP_ERR_BadLink 				12
#define GNGRP_ERR_BadOnFlatGraph		13
#define GNGRP_ERR_BadOnTreeGraph		14
#define GNGRP_ERR_NodeNotFound 			15
#define GNGRP_ERR_TreeSearchError 		16
#define GNGRP_ERR_UnexpectedNullPointer 17
#define GNGRP_ERR_VersionNotSupported	18




/*
 * function prototypes
 */

extern int				gnGrpInitialize		(
											gnGrpGraph_s * 		pgraph ,
											gnByte_t 			Version ,
											gnInt32_t			NodeAttrSize ,
											gnInt32_t			LinkAttrSize ,
											gnInt32_t * 		pOpaqueSet
											);

extern void 			gnGrpResetStats		(
											gnGrpGraph_s *
											);

extern int 				gnGrpRelease		(
											gnGrpGraph_s * 	pgraph
											);

extern int 				gnGrpUnflatten		(
											gnGrpGraph_s * 	pgraph
											);

extern int 				gnGrpFlatten		(
											gnGrpGraph_s * 	pgraph
											);

extern int 				gnGrpSetNodeAttr	(
											gnGrpGraph_s * 	pGraph ,
											void * 			pvAttr ,
											gnInt32_t 		nNodeId
											);

extern void * 			gnGrpGetNodeAttr	(	
											gnGrpGraph_s * 	pGraph ,
											gnInt32_t 		nNodeId
											);

extern int 				gnGrpSetLinkAttr	(
											gnGrpGraph_s * 	pGraph ,
											void * 			pvAttr ,
											gnInt32_t 		nFromNodeId ,
											gnInt32_t		nToNodeId
											);

extern void * 			gnGrpGetLinkAttr	(	
											gnGrpGraph_s * 	pGraph ,
											gnInt32_t 		nFromNodeId ,
											gnInt32_t		nToNodeId
											);

extern gnInt32_t * 		gnGrpGetNode		(
											gnGrpGraph_s * 	pGraph ,
											gnInt32_t		nNodeId
											);

extern gnInt32_t * 		gnGrpGetLinkArea	(
											gnGrpGraph_s * 	pGraph ,
											gnInt32_t *		pnNode
											);

extern gnInt32_t * 		gnGrpGetLink		(
											gnGrpGraph_s * 	pGraph ,
											gnInt32_t *		pnFromNode ,
											gnInt32_t 		nToNodeId
											);

extern int 				gnGrpAddLink		(
											gnGrpGraph_s * 		pgraph ,
											gnInt32_t 			lFrom ,
											gnInt32_t 			lTo ,
											gnInt32_t 			lCost ,
											gnInt32_t 			lUser ,
											void *				pvFnodeAttr ,
											void *				pvTnodeAttr ,
											void *				pvLinkAttr
											);

extern int 				gnGrpWrite			(
											gnGrpGraph_s * 	pgraph ,
											int 			fd
											);

extern int				gnGrpRead			(
											gnGrpGraph_s *  pgraph ,
											int 			fd
											);

extern int 				gnGrpScan			(
											gnGrpGraph_s * pgraph ,
											int (*push)( gnGrpGraph_s * pgraph , gnInt32_t * pnode , void * pvarg ) ,
											void * pvarg
											);

extern gnGrpSPReport_s *gnGrpShortestPath	(
											gnGrpGraph_s * 	pgraph ,
					 						gnInt32_t 		from ,
					 						gnInt32_t 		to ,
					 						int (*clip)	(
														gnGrpGraph_s * 			pgraph ,
														gnGrpSPClipInput_s *	pArgIn ,
														gnGrpSPClipOutput_s *	pArgOut ,
														void * 					pvarg		/* caller's pointer */
														) ,
					 						void * 			pvcliparg				/* caller's pointer (passed back to clip) */
					 						);

extern int				gnGrpDepthSpanning	(
											gnGrpGraph_s *  	pgraphInput,
											gnGrpGraph_s *  	pgraphOutput,
											gnInt32_t			nVertexNode,
											gnGrpSpanClip_fn	fnClip,
											void *				pvClipArg
											);

extern int				gnGrpDepthComponents(
											gnGrpGraph_s *  	pgraphInput,
											gnGrpGraph_s *  	pgraphComponents,
											int					cgraphComponents,
											gnGrpSpanClip_fn	fnClip,
											void *				pvClipArg
											);

extern void 			gnGrpFreeSPReport	(
											gnGrpGraph_s * 	pgraph,
											gnGrpSPReport_s * pSPReport
											);

extern int	 			gnGrpErrno			(
											gnGrpGraph_s * 	pgraph
											);

extern char * 			gnGrpStrerror		(
											gnGrpGraph_s * 	pgraph
											);


/*
 * Hide explicit access to gnGrpGraph_s properties
 */
extern int				gnGrpGet_Version		(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_Endianess		(gnGrpGraph_s *  pgraph); /* return value: GNGRP_ENDIAN_* macros */
extern int				gnGrpGet_NodeAttrSize	(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_LinkAttrSize	(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_NodeCount		(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_FromNodeCount	(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_ToNodeCount	(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_LinkCount		(gnGrpGraph_s *  pgraph);
extern int				gnGrpGet_GraphState		(gnGrpGraph_s *  pgraph); /* return value: GNGRP_GS_* bit flags */
extern gnInt32_t *		gnGrpGet_Opaque			(gnGrpGraph_s *  pgraph);
extern void				gnGrpSet_Opaque			(
												gnGrpGraph_s * 	pgraph,
												gnInt32_t *		pOpaque
												);

__END_DECLS
#endif
