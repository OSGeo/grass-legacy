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
 * Graph State bitmask - returned by gnGrpGet_GraphState() function
 */
#define GNGRP_GS_FLAT			0x1		/* otherwise is TREE */


/*
 * Node Status bitmask - returned by GNGRP_NODE_STATUS() macro
 */
#define GNGRP_NS_FROM			0x1		/* node exists as 'from' (static) */
#define GNGRP_NS_TO				0x2		/* node exists as 'to' (static) */
#define GNGRP_NS_ALONE			0x4		/* node is a component */


/*
 * Endianess Values - returned by gnGrpGet_Endianess() function
 */
#define GNGRP_ENDIAN_BIG		1
#define GNGRP_ENDIAN_LITTLE		2


/*
 * miscellaneous
 */
/* add-link flags */
#define GNGRP_STRONGCONNECT		0x1
#define GNGRP_ALONE				0x2
/* */



/*
 * Shortest Path clip definitions
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
	gnInt32_t			cAlone;
	gnInt32_t			cArc;
	gnInt32_t			Flags;
	gnInt32_t			aOpaqueSet[ 16 ];

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
 * Node/Linkarea/Link Scan callback function type
 */
typedef int (*gnGrpScan_fn)( gnGrpGraph_s * pGraph , gnInt32_t * pn , void * pvArg );

/*
 * Chunked Write callback function type
 */
typedef int (*gnGrpChunkWrite_fn)( gnGrpGraph_s * pGraph, unsigned char * pbChunk, int cbChunk, void * pvArg );

/*
 * An ARC defined as : from-node, to-node, link, to-node-distance (from the path starting node)
 */
typedef struct _gnGrpSPArc
{
	gnInt32_t From;
	gnInt32_t To;
	gnInt32_t *	Link;
	gnInt32_t Distance;
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
 * Shortest Path Cache
 */
typedef struct {
	gnInt32_t	nFromNode;
	gnHeap_s	NodeHeap;
	void * 		pvVisited;	
	void * 		pvPredist;	
}
gnGrpSPCache_s;


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
#define GNGRP_ERR_LinkNotFound			19
#define GNGRP_ERR_NodeAlreadyExist		20
#define GNGRP_ERR_NodeIsAComponent		21



/*
 * graph context management
 */
int  gnGrpInitialize(gnGrpGraph_s * pGraph, gnByte_t Version, gnInt32_t NodeAttrSize, gnInt32_t LinkAttrSize, gnInt32_t * pOpaqueSet);
int  gnGrpRelease( gnGrpGraph_s * pGraph );
int  gnGrpUnflatten( gnGrpGraph_s * pGraph );
int  gnGrpFlatten( gnGrpGraph_s * pGraph );
void gnGrpResetStats( gnGrpGraph_s * pgraph );

/*
 * node management
 */
gnInt32_t * gnGrpGetNode(gnGrpGraph_s * pGraph , gnInt32_t nNodeId);
gnInt32_t *	gnGrpGetNode_OutLinkarea(gnGrpGraph_s * pGraph, gnInt32_t * pnNode);
gnInt32_t *	gnGrpGetNode_InLinkarea(gnGrpGraph_s * pGraph, gnInt32_t * pnNode);
gnInt32_t	gnGrpGetNode_Id(gnGrpGraph_s * pGraph, gnInt32_t * pnNode);
gnInt32_t	gnGrpGetNode_Status(gnGrpGraph_s * pGraph, gnInt32_t * pnNode);
gnInt32_t *	gnGrpGetNode_Attr(gnGrpGraph_s * pGraph, gnInt32_t * pnNode);
int	        gnGrpSetNode_Attr(gnGrpGraph_s * pGraph, gnInt32_t * pnNode, gnInt32_t * pnAttr);
int         gnGrpNodeScan(
               gnGrpGraph_s * pGraph ,
               gnGrpScan_fn fnNodeScan ,
               void * pvArg
            );
int 		gnGrpAddNode(
               gnGrpGraph_s * 	pGraph ,
               gnInt32_t 		nNodeId ,
               void *			pvNodeAttr ,
               gnInt32_t		nFlags
            );

/*
 * link management
 */
gnInt32_t 	gnGrpGetOutLink_Count(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea);
gnInt32_t *	gnGrpGetOutLink_ByNode(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea, gnInt32_t nToNodeId);
gnInt32_t *	gnGrpGetOutLink_ByUserId(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea, gnInt32_t nUserId);
gnInt32_t *	gnGrpGetOutLink_ByIndex(gnGrpGraph_s * pGraph, gnInt32_t * pnOutLinkarea, int iLink);
gnInt32_t 	gnGrpGetInLink_Count(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea);
gnInt32_t *	gnGrpGetInLink_ById(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea, gnInt32_t nFromNodeId);
gnInt32_t *	gnGrpGetInLink_ByUserId(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea, gnInt32_t nUserId);
gnInt32_t *	gnGrpGetInLink_ByIndex(gnGrpGraph_s * pGraph, gnInt32_t * pnInLinkarea, int iLink);
gnInt32_t   gnGrpGetLink_Cost(gnGrpGraph_s * pGraph , gnInt32_t * pnLink );
gnInt32_t   gnGrpGetLink_UserId(gnGrpGraph_s * pGraph , gnInt32_t * pnLink );
gnInt32_t * gnGrpGetLink_FromNode(gnGrpGraph_s * pGraph , gnInt32_t * pnLink );
gnInt32_t * gnGrpGetLink_ToNode(gnGrpGraph_s * pGraph , gnInt32_t * pnLink );
gnInt32_t * gnGrpGetLink_Attr(gnGrpGraph_s * pGraph , gnInt32_t * pnLink );
int         gnGrpSetLink_Attr(gnGrpGraph_s * pGraph , gnInt32_t * pnAttr , gnInt32_t * pnLink );
int         gnGrpAddLink(
               gnGrpGraph_s * 	pGraph ,
               gnInt32_t 		nFrom ,
               gnInt32_t 		nTo ,
               gnInt32_t 		nCost ,
               gnInt32_t 		nUser
            );
int         gnGrpAddLinkX(
               gnGrpGraph_s * 	pGraph ,
               gnInt32_t 		nFrom ,
               gnInt32_t 		nTo ,
               gnInt32_t 		nCost ,
               gnInt32_t 		nUser ,
               void *			pvFnodeAttr ,
               void *			pvTnodeAttr ,
               void *			pvLinkAttr ,
               gnInt32_t		nFlags
            );

/*
 * graph I/O
 */
int gnGrpWrite( gnGrpGraph_s * pGraph, int fd );
int gnGrpRead( gnGrpGraph_s * pGraph, int fd );


/*
 * algorithms
 */
int gnGrpShortestPath(
                     gnGrpGraph_s *     pGraph,
					 gnGrpSPReport_s ** ppReport,
                     gnInt32_t 		    nFrom,
                     gnInt32_t 		    nTo,
                     gnGrpSPClip_fn	    fnClip,
                     void *			    pvClipArg,
					 gnGrpSPCache_s *   pCache
                  );
int gnGrpShortestDistance(
                     gnGrpGraph_s *   pGraph,
                     gnInt32_t *	  pnDistance,
                     gnInt32_t 		  nFrom,
                     gnInt32_t 		  nTo,
                     gnGrpSPClip_fn	  fnClip,
                     void *			  pvClipArg,
					 gnGrpSPCache_s * pCache
                  );
int gnGrpInitializeSPCache( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache );
void gnGrpReleaseSPCache( gnGrpGraph_s * pgraph, gnGrpSPCache_s * pCache );
void  gnGrpFreeSPReport( gnGrpGraph_s * pGraph , gnGrpSPReport_s * pSPReport );

int	gnGrpDepthSpanning(
       gnGrpGraph_s *  	pgraphInput,
       gnGrpGraph_s *  	pgraphOutput,
       gnInt32_t		nVertexNode,
       gnGrpSpanClip_fn	fnClip,
       void *			pvClipArg
    );

int gnGrpDepthComponents(
       gnGrpGraph_s *  	pgraphInput,
       gnGrpGraph_s *  	pgraphComponents,
       int				cgraphComponents,
       gnGrpSpanClip_fn	fnClip,
       void *			pvClipArg
    );

/*
 * error management
 */
int    gnGrpErrno( gnGrpGraph_s * pgraph );
char * gnGrpStrerror( gnGrpGraph_s * pgraph );

/*
 * graph property hiders
 */
int	        gnGrpGet_Version      (gnGrpGraph_s * pGraph);
int	        gnGrpGet_Endianess    (gnGrpGraph_s * pGraph);
int	        gnGrpGet_NodeAttrSize (gnGrpGraph_s * pGraph);
int	        gnGrpGet_LinkAttrSize (gnGrpGraph_s * pGraph);
int	        gnGrpGet_NodeCount    (gnGrpGraph_s * pGraph);
int	        gnGrpGet_FromNodeCount(gnGrpGraph_s * pGraph);
int	        gnGrpGet_ToNodeCount  (gnGrpGraph_s * pGraph);
int	        gnGrpGet_LinkCount    (gnGrpGraph_s * pGraph);
int	        gnGrpGet_GraphState   (gnGrpGraph_s * pGraph);
gnInt32_t * gnGrpGet_Opaque	      (gnGrpGraph_s * pGraph);
void        gnGrpSet_Opaque       (gnGrpGraph_s * pGraph, gnInt32_t * pOpaque);
gnInt32_t   gnGrpGet_NodeSize     (gnGrpGraph_s * pGraph);
gnInt32_t   gnGrpGet_LinkSize     (gnGrpGraph_s * pGraph);



__END_DECLS
#endif
