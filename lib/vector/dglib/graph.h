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
 * FLAT DIRECTED GRAPH (FDG) MEMORY REPRESENTATION * VERSION 1:
 *
 * FDG is represented by two mandatory arrays of 32bit words: the node buffer and the link buffer. Plus two more optional arrays:
 * the node-attr buffer and the link-attr buffer.
 *
 * Each node has a entry in the node buffer. The status tells if a node has role 'from', 'to' or both. All the arcs departing
 * from a node are described by a link-area stored in the link buffer. The offset field of a node points to its link-area.
 * Each link-area tells the number of link-entries (tocnt) followed by tocnt*link-entries.
 * Each link-entry reports the offset of the to-node in the node buffer, the 'cost' for travelling from->to on this link and
 * a user defined value (usually an external arc-id).
 *
 * Nodes in the node buffer are sorted by ascending nodeid, thus a node can be found using a binary search on the
 * node buffer, given also that a node entry has fixed size.
 * 
 * Node attributes are optionally stored by enlarging the node entry by a fixed amount of bytes (attr), as well as link
 * attributes, which are stored in the respective link entry extension (attr).
 * Sizes for node and link attributes are given at graph creation time and cannot be modified later.
 * 
 *        +-------------------------------------------------------------------------------------------------------> ...
 *        | +--------------------------------------------------+
 *        | |                                                  V
 * Node Buffer (nodeid|status|offset|predec|distan|attr)...    (nodeid|status|offset|predec|distan|attr)...
 *        | |                   |                                              |                     
 *        | |  +----------------+                                     +--------+                     
 *        | |  V                                                      V                              
 * Link Buffer (tocnt|toarr) ...                                      (tocnt|toarr)                  
 *        | |        (offset|cost|user|attr)[0]... (||)[tocnt-1]...         (offset|cost|user|attr)[0]... (||)[tocnt-1]...
 *        | |           |                                                     |
 *        | +-----------+                                                     |
 *        +-------------------------------------------------------------------+
 *
 * TODO: actual node fields 'predec' and 'distan' are kept in the graph data model but are not really carring
 * effective graph description. They are used in shortest path computations to assume dynamic assigned values.
 * This is a problem: 1) the size of the flat graph is larger than what it could be; 2) the concurrent shortest path
 * computation cannot be achieved this way. I'll get them out of the graph in the next revisions.
 */

/* FLAT DIRECTED GRAPH (FDG) FILE REPRESENTATION * VERSION 1
 *
 * Flat graphs can be stored into files as they are, their representation is in what we use to call a 'serialized format'.
 * Graphs are versioned: a pool exists of pointers to methods which implement a given graph version, for each graph version.
 * Actually only version 1 has been defined.
 * The version number is stored as the first byte of the graph file and must be supplied as an argument when we want to
 * create a new graph.
 *
 * Verion 1 graph file format:
 *
 * Field                          Size/Type                     Value range          Description
 * [VERSION...................]   1 byte                        1                    Version 1 graphs always keep value 1
 * [ENDIANESS.................]   1 byte                        1 | 2                Integer words byte order 1=big 2=little
 * [NODE ATTRIBUTES BYTE SIZE.]   4 bytes integer               >= 0                 Byte size of attributes area for each node
 * [LINK ATTRIBUTES BYTE SIZE.]   4 bytes integer               >= 0                 Byte size of attributes area for each link
 * [OPAQUE SET................]   16 words of 4 bytes           -                    Free user's storage
 * [NODE COUNT................]   4 bytes integer               > 0                  How many nodes in graph
 * [FROM NODE COUNT...........]   4 bytes integer               > 0                  How many nodes with from role
 * [TO NODE COUNT.............]   4 bytes integer               > 0                  How many nodes with to role
 * [ARC (LINK) COUNT..........]   4 bytes integer               > 0                  Home many links in graph
 * [NODE BUFFER BYTE SIZE.....]   4 bytes integer               >= 0                 Byte size of the node buffer
 * [LINK BUFFER BYTE SIZE.....]   4 bytes integer               >= 0                 Byte size of the link buffer
 * [NODE BUFFER...............]   variable len array of bytes   V1 FDG NODE BUFFER   Node buffer
 * [LINK BUFFER...............]   variable len array of bytes   V1 FDG LINK BUFFER   Link buffer
 *
 */



/*
 * Node macros
 */
#define GNGRP_C_NODEID						0
#define GNGRP_C_STATUS 						1
#define GNGRP_C_OFFSET 						2
#define GNGRP_C_ATTR						3
#define GNGRP_C_SIZE						GNGRP_C_ATTR

#define GNGRP_C_SIZEOF( nattr  ) 		 	( sizeof( gnInt32_t ) * GNGRP_C_SIZE + (nattr) )
#define GNGRP_C_WSIZE( nattr )				( GNGRP_C_SIZEOF( nattr ) / sizeof(gnInt32_t) )
#define GNGRP_C_ALLOC( nattr )   			( malloc( GNGRP_C_SIZEOF( nattr ) ) )

#define GNGRP_NODE_ID(p)					(p[GNGRP_C_NODEID])
#define GNGRP_NODE_STATUS(p)				(p[GNGRP_C_STATUS])
#define GNGRP_NODE_LINKAREA_OFFSET(p)		(p[GNGRP_C_OFFSET])

/*
 * LinkArea macros
 */
#define GNGRP_F_TOCNT						0
#define GNGRP_F_SIZE						1
#define GNGRP_F_TOARR						GNGRP_F_SIZE

#define GNGRP_F_SIZEOF( C , lattr )  		( sizeof( gnInt32_t ) * (GNGRP_F_SIZE) + GNGRP_T_SIZEOF(lattr) * (C) )
#define GNGRP_F_WSIZE( C , lattr )			( GNGRP_F_SIZEOF( C , lattr ) / sizeof(gnInt32_t) ) )
#define GNGRP_F_ALLOC( C , lattr )   		( malloc( GNGRP_F_SIZEOF( C , lattr ) ) )
#define GNGRP_F_REALLOC( P , C , lattr ) 	( realloc( P , GNGRP_F_SIZEOF( C , lattr ) ) )

#define GNGRP_LINKAREA_LINKCOUNT(p)			(p[GNGRP_F_TOCNT])
#define GNGRP_LINKAREA_LINKARRAY_PTR(p)		(p + GNGRP_F_TOARR)

/*
 * Link macros
 */
#define GNGRP_T_OFFSET						0
#define GNGRP_T_COST						1
#define GNGRP_T_USER						2
#define GNGRP_T_ATTR						3
#define GNGRP_T_SIZE						GNGRP_T_ATTR

#define GNGRP_T_SIZEOF( lattr ) 			( sizeof( gnInt32_t ) * GNGRP_T_SIZE + (lattr) )
#define GNGRP_T_WSIZE( lattr ) 				( GNGRP_T_SIZEOF( lattr ) / sizeof( gnInt32_t ) )
#define GNGRP_T_ALLOC( lattr )  			( malloc( GNGRP_T_SIZEOF( lattr ) ) )

#define GNGRP_LINK_TONODE_OFFSET(p)			(p[GNGRP_T_OFFSET])
#define GNGRP_LINK_COST(p)					(p[GNGRP_T_COST])
#define GNGRP_LINK_USER(p)					(p[GNGRP_T_USER])


/*
 * node status
 */
#define GNGRP_NF_FROM			0x1		/* node exists as 'from' (static) */
#define GNGRP_NF_TO				0x2		/* node exists as 'to' (static) */


/*
 * header fields
 */
#define GNGRP_HEAD_VERSION		0
#define GNGRP_HEAD_ENDIAN		1
#define GNGRP_HEAD_NODETYPE		2
#define GNGRP_HEAD_UNUSED		3


/*
 * graph header values
 */
#define GNGRP_ENDIAN_BIG		1
#define GNGRP_ENDIAN_LITTLE		2
#define GNGRP_NODETYPE_1		1
#define GNGRP_NODETYPE_2		2
#define GNGRP_CURRENT_VERSION	1
#define GNGRP_NODETYPE_MAX		2

#ifdef GNGRP_NEWCLIP
/*
 * shortest path clip function takes a pointer to
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


#endif /* GNGRP_NEWCLIP */
/* forward declaration of gnGrpGraph structure  */
struct _gnGrpGraph;

typedef struct _gnGrpMethods
{
	int			(*release)			(
									struct _gnGrpGraph *	pgraph
									);

	int 		(*write)			(
									struct _gnGrpGraph *	pgraph ,
									int						fd
									);

	int			(*read)				(
									struct _gnGrpGraph *	pgraph ,
									int						fd
									);

	gnInt32_t * 	(*parsearcvalues)(
									struct _gnGrpGraph *	pgraph ,
									gnInt32_t *				ppar ,
									gnInt32_t 				lFrom ,
									gnInt32_t 				lTo , 
									gnInt32_t 				lCost , 
									gnInt32_t 				lUser
									);

	int         (*addlink)     		(
									struct _gnGrpGraph * 	pgraph ,
									gnInt32_t *				ppar ,
									void *					pvFromNodeAttr ,
									void *					pvToNodeAttr ,
									void *					pvLinkAttr
									);

	gnInt32_t *     (*searchnode) 		(
									struct _gnGrpGraph * 	pgraph ,
									gnInt32_t 				nodeid
									);

	int         (*unflatten)		(
									struct _gnGrpGraph * 	pgraph
									);

	int         (*flatten)			(
									struct _gnGrpGraph * 	pgraph
									);

	int         (*setnodeattr)		(
									struct _gnGrpGraph * 	pgraph ,
									void *					pattr ,
									gnInt32_t				nodeid
									);

	void *      (*getnodeattr)		(
									struct _gnGrpGraph * 	pgraph ,
									gnInt32_t				nodeid
									);

	int         (*setlinkattr)		(
									struct _gnGrpGraph * 	pgraph ,
									void *					pattr ,
									gnInt32_t				fnodeid ,
									gnInt32_t				tnodeid
									);

	void *      (*getlinkattr)		(
									struct _gnGrpGraph * 	pgraph ,
									gnInt32_t				fnodeid ,
									gnInt32_t				tnodeid
									);

	void        (*dumphead)   		(
									struct _gnGrpGraph * 	pgraph ,
									FILE * 					f
									);

	void        (*dumpnode)   		(
									struct _gnGrpGraph * 	pgraph ,
									FILE * 					f ,
									gnInt32_t *				pnode
									);

	void * 		(*shortestpath)		(
									struct _gnGrpGraph * 	pgraph ,
									gnInt32_t 				from ,
					 				gnInt32_t 				to ,
									int 					(*clip) (
																	struct _gnGrpGraph * 	pgraph ,
#ifndef GNGRP_NEWCLIP
																	gnInt32_t * 			pprevlink ,
																	gnInt32_t * 			pnodefrom ,
																	gnInt32_t * 			plink ,
																	gnInt32_t * 			pnodeto ,
																	gnInt32_t * 			pcost ,
#else /* GNGRP_NEWCLIP */
																	gnGrpSPClipInput_s *	pArgIn ,
																	gnGrpSPClipOutput_s *	pArgOut ,
#endif /* GNGRP_NEWCLIP */
																	void * 					pvarg
																	) ,
					 				void * 					pvcliparg
					 				);
}
gnGrpMethods_s;


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
	gnTreeNode_s *		pLinkTree;
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
	gnGrpMethods_s *	pMethods;

/* so far statistics are only computed by gnGrpAddLink() */
#ifdef GNGRP_STATS
	clock_t				clkAddLink;  /* cycles spent during the last addlink execution */
	int					cAddLink;    /* # of calls to gnGrpAddLink() */
	clock_t				clkLinkTree; /* cycles spent in accessing the link binary tree */
	clock_t				clkNodeTree; /* cycles spent in accessing the node binary tree */
	clock_t				clkNodeHeap; /* cycles spent in accessing the node min heap */
	int					cLinkTree;   /* # of probes in the link tree */
	int					cNodeTree;   /* # of probes in the node tree */
	int					cNodeHeap;   /* # of probes in the node heap */
#endif
}
gnGrpGraph_s;


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
 * Error codes returned in gnGrpError
 */
#define GNGRP_ERR_BadVersion 			-1
#define GNGRP_ERR_BadNodeType 			-2
#define GNGRP_ERR_MemoryExhausted 		-3
#define GNGRP_ERR_HeapError 			-4
#define GNGRP_ERR_UndefinedMethod 		-5
#define GNGRP_ERR_Write 				-6
#define GNGRP_ERR_Read 					-7
#define GNGRP_ERR_NotSupported 			-8
#define GNGRP_ERR_UnknownByteOrder 		-9
#define GNGRP_ERR_FromNodeNotFound 		-10
#define GNGRP_ERR_ToNodeNotFound 		-11
#define GNGRP_ERR_BadLink 				-12
#define GNGRP_ERR_BadOnFlatGraph		-13
#define GNGRP_ERR_BadOnNoFlatGraph		-14
#define GNGRP_ERR_NodeNotFound 			-15
#define GNGRP_ERR_TreeSearchError 		-16
#define GNGRP_ERR_UnexpectedNullPointer -17
#define GNGRP_ERR_VersionNotSupported	-18




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
											gnGrpGraph_s * pgraph ,
											void * pattr ,
											gnInt32_t nodeid
											);

extern void * 			gnGrpGetNodeAttr	(	
											gnGrpGraph_s * pgraph ,
											gnInt32_t nodeid
											);

extern int 				gnGrpSetLinkAttr	(
											gnGrpGraph_s * 	pgraph ,
											void * 			pattr ,
											gnInt32_t 		fnodeid ,
											gnInt32_t		tnodeid
											);

extern void * 			gnGrpGetLinkAttr	(	
											gnGrpGraph_s * 	pgraph ,
											gnInt32_t 		fnodeid ,
											gnInt32_t		tnodeid
											);

extern gnInt32_t * 		gnGrpSearchNode		(
											gnGrpGraph_s * 	pgraph ,
											gnInt32_t 			nodeid
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

extern void 			gnGrpDumpHead		(
											gnGrpGraph_s * 	pgraph ,
											FILE * 			f
											);

extern void 			gnGrpDumpNode		(
											gnGrpGraph_s * 	pgraph ,
											FILE * 			f ,
											gnInt32_t * 		pnode
											);

extern gnGrpSPReport_s *gnGrpShortestPath	(
											gnGrpGraph_s * 	pgraph ,
					 						gnInt32_t 		from ,
					 						gnInt32_t 		to ,
					 						int (*clip)	(
#ifndef GNGRP_NEWCLIP
														gnGrpGraph_s * 	pgraph ,
														gnInt32_t * 	pprevlink ,	/* previous link pointer */
														gnInt32_t * 	pnodefrom ,	/* from node pointer */
														gnInt32_t * 	plink ,		/* this link pointer */
														gnInt32_t * 	pnodeto ,	/* to node pointer */
														gnInt32_t * 	pcost ,	/* to node pointer */
														void * 			pvarg		/* caller's pointer */
#else /* GNGRP_NEWCLIP */
														gnGrpGraph_s * 			pgraph ,
														gnGrpSPClipInput_s *	pArgIn ,
														gnGrpSPClipOutput_s *	pArgOut ,
														void * 					pvarg		/* caller's pointer */
#endif /* GNGRP_NEWCLIP */
														) ,
					 						void * 			pvcliparg				/* caller's pointer (passed back to clip) */
					 						);

extern void 			gnGrpFreeSPReport	(
											gnGrpGraph_s * 	pgraph,
											gnGrpSPReport_s * pSPReport
											);

extern void				gnGrpSetOpaque		(
											gnGrpGraph_s * 	pgraph,
											gnInt32_t *		pOpaque
											);

extern gnInt32_t *		gnGrpGetOpaque		(
											gnGrpGraph_s * 	pgraph
											);

extern int	 			gnGrpErrno			(
											gnGrpGraph_s * 	pgraph
											);

extern char * 			gnGrpStrerror		(
											gnGrpGraph_s * 	pgraph
											);

__END_DECLS
#endif
