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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <malloc.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>

#include "../type.h"
#include "../graph.h"

#include "opt.h"


extern int errno;

/* If the clipper function returns 1 , the node is discarded and
 * the traversing of the graph toward its direction is abandoned.
 * Try to change the return value to 1 and see that the program
 * will not find any more paths to destinations.
 * The clipper receives data relating the network segment being examinated.
 * The pvarg is a user pointer registered to gnGrpShortestPath() and
 * passed back to the clipper.
 * As a demo, the main uses the ClipperContext_s structure to store a nodeid
 * that must be discarded during the search. The clipper receives
 * that pointer and checks every node against the one specified there.
 * If the node matches return 1 , otherwise return 0.
 */

typedef struct {
	gnInt32_t node_to_discard;
} ClipperContext_s;

static int 	clipper 	(
						gnGrpGraph_s * 	pgraph ,
						gnGrpSPClipInput_s * pIn ,
						gnGrpSPClipOutput_s * pOut ,
						void * 			pvarg		/* caller's pointer */
						)
{
	ClipperContext_s * pclip = (ClipperContext_s*) pvarg;
#if 0
	gnInt32_t * pnFromXYZ = (gnInt32_t*) gnGrpGetNode_Attr( pgraph, pIn->pnNodeFrom );
	gnInt32_t * pnToXYZ   = (gnInt32_t*) gnGrpGetNode_Attr( pgraph, pIn->pnNodeTo );

	printf( "clipper called:\n" );
	printf( "        from node: %ld - attributes x=%ld y=%ld z=%ld\n",
		gnGrpGetNode_Id(pgraph, pIn->pnNodeFrom), pnFromXYZ[0], pnFromXYZ[1], pnFromXYZ[2]);
	printf( "        to   node: %ld - attributes x=%ld y=%ld z=%ld\n",
		gnGrpGetNode_Id(pgraph, pIn->pnNodeTo), pnToXYZ[0], pnToXYZ[1], pnToXYZ[2]);
	printf( "        link     : %ld\n",
		gnGrpGetLink_UserId(pgraph, pIn->pnLink) );
#endif

	if ( pclip )
	{
		if ( gnGrpGetNode_Id(pgraph, pIn->pnNodeTo) == pclip->node_to_discard )
		{
			/*
			printf( "        discarder.\n" );
			*/
			return 1;
		}
	}
	/*
	printf( "        accepted.\n" );
	*/
	return 0;
}



int main( int argc , char ** argv )
{
	gnGrpGraph_s  		graph;
	gnInt32_t 			from , to;

	int					fd , nret;
	gnGrpSPReport_s * 	pReport;
	gnInt32_t			nDistance;
	gnGrpSPCache_s		spCache;
	ClipperContext_s	clipctx , * pclipctx;

	/* program options
	 */
 	char	*	pszFilein;
 	char	*	pszFrom;
 	char	*	pszTo;
 	char	*	pszDiscard;
	Boolean		fDistance;
 
	GNO_BEGIN /*short  	long        default     variable        help */
 	GNO_OPTION( "g", 	"graph", 	NULL ,  	& pszFilein ,	"graph file to view" )
 	GNO_OPTION( "f", 	"from", 	NULL ,  	& pszFrom ,		"from-node id" )
 	GNO_OPTION( "t", 	"to", 		NULL ,  	& pszTo ,		"to-node id" )
 	GNO_OPTION( "d", 	"discard", 	NULL ,  	& pszDiscard ,	"node to discard in clipper" )
 	GNO_SWITCH( "D", 	"distance",	False , 	& fDistance ,	"Report shortest distance only" )
 	GNO_END
 

	if ( GNO_PARSE( argc , argv ) < 0 )
	{
		return 1;
	}
	/* options parsed
	 */

	if ( pszFilein == NULL || pszFrom == NULL || pszTo == NULL )
	{
		GNO_HELP( "incomplete parameters" );
		return 1;
	}

	if ( pszDiscard )
	{
		clipctx.node_to_discard = atol( pszDiscard );
		pclipctx = & clipctx;
	}
	else
		pclipctx = NULL;

	fd = open( pszFilein , O_RDONLY );
	if ( fd < 0 )
	{	
		perror( "open" ); return 1;
	}

	nret = gnGrpRead( & graph , fd );

	close( fd );

	if ( nret < 0 )
	{
		fprintf( stderr , "gnGrpRead error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}

	from = atol( pszFrom );
	to = atol( pszTo );

	printf( "shortest path: from-node %ld - to-node %ld\n\n" , from , to );

	gnGrpInitializeSPCache( & graph, & spCache );

	if ( fDistance == False ) {
		nret = gnGrpShortestPath( & graph , & pReport , from , to , clipper , pclipctx , & spCache );

		if ( nret == 0 )
		{
			if ( gnGrpErrno(& graph) == 0 )
			{
				printf( "destination node is unreachable\n\n" );
			}
		}
		else if ( nret < 0 ) 
		{
			fprintf( stderr , "gnGrpShortestPath error: %s\n", gnGrpStrerror( & graph ) );
		}
		else
		{
			int i;
	
			printf( "shortest path report: total links %ld - total distance %ld\n" , pReport->cArc , pReport->distance );
	
			for( i = 0 ; i < pReport->cArc ; i ++ )
			{
				printf(	"link[%d]: from %ld to %ld - travel cost %ld - user linkid %ld - distance from start node %ld\n" ,
						i,
						pReport->pArc[i].From,
						pReport->pArc[i].To,
						gnGrpGetLink_Cost(&graph, pReport->pArc[i].Link), /* this is the cost from clip() */
						gnGrpGetLink_UserId(&graph, pReport->pArc[i].Link),
						pReport->pArc[i].Distance
						);
			}
			gnGrpFreeSPReport( & graph , pReport );
		}
	}
	else {
		nret = gnGrpShortestDistance( & graph , & nDistance , from , to , clipper , pclipctx , & spCache );

		if ( nret == 0 )
		{
			if ( gnGrpErrno(& graph) == 0 )
			{
				printf( "destination node is unreachable\n\n" );
			}
		}
		else if ( nret < 0 ) 
		{
			fprintf( stderr , "gnGrpShortestDistance error: %s\n", gnGrpStrerror( & graph ) );
		}
		else
		{
			printf( "shortest distance: %ld\n" , nDistance );
		}
	}

	gnGrpReleaseSPCache( & graph, & spCache );
	gnGrpRelease( & graph );

	return 0;

}
