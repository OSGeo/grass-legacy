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
#include <ctype.h>

#include "../type.h"
#include "../graph.h"

#include "opt.h"


extern int errno;

static int _print_node( gnGrpGraph_s * pgraph , gnInt32_t * pnode , void * pvarg )
{
	FILE * f = (FILE*)pvarg;
	gnInt32_t * plinkarea;
	gnInt32_t * plink;
	gnInt32_t * ptonode;
	int		iAttr, cAttr;
	int		role;
	int		i;
	char	ch;

	role = 0;

	if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_FROM )
	{
		role |= 1;
	}

	if ( GNGRP_NODE_STATUS(pnode) & GNGRP_NS_TO )
	{
		role |= 2;
	}

	fprintf( f , "NODE %-8ld - ROLE %-s - LINKAREA OFFSET %-8ld",
			GNGRP_NODE_ID(pnode),
			(role>2)?"'from/to'":(role==2)?"'to'     ":"'from'   ",
			GNGRP_NODE_LINKAREA_OFFSET(pnode) );

	if ( (cAttr = gnGrpGet_NodeAttrSize(pgraph)) > 0 ) {
		fprintf( f , " - ATTR HEX DUMP [" );
		for ( iAttr = 0 ; iAttr < cAttr ; iAttr ++ ) {
			if ( iAttr && !(iAttr%4) ) fprintf( f , " " );
			fprintf( f , "%02x" , ((unsigned char*)GNGRP_NODE_ATTR_PTR(pnode))[iAttr] );
		}
		fprintf( f , "]\n" );
	}
	else {
		fprintf( f , "\n" );
	}

	if ( role & 1 ) {
		plinkarea = gnGrpGetLinkArea( pgraph, pnode );

		i = 0;
		GNGRP_FOREACH_LINK(pgraph,plinkarea,plink) {
			if ( pgraph->Flags & 0x1 ) 	ptonode = GNGRP_NODEBUFFER_SHIFT(pgraph, GNGRP_LINK_TONODE_OFFSET(plink));
			else						ptonode = gnGrpGetNode(pgraph, GNGRP_LINK_TONODE_OFFSET(plink));

			if ( ptonode ) {
				fprintf( f , "LINK #%-8d: TO NODE %-8ld - COST %-8ld - USER %-8ld",
					 i++ ,
					 GNGRP_NODE_ID(ptonode) ,
					 GNGRP_LINK_COST(plink) ,
					 GNGRP_LINK_USER(plink)
					 );

				if ( (cAttr = gnGrpGet_LinkAttrSize(pgraph)) > 0 ) {
					fprintf( f , " - ATTR HEX DUMP [" );
					for ( iAttr = 0 ; iAttr < cAttr ; iAttr ++ ) {
						if ( iAttr && !(iAttr%4) ) fprintf( f , " " );
						fprintf( f , "%02x" , ((unsigned char*)GNGRP_LINK_ATTR_PTR(plink))[iAttr] );
					}
					fprintf( f , "] " );
					fprintf( f , " - PRINTABLE [" );
					for ( iAttr = 0 ; iAttr < cAttr ; iAttr ++ ) {
						ch = ((char*)GNGRP_LINK_ATTR_PTR(plink))[iAttr];
						fprintf( f , "%c" , (isprint(ch))?ch:' ' );
					}
					fprintf( f , "]\n" );
				}
				else {
					fprintf( f , "\n" );
				}
			}
		}
	}
	return 0;
}

int main( int argc , char ** argv )
{
	gnGrpGraph_s  	graph;
	int				fd;
	int				nret;

	/* program options
	 */
 	char	*	pszFilein;
 
	GNO_BEGIN/* short  long        default     variable        help */
 	GNO_OPTION( "g", 	"graph", 	NULL ,  	& pszFilein ,	"Graph file to view" )
 	GNO_END
 

	if ( GNO_PARSE( argc , argv ) < 0 )
	{
		return 1;
	}
	/*
	 * options parsed
	 */

	if ( pszFilein == NULL )
	{
		GNO_HELP( "Incomplete parameters" );
		return 1;
	}

	fd = open( pszFilein , O_RDONLY );
	if ( fd < 0 )
	{	
		perror( "open" ); return 1;
	}

	nret = gnGrpRead( & graph , fd );

	if ( nret < 0 )
	{
		fprintf( stderr , "gnGrpRead error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}

	close( fd );

	/* print the header
	 */
	fprintf( stdout , "Version: %d\n" ,
			 graph.Version );
	fprintf( stdout , "Byte Order: %s\n" ,
			 (graph.Endian == GNGRP_ENDIAN_LITTLE)?"Little Endian":"Big Endian" );
	fprintf( stdout , "Node Attribute Size:  %ld\n" ,
			 graph.NodeAttrSize );
	fprintf( stdout , "Link Attribute Size:  %ld\n" ,
			 graph.LinkAttrSize );
	fprintf( stdout , "Counters:  %ld Links - %ld Nodes: %ld 'from role' / %ld 'to role'\n" ,
			 graph.cArc , graph.cNode ,
			 graph.cFrom , graph.cTo );
	fprintf( stdout , "Opaque Settings:\n" );
	fprintf( stdout , "%10ld %10ld %10ld %10ld\n",
			 graph.aOpaqueSet[ 0 ], graph.aOpaqueSet[ 1 ],
			 graph.aOpaqueSet[ 2 ], graph.aOpaqueSet[ 3 ] );
	fprintf( stdout , "%10ld %10ld %10ld %10ld\n",
			 graph.aOpaqueSet[ 4 ], graph.aOpaqueSet[ 5 ],
			 graph.aOpaqueSet[ 6 ], graph.aOpaqueSet[ 7 ] );
	fprintf( stdout , "%10ld %10ld %10ld %10ld\n",
			 graph.aOpaqueSet[ 8 ], graph.aOpaqueSet[ 9 ],
			 graph.aOpaqueSet[ 10 ], graph.aOpaqueSet[ 11 ] );
	fprintf( stdout , "%10ld %10ld %10ld %10ld\n",
			 graph.aOpaqueSet[ 12 ], graph.aOpaqueSet[ 13 ],
			 graph.aOpaqueSet[ 14 ], graph.aOpaqueSet[ 15 ] );
	fprintf( stdout , "--\n" );


	/* scan the graph
	 */
	gnGrpScan( & graph , _print_node , stdout );
	printf( "\n" );
	gnGrpRelease( & graph );
	return 0;

}
