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
 * Source best viewed with tabstop=4
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
#include <math.h>

#include "../type.h"
#include "../graph.h"

#include "opt.h"

static int _clipper(gnGrpGraph_s * pgraphIn,
					gnGrpGraph_s * pgraphOut,
					gnGrpSpanClipInput_s * pArgIn,
					gnGrpSpanClipOutput_s * pArgOut,
					void * pvArg)
{
	return 0;
}

int main( int argc , char ** argv )
{
	gnGrpGraph_s  	graph;
#define MY_MAX_COMPONENTS	1024
	gnGrpGraph_s  	agraphComponents[MY_MAX_COMPONENTS];
	int			 	nret , fd , i , cComponents;
	char			szGraphOutFilename[1024];

	/* program options
	 */
 	char	*	pszGraph;
 	char	*	pszGraphOut;
 
	GNO_BEGIN/* short   long        	default     variable        help */
 	GNO_OPTION( "g", 	"graph", 		NULL ,  	& pszGraph ,	"Input Graph file" )
 	GNO_OPTION( "o", 	"graphout", 	NULL ,  	& pszGraphOut ,	"Output Graph file" )
 	GNO_END
 

	if ( GNO_PARSE( argc , argv ) < 0 ) {
		return 1;
	}
	/*
	 * options parsed
	 */

	if ( pszGraph == NULL || pszGraphOut == NULL ) {
		GNO_HELP("components usage");
		return 1;
	}


	printf( "Graph read:\n" );
	if ( (fd = open( pszGraph , O_RDONLY )) < 0 ) {
		perror( "open" ); return 1;
	}
	nret = gnGrpRead( & graph , fd );
	if ( nret < 0 ) {
		fprintf( stderr , "gnGrpRead error: %s\n", gnGrpStrerror( & graph ) );
		return 1;
	}
	close( fd );
	printf( "Done.\n" );



	printf( "Graph depth components spanning:\n" );
	cComponents = gnGrpDepthComponents( & graph , agraphComponents , MY_MAX_COMPONENTS , _clipper , NULL );
	if ( cComponents < 0 ) {
		fprintf( stderr , "gnGrpDepthSpanning error: %s\n", gnGrpStrerror( & graph ) );
		return 1;
	}
	printf( "Done.\n" );

	printf( "Connected Component(s) Found: %d\n", cComponents );

	for( i = 0 ; i < cComponents ; i ++ ) {
		printf( "Component %d of %d: ", i+1 , cComponents ); fflush(stdout);

		printf( "[flatten..." ); fflush(stdout);
		nret = gnGrpFlatten( & agraphComponents[i] );
		printf( "done] " ); fflush(stdout);

		if ( gnGrpGet_LinkCount( & agraphComponents[i] ) > 0 ) {
			if ( pszGraphOut ) {
				snprintf( szGraphOutFilename, sizeof(szGraphOutFilename), "%s-component-%d", pszGraphOut, i );
				printf( "[write <%s>...", szGraphOutFilename ); fflush(stdout);
				if ( (fd = open( szGraphOutFilename , O_WRONLY | O_CREAT | O_TRUNC, 0666 )) < 0 ) {
					perror( "open" ); return 1;
				}
				gnGrpWrite( & agraphComponents[i], fd );
				if ( nret < 0 ) {
					fprintf( stderr , "gnGrpWrite error: %s\n" , gnGrpStrerror( & graph ) );
					return 1;
				}
				close( fd );
				printf( "done] " ); fflush(stdout);
			}
		}
		else {
			printf( "component is empty. No output produced.\n" );
		}

		printf( "[release..." );
		gnGrpRelease( & agraphComponents[i] );
		printf( "done]\n" );
	}

	gnGrpRelease( & graph );
	return 0;
}
