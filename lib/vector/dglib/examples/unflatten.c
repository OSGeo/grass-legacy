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

int main( int argc , char ** argv )
{
	gnGrpGraph_s  	graph;
	int			 	nret , fd;

	/* program options
	 */
 	char	*	pszGraph;
 	char	*	pszGraphOut;
 
	GNO_BEGIN/* short   long        	default     variable        help */
 	GNO_OPTION( "g", 	"graph", 		NULL ,  	& pszGraph ,	"Input Graph file" )
 	GNO_OPTION( "o", 	"graphout", 	NULL ,  	& pszGraphOut ,	"Output Graph file" )
 	GNO_END
 

	if ( GNO_PARSE( argc , argv ) < 0 )
	{
		return 1;
	}
	/*
	 * options parsed
	 */

	printf( "Graph read:\n" );
	if ( (fd = open( pszGraph , O_RDONLY )) < 0 )
	{
		perror( "open" ); return 1;
	}
	nret = gnGrpRead( & graph , fd );
	if ( nret < 0 ) {
		fprintf( stderr , "gnGrpRead error: %s\n", gnGrpStrerror( & graph ) );
		return 1;
	}
	close( fd );
	printf( "Done.\n" );


	printf( "Graph unflatten:\n" );
	nret = gnGrpUnflatten( & graph );
	if ( nret < 0 ) {
		fprintf( stderr , "gnGrpUnflatten error: %s\n", gnGrpStrerror( & graph ) );
		return 1;
	}
	printf( "Done.\n" );


	printf( "Graph flatten:\n" );
	nret = gnGrpFlatten( & graph );
	printf( "Done.\n" );


	if ( pszGraphOut ) {
		printf( "Graph write:\n" );
		if ( (fd = open( pszGraphOut , O_WRONLY | O_CREAT | O_TRUNC, 0666 )) < 0 )
		{
			perror( "open" ); return 1;
		}
		gnGrpWrite( & graph, fd );
		if ( nret < 0 )
		{
			fprintf( stderr , "gnGrpWrite error: %s\n" , gnGrpStrerror( & graph ) );
			return 1;
		}
		close( fd );
		printf( "Done.\n" );
	}

	gnGrpRelease( & graph );
	return 0;
}
