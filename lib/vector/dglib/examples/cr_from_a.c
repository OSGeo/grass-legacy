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
#include <string.h>
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



int main( int argc , char ** argv )
{
	FILE *			fp;
	gnGrpGraph_s  	graph;
	char 			sz[ 1024 ];
	char			c;
	int			 	nret , fd;
	int 			version , attrsize;
	gnInt32_t	 	nodeid , from , to , cost , user , xyz[3];
	gnInt32_t	 	opaqueset[ 16 ] = {
		360000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};

	/* program options
	 */
 	char	*	pszFilein;
 	char	*	pszFileout;
 
	GNO_BEGIN/* short  long        default     variable        help */
 	GNO_OPTION( "f", 	"file", 	NULL ,  	& pszFilein ,	"Input Graph definition file" )
 	GNO_OPTION( "g", 	"graph", 	NULL ,  	& pszFileout ,	"Output Graph file" )
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

	if ( (fp = fopen( pszFilein , "r" )) == NULL )
	{
		perror( "fopen" ); return 1;
	}

reread_first_line:
	if( fgets( sz , sizeof( sz ) , fp ) == NULL )
	{
		fprintf( stderr , "unexpected EOF\n" ); return 1;
	}

	if ( sz[0] == '#' || strlen( sz ) == 0 ) goto reread_first_line;

	sscanf( sz , "%d %d" , & version , &attrsize );

	/*
	 * new API call
	 */
	gnGrpInitialize (
					& graph , 		/* graph context to initialize */
					1 ,				/* version (so far is always 1) */
					sizeof(xyz) ,	/* node attributes size */
					0 ,				/* link attributes size */
					opaqueset		/* opaque graph parameters */
					);

	/* add arcs and X/Y/Z node attributes
	 */
	while( fgets( sz , sizeof( sz ) , fp ) != NULL )
	{
		if ( sz[0] == '#' ) continue;

		if ( strlen( sz ) == 0 ) continue;

		if ( sz[0] == 'A' ) /* add a arc */
		{
			sscanf( sz , "%c %ld %ld %ld %ld" , &c , &from , &to , &cost , &user );

			nret = gnGrpAddLink	(
								& graph ,
								from ,
								to ,
								cost ,
								user ,
								NULL , /* from-node attributes */
								NULL , /*   to-node attributes */
								NULL   /*      link attributes */
								);


			if ( nret < 0 )
			{
				fprintf( stderr , "gnGrpAddArc error: %s\n" , gnGrpStrerror( & graph ) );
				return 1;
			}
		}
		else if ( sz[0] == 'N' ) /* set attributes for a (already inserted) node */
		{
			sscanf( sz , "%c %ld %ld %ld %ld" , &c , &nodeid , &xyz[0] , &xyz[1] , &xyz[2] );

			nret = gnGrpSetNodeAttr	(
									& graph ,
									xyz ,
									nodeid
									);

			if ( nret < 0 )
			{
				fprintf( stderr , "gnGrpSetNodeAttr error: %s\n" , gnGrpStrerror( & graph ) );
				return 1;
			}
		}
	}
	fclose( fp );

	/*
	 * flatten the graph
	 */
	nret = gnGrpFlatten( & graph );

	if ( nret < 0 )
	{
		fprintf( stderr , "gnGrpFlatten error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}


	/* store the graph
	 */
	if ( (fd = open( pszFileout , O_WRONLY | O_CREAT , 0666 )) < 0 )
	{
		perror( "open" ); return 1;
	}

	nret = gnGrpWrite( & graph , fd );

	if ( nret < 0 )
	{
		fprintf( stderr , "gnGrpWrite error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}

	gnGrpRelease( & graph );

	return 0;
}
