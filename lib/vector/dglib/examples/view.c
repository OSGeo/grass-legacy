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


int main( int argc , char ** argv )
{
	gnGrpGraph_s  	graph;
	gnInt32_t *		pflat;
	int				fd , i;
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


	gnGrpDumpHead( & graph , stdout );

	/* iterate into the node buffer
	 */
	for ( pflat = (gnInt32_t*)graph.pNodeBuffer , i = 0 ; i < graph.cNode ; i ++ )
	{
		gnGrpDumpNode( & graph , stdout , pflat );
		pflat += GNGRP_C_WSIZE(graph.NodeAttrSize);
	}

	printf( "\n" );
	
	gnGrpRelease( & graph );
	return 0;

}
