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

extern int errno;


#define NROWS 600
#define NCOLS 100
#define FACTOR 10000
#define BIDIRECTIONAL 1


static int _add_link( gnGrpGraph_s * pgraph , gnInt32_t from , gnInt32_t to , gnInt32_t cost , gnInt32_t arc )
{
	int nret;

	nret = gnGrpAddLink	( pgraph , from , to , cost , arc , NULL , NULL , NULL );
	if ( nret < 0 ) {
		fprintf( stderr, "gnGrpAddLink error: %s\n", gnGrpStrerror( pgraph ) );
		return 1;
	}

	if ( (arc % 1024) == 0 ) {
#ifndef GNGRP_STATS
		printf( "add arc %07d - from %07d - to %07d - cost %07d\r",
			   	arc , from , to , cost );
#else
		printf( "add arc %07ld - from %07ld - to %07ld - cost %07ld . Clock: tot %09ld nt %09ld lt %09ld nh %09ld o %09ld\r" ,
			   	arc , from , to , cost ,
			 	pgraph->clkAddLink / pgraph->cAddLink,
			 	pgraph->clkNodeTree / pgraph->cAddLink,
			 	pgraph->clkLinkTree / pgraph->cAddLink,
			 	pgraph->clkNodeHeap / pgraph->cAddLink,
			 	(pgraph->clkAddLink - (pgraph->clkNodeTree + pgraph->clkLinkTree + pgraph->clkNodeHeap)) / pgraph->cAddLink
			   	);
#endif
		fflush( stdout );
	}
	return 0;
}

int main( int argc , char ** argv )
{
	gnGrpGraph_s  	graph;
	int			 	nret , fd;

	int				irow , icol , itrow , itcol;

	gnInt32_t	 	from , to , arc , cost , xyz[3];


	gnInt32_t	 	opaqueset[ 16 ] = {
		FACTOR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};

	/* program options
	 */
 	char	*	pszFileout;
 
	GNO_BEGIN/* short   long        default     variable        help */
 	GNO_OPTION( "g", 	"graph", 	NULL ,  	& pszFileout ,	"Output Graph file" )
 	GNO_END
 

	if ( GNO_PARSE( argc , argv ) < 0 )
	{
		return 1;
	}
	/*
	 * options parsed
	 */

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

	from = 0;
	to = 0;
	arc = 0;

	printf( "Graph generate:\n" );

	printf( "Add horizontal and vertical links:\n" );
	for ( irow = 0 ; irow < NROWS ; irow ++ ) {
		for ( icol = 0 ; icol < NCOLS ; icol ++ ) {
			itcol = icol + 1;
			itrow = irow + 1;

			if ( itcol < NCOLS ) {
				from = irow * NCOLS + icol;
				to = irow * NCOLS + itcol;
				cost = FACTOR;
				arc ++;
				if ( _add_link( &graph, from , to , cost , arc ) ) {
					return 1;
				}
#ifdef BIDIRECTIONAL
				arc ++;
				if ( _add_link( &graph, to , from , cost , arc ) ) {
					return 1;
				}
#endif
			}

			if ( itrow < NROWS ) {
				from = irow * NCOLS + icol;
				to = itrow * NCOLS + icol;
				cost = FACTOR;
				arc ++;
				if ( _add_link( &graph, from , to , cost , arc ) ) {
					return 1;
				}
#ifdef BIDIRECTIONAL
				arc ++;
				if ( _add_link( &graph, to , from , cost , arc ) ) {
					return 1;
				}
#endif
			}
		}
	}

	printf( "\nAdd oblique links:\n" );
	for ( irow = 0 ; irow < NROWS ; irow ++ ) {
		for ( icol = 0 ; icol < NCOLS ; icol ++ ) {
			itcol = icol + 1;
			itrow = irow + 1;

			if ( itrow < NROWS && itcol < NCOLS ) {
				from = irow * NCOLS + icol;
				to = itrow * NCOLS + itcol;
				cost = (gnInt32_t)(sqrt(2.0) * (double)FACTOR);
				arc ++;
				if ( _add_link( &graph, from , to , cost , arc ) ) {
					return 1;
				}
#ifdef BIDIRECTIONAL
				arc ++;
				if ( _add_link( &graph, to  , from , cost , arc ) ) {
					return 1;
				}
#endif
			}
		}
	}
	printf( "\nDone.\n\n" );


	printf( "Graph flattening:\n" );
	nret = gnGrpFlatten( & graph );
	if ( nret < 0 )
	{
		fprintf( stderr , "gnGrpFlatten error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}
	printf( "Done.\n\n" );


	printf( "Graph write:\n" );
	if ( (fd = open( pszFileout , O_WRONLY | O_CREAT | O_TRUNC, 0666 )) < 0 )
	{
		perror( "open" ); return 1;
	}
	nret = gnGrpWrite( & graph , fd );
	if ( nret < 0 )
	{
		fprintf( stderr , "gnGrpWrite error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}
	printf( "Done.\n" );


	gnGrpRelease( & graph );
	return 0;
}
