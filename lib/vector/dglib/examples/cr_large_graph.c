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

/*
#define NROWS 600
#define NCOLS 400
#define FACTOR 10000
#define BIDIRECTIONAL 1
*/


static int _add_link( 	gnGrpGraph_s * pgraph ,
						gnInt32_t from , gnInt32_t to , gnInt32_t cost , gnInt32_t arc ,
						char chDirection ,
						int fLast )
{
	int nret;

	gnInt32_t xyz_from[3] , xyz_to[3] , direction[2];

	if ( ! fLast ) {
		/* setup node attributes with the x y coords in the grid by
	   	   reversing the calculation done the main loop */
		xyz_from[0] = from % NCOLS;
		xyz_from[1] = from / NCOLS;
		xyz_from[2] = 0;

		xyz_to[0] = to % NCOLS;
		xyz_to[1] = to / NCOLS;
		xyz_to[2] = 0;

		/* chDirection says if the link direction is 'u'=toward the top 'b'=the bot. 'l'=the left 'r'=the right
	   	   'o'=toward right-bottom 'O'=toward top-left * Account for this in the link attributes */
		direction[0] = (gnInt32_t)chDirection;
		direction[1] = 0;


		nret = gnGrpAddLinkX( pgraph , from , to , cost , arc , xyz_from , xyz_to , direction , 0 );
		if ( nret < 0 ) {
			fprintf( stderr, "gnGrpAddLink error: %s\n", gnGrpStrerror( pgraph ) );
			return 1;
		}
	}

	if ( (arc % 1024) == 0 || fLast ) {
#ifndef GNGRP_STATS
		printf( "add arc %07d - from %07d - to %07d - cost %07d\r",
			   	arc , from , to , cost );
#else
		printf( "add arc %07ld - from %07ld - to %07ld - cost %07ld . Clock: tot %09ld nt %09ld nh %09ld o %09ld\r" ,
			   	arc , from , to , cost ,
			 	pgraph->clkAddLink / pgraph->cAddLink,
			 	pgraph->clkNodeTree / pgraph->cAddLink,
			 	pgraph->clkNodeHeap / pgraph->cAddLink,
			 	(pgraph->clkAddLink - (pgraph->clkNodeTree + pgraph->clkNodeHeap)) / pgraph->cAddLink
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
	int				irowsave , icolsave;

	gnInt32_t	 	from, to, arc, cost;

	/* node attributes */
	gnInt32_t		xyz[3];

	/* link attributes */
	gnInt32_t		direction[2];

	gnInt32_t	 	opaqueset[ 16 ] = {
		FACTOR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};

	/* program options
	 */
 	char	*	pszFileout;
	Boolean		fInterlaced;
 
	GNO_BEGIN/* short   long        	default     variable        help */
 	GNO_OPTION( "g", 	"graph", 		NULL ,  	& pszFileout ,	"Output Graph file" )
 	GNO_SWITCH( "i", 	"interlaced", 	False , 	& fInterlaced ,	"Avoid node ids sorting at insertion" )
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
	printf( "graph initialize..." ); fflush(stdout);
	gnGrpInitialize (
					& graph , 			/* graph context to initialize */
					1 ,					/* version (so far is always 1) */
					sizeof(xyz) ,		/* node attributes size */
					sizeof(direction) ,	/* link attributes size */
					opaqueset			/* opaque graph parameters */
					);
	printf( "done.\n" );

	from = 0;
	to = 0;
	arc = 0;


	printf( "add horizontal and vertical links...\n" );
	for ( irow = 0 ; irow < NROWS ; irow ++ ) {

		if ( fInterlaced == True ) {
			irowsave = irow;
			if ( irow % 2 ) irow = NROWS - irow;
		}

		for ( icol = 0 ; icol < NCOLS ; icol ++ ) {

			if ( fInterlaced == True ) {
					icolsave = icol;
					if ( icol % 2 ) icol = NCOLS - icol;
			}

			itcol = icol + 1;
			itrow = irow + 1;

			if ( itcol < NCOLS ) {
				from = irow * NCOLS + icol;
				to = irow * NCOLS + itcol;
				cost = FACTOR;
				arc ++;
				if ( _add_link( &graph, from , to , cost , arc , 'r' , 0 ) ) {
					return 1;
				}
#ifdef BIDIRECTIONAL
				arc ++;
				if ( _add_link( &graph, to , from , cost , arc , 'l' , 0 ) ) {
					return 1;
				}
#endif
			}

			if ( itrow < NROWS ) {
				from = irow * NCOLS + icol;
				to = itrow * NCOLS + icol;
				cost = FACTOR;
				arc ++;
				if ( _add_link( &graph, from , to , cost , arc , 'b' , 0 ) ) {
					return 1;
				}
#ifdef BIDIRECTIONAL
				arc ++;
				if ( _add_link( &graph, to , from , cost , arc , 't' , 0 ) ) {
					return 1;
				}
#endif
			}

			if ( fInterlaced == True ) icol = icolsave;
		}

		if ( fInterlaced == True ) irow = irowsave;
	}
	_add_link( &graph, to  , from , cost , arc , 'x' , 1 );


#if 0
	printf( "\nadd oblique links...\n" );
	for ( irow = 0 ; irow < NROWS ; irow ++ ) {
		for ( icol = 0 ; icol < NCOLS ; icol ++ ) {
			itcol = icol + 1;
			itrow = irow + 1;

			if ( itrow < NROWS && itcol < NCOLS ) {
				from = irow * NCOLS + icol;
				to = itrow * NCOLS + itcol;
				cost = (gnInt32_t)(sqrt(2.0) * (double)FACTOR);
				arc ++;
				if ( _add_link( &graph, from , to , cost , arc , 'o' , 0 ) ) {
					return 1;
				}
#ifdef BIDIRECTIONAL
				arc ++;
				if ( _add_link( &graph, to  , from , cost , arc , 'O' , 0 ) ) {
					return 1;
				}
#endif
			}
		}
	}
	_add_link( &graph, to  , from , cost , arc , 'x' , 1 );
	printf( "\ndone.\n" );
#endif


	printf( "graph flattening..." ); fflush(stdout);
	nret = gnGrpFlatten( & graph );
	if ( nret < 0 )
	{
		fprintf( stderr , "\ngnGrpFlatten error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}
	printf( "done.\n" );


	printf( "graph write..." ); fflush(stdout);
	if ( (fd = open( pszFileout , O_WRONLY | O_CREAT | O_TRUNC, 0666 )) < 0 )
	{
		perror( "open" ); return 1;
	}
	nret = gnGrpWrite( & graph , fd );
	if ( nret < 0 )
	{
		fprintf( stderr , "\ngnGrpWrite error: %s\n" , gnGrpStrerror( & graph ) );
		return 1;
	}
	close( fd );
	printf( "done.\n" );


	printf( "graph release..." ); fflush(stdout);
	gnGrpRelease( & graph );
	printf( "program finished.\n" );
	return 0;
}
