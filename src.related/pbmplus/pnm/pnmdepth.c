/* pnmdepth.c - change the maxval in a portable pixmap
**
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "pnm.h"

void main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp;
    xel* xelrow;
    register xel* xP;
    int argn, rows, cols, format, newformat, row;
    register int col;
    xelval maxval, newmaxval;
    int i;
    xelval newvals[PNM_MAXMAXVAL];
    char* usage = "newmaxval [pnmfile]";

    pnm_init( &argc, argv );

    argn = 1;

    if ( argn == argc )
	pm_usage( usage );
    if ( sscanf( argv[argn], "%d", &i ) != 1 )
	pm_usage( usage );
    newmaxval = i;
    ++argn;
    if ( newmaxval < 1 )
	pm_error( "newmaxval must be > 1", 0,0,0,0,0 );

    if ( argn != argc )
	{
	ifp = pm_openr( argv[argn] );
	++argn;
	}
    else
	ifp = stdin;

    if ( argn != argc )
	pm_usage( usage );

    pnm_readpnminit( ifp, &cols, &rows, &maxval, &format );
    xelrow = pnm_allocrow( cols );

#ifdef PGM
    /* Promote PBM files to PGM. */
    if ( PNM_FORMAT_TYPE(format) == PBM_TYPE )
	{
        newformat = PGM_TYPE;
	pm_message( "promoting from PBM to PGM", 0,0,0,0,0 );
	}
    else
        newformat = format;
#else /*PGM*/
    pm_error( "must have at least PGM defined - try reconfiguring", 0,0,0,0,0 );
#endif /*PGM*/

    for ( i = 0; i <= maxval; ++i )
	newvals[i] = i * newmaxval / maxval;

    pnm_writepnminit( stdout, cols, rows, newmaxval, newformat, 0 );

    for ( row = 0; row < rows; ++row )
	{
	pnm_readpnmrow( ifp, xelrow, cols, maxval, format );

	switch ( PNM_FORMAT_TYPE(format) )
	    {
#ifdef PPM
	    case PPM_TYPE:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		PPM_ASSIGN(
		    *xP, newvals[PPM_GETR(*xP)], newvals[PPM_GETG(*xP)],
		    newvals[PPM_GETB(*xP)] );
	    break;
#endif /*PPM*/
#ifdef PGM
	    default:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		PNM_ASSIGN1( *xP, newvals[PNM_GET1(*xP)] );
	    break;
#endif /*PGM*/
	    }

	pnm_writepnmrow( stdout, xelrow, cols, newmaxval, newformat, 0 );
	}

    pm_close( ifp );

    exit( 0 );
    }
