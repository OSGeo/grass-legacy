/* pnmpaste.c - paste a rectangle into a portable anymap
**
** Copyright (C) 1989 by Jef Poskanzer.
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
    FILE* ifp1;
    FILE* ifp2;
    register xel** xels1;
    register xel** xels2;
    register xel* x1P;
    register xel* x2P;
    xelval maxval1, maxval2, newmaxval;
    int argn, rows1, cols1, format1, x, y;
    int rows2, cols2, format2, newformat, row;
    register int col;
    char* usage = "frompnmfile x y [intopnmfile]";

    pnm_init( &argc, argv );

    argn = 1;

    if ( argn == argc )
	pm_usage( usage );
    ifp1 = pm_openr( argv[argn] );
    ++argn;

    if ( argn == argc )
	pm_usage( usage );
    if ( sscanf( argv[argn], "%d", &x ) != 1 )
	pm_usage( usage );
    ++argn;
    if ( argn == argc )
	pm_usage( usage );
    if ( sscanf( argv[argn], "%d", &y ) != 1 )
	pm_usage( usage );
    ++argn;

    if ( argn != argc )
	{
	ifp2 = pm_openr( argv[argn] );
	++argn;
	}
    else
	ifp2 = stdin;

    if ( argn != argc )
	pm_usage( usage );

    xels1 = pnm_readpnm( ifp1, &cols1, &rows1, &maxval1, &format1 );
    pm_close( ifp1 );

    xels2 = pnm_readpnm( ifp2, &cols2, &rows2, &maxval2, &format2 );
    pm_close( ifp2 );

    if ( format1 > format2 )
	{
	newformat = format1;
	newmaxval = maxval1;
	}
    else
	{
	newformat = format2;
	newmaxval = maxval2;
	}
    pnm_promoteformat( xels1, cols2, rows1, maxval1, format1, newmaxval, newformat );
    pnm_promoteformat( xels2, cols2, rows2, maxval2, format2, newmaxval, newformat );

    if ( x <= -cols2 )
	pm_error(
	    "x is too negative -- the second anymap has only %d cols",
	    cols2, 0,0,0,0 );
    else if ( x >= cols2 )
	pm_error(
	    "x is too large -- the second anymap has only %d cols",
	    cols2, 0,0,0,0 );
    if ( y <= -rows2 )
	pm_error(
	    "y is too negative -- the second anymap has only %d rows",
	    rows2, 0,0,0,0 );
    else if ( y >= rows2 )
	pm_error(
	    "y is too large -- the second anymap has only %d rows",
	    rows2, 0,0,0,0 );

    if ( x < 0 )
	x += cols2;
    if ( y < 0 )
	y += rows2;

    if ( x + cols1 > cols2 )
	pm_error(
	    "x + width is too large by %d pixels", x + cols1 - cols2, 0,0,0,0 );
    if ( y + rows1 > rows2 )
	pm_error(
	    "y + height is too large by %d pixels", y + rows1 - rows2,
	    0,0,0,0 );

    for ( row = 0; row < rows1; ++row )
	{
        for ( col = 0, x1P = xels1[row], x2P = &(xels2[row+y][x]); col < cols1; ++col, ++x1P, ++x2P )
	    *x2P = *x1P;
	}

    pnm_writepnm( stdout, xels2, cols2, rows2, newmaxval, newformat, 0 );

    exit( 0 );
    }
