/* pbmpaste.c - paste a rectangle into a portable bitmap
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

#include "pbm.h"

void main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp1;
    FILE* ifp2;
    register bit* bitrow1;
    register bit** bits2;
    register bit* b1P;
    register bit* b2P;
    int argn, rows1, cols1, format1, x, y, rows2, cols2, row, col;
    char function;
    char* usage = "[-replace|-or|-and|-xor] frompbmfile x y [intopbmfile]";

    pbm_init( &argc, argv );

    argn = 1;
    function = 'r';

    /* Check for flags. */
    if ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
	{
	if ( pm_keymatch( argv[argn], "-replace", 2 ) )
	    function = 'r';
	else if ( pm_keymatch( argv[argn], "-or", 2 ) )
	    function = 'o';
	else if ( pm_keymatch( argv[argn], "-and", 2 ) )
	    function = 'a';
	else if ( pm_keymatch( argv[argn], "-xor", 2 ) )
	    function = 'x';
	else
	    pm_usage( usage );
	++argn;
	}

    if ( argn == argc )
	pm_usage( usage );
    ifp1 = pm_openr( argv[argn] );
    pbm_readpbminit( ifp1, &cols1, &rows1, &format1 );
    bitrow1 = pbm_allocrow( cols1 );
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

    if ( argn == argc )
	ifp2 = stdin;
    else
	{
	ifp2 = pm_openr( argv[argn] );
	++argn;
	}
    bits2 = pbm_readpbm( ifp2, &cols2, &rows2 );
    pm_close( ifp2 );

    if ( x <= -cols2 )
	pm_error(
	    "x is too negative -- the second bitmap has only %d cols",
	    cols2, 0,0,0,0 );
    else if ( x >= cols2 )
	pm_error(
	    "x is too large -- the second bitmap has only %d cols",
	    cols2, 0,0,0,0 );
    if ( y <= -rows2 )
	pm_error(
	    "y is too negative -- the second bitmap has only %d rows",
	    rows2, 0,0,0,0 );
    else if ( y >= rows2 )
	pm_error(
	    "y is too large -- the second bitmap has only %d rows",
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
	    "y + height is too large by %d pixels", y + rows1 - rows2, 0,0,0,0);

    if ( argn != argc )
	pm_usage( usage );

    for ( row = 0; row < rows1; ++row )
	{
	pbm_readpbmrow( ifp1, bitrow1, cols1, format1 );
        for ( col = 0, b1P = bitrow1, b2P = &(bits2[row+y][x]); col < cols1; ++col, ++b1P, ++b2P )
	    switch ( function )
		{
		case 'r':
		*b2P = *b1P;
		break;

		case 'o':
		*b2P = ( *b1P == PBM_WHITE || *b2P == PBM_WHITE ?
			 PBM_WHITE : PBM_BLACK );
		break;

		case 'a':
		*b2P = ( *b1P == PBM_WHITE && *b2P == PBM_WHITE ?
			 PBM_WHITE : PBM_BLACK );
		break;

		case 'x':
		*b2P = ( ( *b1P == PBM_WHITE && *b2P != PBM_WHITE ) ||
			 ( *b1P != PBM_WHITE && *b2P == PBM_WHITE ) ?
			 PBM_WHITE : PBM_BLACK );
		break;

		default:
		pm_error( "can't happen", 0,0,0,0,0 );
		}
	}

    pm_close( ifp1 );

    pbm_writepbm( stdout, bits2, cols2, rows2, 0 );

    exit( 0 );
    }
