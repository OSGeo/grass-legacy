/* cmuwmtopbm.c - read a CMU window manager bitmap and produce a portable bitmap
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
#include "cmuwm.h"

static void getinit();
static bit getbit();

void main( argc, argv )
int argc;
char *argv[];
    {
    FILE *ifp;
    register bit *bitrow, *bP;
    int rows, cols, padright, row, col;
    short depth;

    pbm_init( &argc, argv );

    if ( argc > 2 )
	pm_usage( "[cmuwmfile]" );

    if ( argc == 2 )
	ifp = pm_openr( argv[1] );
    else
	ifp = stdin;

    getinit( ifp, &cols, &rows, &depth, &padright );
    if ( depth != 1 )
	pm_error(
	    "CMU window manager file has depth of %d, must be 1",
	    (int) depth, 0,0,0,0 );

    pbm_writepbminit( stdout, cols, rows, 0 );
    bitrow = pbm_allocrow( cols );

    for ( row = 0; row < rows; row++ )
	{
	/* Get data. */
        for ( col = 0, bP = bitrow; col < cols; col++, bP++ )
	    *bP = getbit( ifp );
	/* Discard line padding */
        for ( col = 0; col < padright; col ++ )
	    (void) getbit( ifp );
	pbm_writepbmrow( stdout, bitrow, cols, 0 );
	}

    pm_close( ifp );

    exit( 0 );
    }


static int item, bitsperitem, bitshift;

static void
getinit( file, colsP, rowsP, depthP, padrightP )
FILE *file;
int *colsP, *rowsP, *padrightP;
short *depthP;
    {
    long l;

    if ( pm_readbiglong( file, &l ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );
    if ( l != CMUWM_MAGIC )
	pm_error( "bad magic number in CMU window manager file", 0,0,0,0,0 );
    if ( pm_readbiglong( file, &l ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );
    *colsP = l;
    if ( pm_readbiglong( file, &l ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );
    *rowsP = l;
    if ( pm_readbigshort( file, depthP ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );
    *padrightP = ( ( *colsP + 7 ) / 8 ) * 8 - *colsP;

    bitsperitem = 0;
    }

static bit
getbit( file )
FILE *file;
    {
    bit b;

    if ( bitsperitem == 0 )
	{
	item = getc( file );
	if ( item == EOF )
	    pm_error( "EOF / read error", 0,0,0,0,0 );
	bitsperitem = 8;
	bitshift = 7;
	}
    b = ( ( item >> bitshift) & 1 ) ? PBM_WHITE : PBM_BLACK;
    bitsperitem--;
    bitshift--;
    return b;
    }
