/* ybmtopbm.c - read a file from Bennet Yee's 'xbm' program and write a pbm.
**
** Written by Jamie Zawinski based on code (C) 1988 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdio.h>
#include "pbm.h"

#define YBM_MAGIC  ( '!'<<8 | '!' )

void main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp;
    bit* bitrow;
    register bit* bP;
    short rows, cols, padright, row, col;
    short depth;
    bit getbit();

    pbm_init( &argc, argv );

    if ( argc > 2 )
	pm_usage( "[ybmfile]" );

    if ( argc == 2 )
	ifp = pm_openr( argv[1] );
    else
	ifp = stdin;

    getinit( ifp, &cols, &rows, &depth, &padright );
    if ( depth != 1 )
	pm_error(
	    "YBM file has depth of %d, must be 1",
	    (int) depth, 0,0,0,0 );

    pbm_writepbminit( stdout, cols, rows, 0 );
    bitrow = pbm_allocrow( cols );

    for ( row = 0; row < rows; ++row )
	{
	/* Get data. */
        for ( col = 0, bP = bitrow; col < cols; ++col, ++bP )
	    *bP = getbit( ifp );
	/* Discard line padding */
        for ( col = 0; col < padright; ++col )
	    (void) getbit( ifp );
	pbm_writepbmrow( stdout, bitrow, cols, 0 );
	}

    pm_close( ifp );

    exit( 0 );
    }


int item;
int bitsperitem, bitshift;

getinit( file, colsP, rowsP, depthP, padrightP )
    FILE* file;
    short* colsP;
    short* rowsP;
    short* padrightP;
    short* depthP;
    {
    short magic;

    if ( pm_readbigshort( file, &magic ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );
    if ( magic != YBM_MAGIC )
	pm_error( "bad magic number in YBM file", 0,0,0,0,0 );
    if ( pm_readbigshort( file, colsP ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );
      if ( pm_readbigshort( file, rowsP ) == -1 )
	pm_error( "EOF / read error", 0,0,0,0,0 );

    *depthP = 1;
    *padrightP = ( ( *colsP + 15 ) / 16 ) * 16 - *colsP;
    bitsperitem = 0;
    }

bit
getbit( file )
    FILE* file;
    {
    bit b;

    if ( bitsperitem == 0 )
	{
	item = getc(file) | getc(file)<<8;
	if ( item == EOF )
	    pm_error( "EOF / read error", 0,0,0,0,0 );
	bitsperitem = 16;
	bitshift = 0;
	}
    b = ( ( item >> bitshift) & 1 ) ? PBM_BLACK : PBM_WHITE;
    --bitsperitem;
    ++bitshift;
    return b;
    }
