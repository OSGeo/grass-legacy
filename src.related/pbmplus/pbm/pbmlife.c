/* pbmlife.c - read a portable bitmap and apply Conway's rules of Life to it
**
** Copyright (C) 1988 by Jef Poskanzer.
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
char *argv[];
    {
    FILE *ifp;
    register bit **bits, *bitrow;
    int rows, cols, row, col, count;

    pbm_init( &argc, argv );

    if ( argc > 2 )
	pm_usage( "[pbmfile]" );

    if ( argc == 2 )
	ifp = pm_openr( argv[1] );
    else
	ifp = stdin;

    bits = pbm_readpbm( ifp, &cols, &rows );
    pbm_writepbminit( stdout, cols, rows, 0 );
    bitrow = pbm_allocrow( cols );

    pm_close( ifp );

    for ( row = 0; row < rows; row++ )
	{
        for ( col = 0; col < cols; col++ )
	    {
	    /* Check the neighborhood, with an unrolled double loop. */
	    count = 0;
	    if ( row - 1 >= 0 )
		{
		/* upper left */
		if ( col - 1 >= 0 && bits[row - 1][col - 1] )
		    count++;
		/* upper center */
		if ( bits[row - 1][col] )
		    count++;
		/* upper right */
		if ( col + 1 < cols && bits[row - 1][col + 1] )
		    count++;
		}
	    /* left */
	    if ( col - 1 >= 0 && bits[row][col - 1] )
		count++;
	    /* right */
	    if ( col + 1 < cols && bits[row][col + 1] )
		count++;
	    if ( row + 1 < rows )
		{
		/* lower left */
		if ( col - 1 >= 0 && bits[row + 1][col - 1] )
		    count++;
		/* lower center */
		if ( bits[row + 1][col] )
		    count++;
		/* lower right */
		if ( col + 1 < cols && bits[row + 1][col + 1] )
		    count++;
		}

	    /* And compute the new value. */
	    if ( bits[row][col] )
		if ( count == 2 || count == 3 )
		    bitrow[col] = 1;
		else
		    bitrow[col] = 0;
	    else
		if ( count == 3 )
		    bitrow[col] = 1;
		else
		    bitrow[col] = 0;
	    }
	pbm_writepbmrow( stdout, bitrow, cols, 0 );
	}

    exit( 0 );
    }
