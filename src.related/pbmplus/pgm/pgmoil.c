/* pgmoil.c - read a portable graymap and turn into an oil painting
**
** Copyright (C) 1990 by Wilson Bent (whb@hoh-2.att.com)
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdio.h>
#include "pgm.h"

static int hist[PGM_MAXMAXVAL+1];

void main( argc, argv )
int argc;
char* argv[];
    {
    FILE* ifp;
    gray midval, maxval;
    gray** gin;
    gray** gout;
    int argn, rows, n, cols, row;
    register int col, drow, dcol;
    int i;
    char* usage = "[-n <n>] [pgmfile]";

    pgm_init( &argc, argv );

    argn = 1;
    n = 3;		/* DEFAULT VALUE */

    /* Check for flags. */
    if ( argn < argc && argv[argn][0] == '-' )
	{
	if ( argv[argn][1] == 'n' )
	    {
	    ++argn;
	    if ( argn == argc || sscanf( argv[argn], "%d", &n ) != 1 )
		pm_usage( usage );
	    }
	else
	    pm_usage( usage );
	++argn;
	}

    if ( argn < argc )
	{
	ifp = pm_openr( argv[argn] );
	++argn;
	}
    else
	ifp = stdin;

    if ( argn != argc )
	pm_usage( usage );

    gin = pgm_readpgm( ifp, &cols, &rows, &maxval );
    pm_close( ifp );
    gout = pgm_allocarray( cols, rows );

    /* Build histogram. */
    for ( row = n; row < rows - n; ++row )
	for ( col = n; col < cols - n; ++col )
	    {
	    for ( i = 0; i <= maxval; ++i )
		hist[i] = 0;

	    for ( drow = row - n; drow <= row + n; ++drow )
		for ( dcol = col - n; dcol <= col + n; ++dcol )
		    ++hist[ (int) (gin[drow][dcol]) ];

	    for ( drow = dcol = 0; dcol < maxval; ++dcol )
		if ( hist[dcol] > drow )
		    {
		    drow = hist[dcol];
		    midval = dcol;
		    }
	    gout[row][col] = midval;
	    }

    pgm_writepgm( stdout, gout, cols, rows, maxval, 0 );
    pgm_freearray( gout, rows );

    exit( 0 );
    }
