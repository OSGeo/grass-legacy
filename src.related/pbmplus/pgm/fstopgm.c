/* fstopgm.c - read a Usenix FaceSaver(tm) file and produce a portable graymap
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

#include "pgm.h"

static int gethexit();

void main( argc, argv )
int argc;
char *argv[];
    {
    FILE *ifp;
    register gray **grays, *gP;
    int argn, row;
    register int col;
    int maxval;
    int rows = 0, cols = 0, depth = 0, xrows = 0, xcols = 0, xdepth = 0;
#define STRSIZE 1000
    char buf[STRSIZE], firstname[STRSIZE], lastname[STRSIZE], email[STRSIZE];

    pgm_init( &argc, argv );

    argn = 1;

    if ( argn < argc )
	{
	ifp = pm_openr( argv[argn] );
	argn++;
	}
    else
	ifp = stdin;

    if ( argn != argc )
	pm_usage( "[fsfile]" );

    /* Read the FaceSaver(tm) header. */
    for ( ; ; )
	{
	if ( fgets( buf, STRSIZE, ifp ) == (char *) 0 )
	    pm_error( "error reading header", 0,0,0,0,0 );

	/* Blank line ends header. */
	if ( strlen( buf ) == 1 )
	    break;

	if ( sscanf( buf, "FirstName: %s\n", firstname ) == 1 )
	    ;
	else if ( sscanf( buf, "LastName: %s\n", lastname ) == 1 )
	    ;
	else if ( sscanf( buf, "E-mail: %s\n", email ) == 1 )
	    ;
	else if ( sscanf( buf, "PicData: %d %d %d\n",
			  &cols, &rows, &depth ) == 3 )
	    {
	    if ( depth != 8 )
		pm_error(
		    "can't handle 'PicData' depth other than 8", 0,0,0,0,0 );
	    }
	else if ( sscanf( buf, "Image: %d %d %d\n",
			  &xcols, &xrows, &xdepth ) == 3 )
	    {
	    if ( xdepth != 8 )
		pm_error(
		    "can't handle 'Image' depth other than 8", 0,0,0,0,0 );
	    }
	}
    if ( cols <= 0 || rows <= 0 )
	pm_error( "invalid header", 0,0,0,0,0 );
    maxval = pm_bitstomaxval( depth );
    if ( maxval > PGM_MAXMAXVAL )
	pm_error(
	    "depth is too large - try recompiling with a larger gray type",
	    0,0,0,0,0 );
    if ( xcols != 0 && xrows != 0 && ( xcols != cols || xrows != rows ) )
	{
	float rowratio, colratio;

	rowratio = (float) xrows / (float) rows;
	colratio = (float) xcols / (float) cols;
	pm_message(
	    "warning, non-square pixels; to fix do a 'pnmscale -%cscale %g'.)\n",
	    rowratio > colratio ? 'y' : 'x',
	    rowratio > colratio ? rowratio / colratio : colratio / rowratio,
	    0,0,0 );
	}

    /* Now read the hex bits. */
    grays = pgm_allocarray( cols, rows );
    for ( row = rows - 1; row >= 0; row--)
	{
	for ( col = 0, gP = grays[row]; col < cols; col++, gP++ )
	    {
	    *gP = gethexit( ifp ) << 4;
	    *gP += gethexit( ifp );
	    }
	}
    pm_close( ifp );

    /* And write out the graymap. */
    pgm_writepgm( stdout, grays, cols, rows, (gray) maxval, 0 );

    exit( 0 );
    }

static int
gethexit( ifp )
FILE *ifp;
    {
    register int i;
    register char c;

    for ( ; ; )
	{
	i = getc( ifp );
	if ( i == EOF )
	    pm_error( "EOF / read error", 0,0,0,0,0 );
	c = (char) i;
	if ( c >= '0' && c <= '9' )
	    return c - '0';
	else if ( c >= 'A' && c <= 'F' )
	    return c - 'A' + 10;
	else if ( c >= 'a' && c <= 'f' )
	    return c - 'a' + 10;
	/* Else ignore - whitespace. */
	}
    }
