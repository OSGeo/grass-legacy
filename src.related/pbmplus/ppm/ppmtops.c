/* ppmtops.c - read a portable pixmap and produce a PostScript file
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

#include "ppm.h"

static void putinit(), putitem(), putpix(), putgray(), putrest();

void main( argc, argv )
int argc;
char *argv[];
    {
    FILE *ifp;
    register pixel *pixrow, *pP;
    int argn, rows, cols, format, bps, padright, row, col;
    pixval maxval, nmaxval;
    float scale;
    char name[100], *cp;
    char *usage = "[-scale <x>] [ppmfile]";

    ppm_init( &argc, argv );

    argn = 1;
    scale = 1.0;

    /* Check for flags. */
    while ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
	{
	if ( pm_keymatch( argv[argn], "-scale", 2 ) )
	    {
	    ++argn;
	    if ( argn == argc || sscanf( argv[argn], "%f", &scale ) != 1 )
		pm_usage( usage );
	    }
	else
	    pm_usage( usage );
	++argn;
	}

    if ( argn < argc )
	{
	ifp = pm_openr( argv[argn] );
	strcpy( name, argv[argn] );
	if ( strcmp( name, "-" ) == 0 )
	    strcpy( name, "noname" );

	if ( ( cp = index( name, '.' ) ) != 0 )
	    *cp = '\0';
	++argn;
	}
    else
	{
	ifp = stdin;
	strcpy( name, "noname" );
	}

    if ( argn != argc )
	pm_usage( usage );

    ppm_readppminit( ifp, &cols, &rows, &maxval, &format );
    pixrow = ppm_allocrow( cols );

    /* Figure out bps. */
    bps = pm_maxvaltobits( (int) maxval );
    if ( bps > 2 && bps < 4 )
	bps = 4;
    else if ( bps > 4 && bps < 8 )
	bps = 8;
    else if ( bps > 8 )
	pm_error( "maxval of %d is too large for PostScript", maxval, 0,0,0,0 );
    nmaxval = pm_bitstomaxval( bps );
    
    /* Compute padding to round cols * bps up to the nearest multiple of 8. */
    padright = ( ( ( cols * bps + 7 ) / 8 ) * 8 - cols * bps ) / bps;

    putinit( name, cols, rows, bps, scale );
    for ( row = 0; row < rows; row++ )
	{
	ppm_readppmrow( ifp, pixrow, cols, maxval, format );
        for ( col = 0, pP = pixrow; col < cols; col++, pP++ )
	    {
	    if ( maxval != nmaxval )
		PPM_DEPTH( *pP, *pP, maxval, nmaxval );
	    putpix( *pP );
	    }
	for ( col = 0; col < padright; col++ )
	    putpix( pixrow[0] );
        }

    pm_close( ifp );

    putrest( );

    exit( 0 );
    }

int bitspersample, item, bitsperitem, bitshift, itemsperline, items;
#define HSBUFSIZ 384

static void
putinit( name, cols, rows, bps, scale )
char *name;
int cols, rows, bps;
float scale;
    {
    float scols, srows, llx, lly;

    scols = scale * cols * 0.96;	/*   0.96 is the multiple of   */
    srows = scale * rows * 0.96;	/* 72/300 that is closest to 1 */
    llx = ( 612.0 - scols ) / 2;
    lly = ( 792.0 - srows ) / 2;
    if ( llx < 0.0 || lly < 0.0 )
	pm_message( "warning, image too large for page", 0,0,0,0,0 );

    printf( "%%!PS-Adobe-2.0 EPSF-2.0)\n" );
    printf( "%%%%Creator: ppmtops\n" );
    printf( "%%%%Title: %s.ps\n", name );
    printf( "%%%%Pages: 1\n" );
    printf(
	"%%%%BoundingBox: %d %d %d %d\n",
	(int) llx, (int) lly, (int) ( llx + scols ), (int) ( lly + srows ) );
    printf( "%%%%EndComments\n" );
    printf( "%%%%EndProlog\n" );
    printf( "%%%%Page: 1 1\n" );
    printf( "/picstr %d string def\n", HSBUFSIZ );
    printf( "gsave\n" );
    printf( "%g %g translate\n", llx, lly );
    printf( "%g %g scale\n", scols, srows );
    printf( "%d %d %d\n", cols, rows, bps );
    printf( "[ %d 0 0 -%d 0 %d ]\n", cols, rows, rows );
    printf( "{ currentfile picstr readhexstring pop }\n" );
    printf( "false 3\n" );
    printf( "colorimage\n" );

    bitspersample = bps;
    itemsperline = items = 0;
    item = 0;
    bitsperitem = 0;
    bitshift = 8 - bitspersample;
    }

static void
putitem( )
    {
    char *hexits = "0123456789abcdef";

    if ( itemsperline == 30 )
	{
	putchar( '\n' );
	itemsperline = 0;
	}
    putchar( hexits[item >> 4] );
    putchar( hexits[item & 15] );
    itemsperline++;
    items++;
    item = 0;
    bitsperitem = 0;
    bitshift = 8 - bitspersample;
    }

static void
putpix( p )
pixel p;
    {
    putgray( PPM_GETR( p ) );
    putgray( PPM_GETG( p ) );
    putgray( PPM_GETB( p ) );
    }

static void
putgray( g )
pixval g;
    {
    if ( bitsperitem == 8 )
	putitem( );
    item += g << bitshift;
    bitsperitem += bitspersample;
    bitshift -= bitspersample;
    }

static void
putrest( )
    {
    if ( bitsperitem > 0 )
	putitem( );
    while ( items % HSBUFSIZ != 0 )
	putitem( );
    printf( "\n" );
    printf( "grestore\n" );
    printf( "showpage\n" );
    printf( "%%%%Trailer\n" );
    }
