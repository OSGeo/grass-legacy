/* pgmtops.c - read a portable graymap and produce a PostScript file
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

static void putinit(), putinit(), putitem(), putgray(), putrest(),
    rleputinit(), rleputbuffer(), rleputitem(), rleputgray(), rleputrest();

void main( argc, argv )
int argc;
char *argv[];
    {
    FILE *ifp;
    register gray *grayrow, *gP;
    int argn, rleflag, rows, cols, format, bps, padright, row, col;
    gray maxval, nmaxval;
    float scale;
    char name[100], *cp;
    char *usage = "[-rle|-runlength] [-scale <x>] [pgmfile]";

    pgm_init( &argc, argv );

    argn = 1;
    rleflag = 0;
    scale = 1.0;

    /* Check for flags. */
    while ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
	{
	if ( pm_keymatch( argv[argn], "-rle", 2 ) ||
	     pm_keymatch( argv[argn], "-runlength", 2 ) )
	    rleflag = 1;
	else if ( pm_keymatch( argv[argn], "-norle", 2 ) ||
	     pm_keymatch( argv[argn], "-norunlength", 2 ) )
	    rleflag = 0;
	else if ( pm_keymatch( argv[argn], "-scale", 2 ) )
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
	argn++;
	}
    else
	{
	ifp = stdin;
	strcpy( name, "noname" );
	}

    if ( argn != argc )
	pm_usage( usage );

    pgm_readpgminit( ifp, &cols, &rows, &maxval, &format );
    grayrow = pgm_allocrow( cols );

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

    if ( rleflag )
	rleputinit( name, cols, rows, bps, scale );
    else
	putinit( name, cols, rows, bps, scale );
    for ( row = 0; row < rows; row++ )
	{
	pgm_readpgmrow( ifp, grayrow, cols, maxval, format );
        for ( col = 0, gP = grayrow; col < cols; col++, gP++ )
	    {
	    if ( maxval != nmaxval )
		*gP = (int) *gP * nmaxval / maxval;
	    if ( rleflag )
		rleputgray( *gP );
	    else
		putgray( *gP );
	    }
	for ( col = 0; col < padright; col++ )
	    if ( rleflag )
		rleputgray( 0 );
	    else
		putgray( 0 );
        }

    pm_close( ifp );

    if ( rleflag )
	rleputrest( );
    else
	putrest( );

    exit( 0 );
    }


int bitspersample, item, bitsperitem, bitshift, itemsperline, items;
int rleitem, rlebitsperitem, rlebitshift;
int repeat, itembuf[128], count, repeatitem, repeatcount;
#define HSBUFSIZ 256

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

    printf( "%%!PS-Adobe-2.0 EPSF-2.0\n" );
    printf( "%%%%Creator: pgmtops\n" );
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
    printf( "image\n" );

    bitspersample = bps;
    itemsperline = items = 0;
    item = 0;
    bitsperitem = 0;
    bitshift = 8 - bitspersample;
    }

static void
putitem( )
    {
    static char *hexits = "0123456789abcdef";

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
putgray( g )
gray g;
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

static void
rleputinit( name, cols, rows, bps, scale )
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

    printf( "%%!PS-Adobe-2.0 EPSF-2.0\n" );
    printf( "%%%%Creator: pgmtops\n" );
    printf( "%%%%Title: %s.ps\n", name );
    printf( "%%%%Pages: 1\n" );
    printf(
	"%%%%BoundingBox: %d %d %d %d\n",
	(int) llx, (int) lly, (int) ( llx + scols ), (int) ( lly + srows ) );
    printf( "%%%%EndComments\n" );
    printf( "%%%%EndProlog\n" );
    printf( "/rlestr1 1 string def\n" );
    printf( "/rlestr 128 string def\n" );
    printf( "/readrlestring {\n" );
    printf( "  currentfile rlestr1 readhexstring pop  0 get\n" );
    printf( "  dup 127 le {\n" );
    printf( "    currentfile rlestr 0  4 3 roll  1 add  getinterval\n" );
    printf( "    readhexstring  pop\n" );
    printf( "  } {\n" );
    printf( "    256 exch sub  dup\n" );
    printf( "    currentfile rlestr1 readhexstring pop  0 get\n" );
    printf( "    exch 0 exch 1 exch 1 sub { rlestr exch 2 index put } for\n" );
    printf( "    pop  rlestr exch 0 exch getinterval\n" );
    printf( "  } ifelse\n" );
    printf( "} bind def\n" );
    printf( "%%%%EndProlog\n" );
    printf( "%%%%Page: 1 1\n" );
    printf( "gsave\n" );
    printf( "%g %g translate\n", llx, lly );
    printf( "%g %g scale\n", scols, srows );
    printf( "%d %d %d\n", cols, rows, bps );
    printf( "[ %d 0 0 -%d 0 %d ]\n", cols, rows, rows );
    printf( "{ readrlestring }\n" );
    printf( "image\n" );

    bitspersample = bps;
    itemsperline = items = 0;
    rleitem = 0;
    rlebitsperitem = 0;
    rlebitshift = 8 - bitspersample;
    repeat = 1;
    count = 0;
    }

static void
rleputbuffer( )
    {
    int i;

    if ( repeat )
	{
	item = 256 - count;
	putitem( );
	item = repeatitem;
	putitem( );
	}
    else
	{
	item = count - 1;
	putitem( );
	for ( i = 0; i < count; i++ )
	    {
	    item = itembuf[i];
	    putitem( );
	    }
	}
    repeat = 1;
    count = 0;
    }

static void
rleputitem( )
    {
    int i;

    if ( count == 128 )
	rleputbuffer( );

    if ( repeat && count == 0 )
	{ /* Still initializing a repeat buf. */
	itembuf[count] = repeatitem = rleitem;
	count++;
	}
    else if ( repeat )
	{ /* Repeating - watch for end of run. */
	if ( rleitem == repeatitem )
	    { /* Run continues. */
	    itembuf[count] = rleitem;
	    count++;
	    }
	else
	    { /* Run ended - is it long enough to dump? */
	    if ( count > 2 )
		{ /* Yes, dump a repeat-mode buffer and start a new one. */
		rleputbuffer( );
		itembuf[count] = repeatitem = rleitem;
		count++;
		}
	    else
		{ /* Not long enough - convert to non-repeat mode. */
		repeat = 0;
		itembuf[count] = repeatitem = rleitem;
		count++;
		repeatcount = 1;
		}
	    }
	}
    else
	{ /* Not repeating - watch for a run worth repeating. */
	if ( rleitem == repeatitem )
	    { /* Possible run continues. */
	    repeatcount++;
	    if ( repeatcount > 3 )
		{ /* Long enough - dump non-repeat part and start repeat. */
		count = count - ( repeatcount - 1 );
		rleputbuffer( );
		count = repeatcount;
		for ( i = 0; i < count; i++ )
		    itembuf[i] = rleitem;
		}
	    else
		{ /* Not long enough yet - continue as non-repeat buf. */
		itembuf[count] = rleitem;
		count++;
		}
	    }
	else
	    { /* Broken run. */
	    itembuf[count] = repeatitem = rleitem;
	    count++;
	    repeatcount = 1;
	    }
	}

    rleitem = 0;
    rlebitsperitem = 0;
    rlebitshift = 8 - bitspersample;
    }

static void
rleputgray( g )
gray g;
    {
    if ( rlebitsperitem == 8 )
	rleputitem( );
    rleitem += g << rlebitshift;
    rlebitsperitem += bitspersample;
    rlebitshift -= bitspersample;
    }

static void
rleputrest( )
    {
    if ( rlebitsperitem > 0 )
	rleputitem( );
    if ( count > 0 )
	rleputbuffer( );
    printf( "\n" );
    printf( "grestore\n" );
    printf( "showpage\n" );
    printf( "%%%%Trailer\n" );
    }
