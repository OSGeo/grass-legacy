/* pnmtorast.c - read a portable anymap and produce a Sun rasterfile
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
#include "rast.h"

#ifdef PPM
#include "ppmcmap.h"
#define MAXCOLORS 256
static colormap_t* make_pr_colormap();
#endif /*PPM*/

void main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp;
    xel** xels;
    xel* xelrow;
    xel p;
    register xel* xP;
#ifdef PPM
    colorhist_vector chv;
    colorhash_table cht;
#endif /*PPM*/
    colormap_t* pr_colormapP;
    int argn, pr_type, rows, cols, format, i;
    int depth, colors, linesize, row;
    register int col, bitcount;
    xelval maxval;
    struct pixrect* pr;
    unsigned char* data;
    register unsigned char* byteP;
    char* usage = "[-standard|-rle] [pnmfile]";

    pnm_init( &argc, argv );

    argn = 1;
    pr_type = RT_BYTE_ENCODED;

    while ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
	{
	if ( pm_keymatch( argv[argn], "-standard", 2 ) )
	    pr_type = RT_STANDARD;
	else if ( pm_keymatch( argv[argn], "-rle", 2 ) )
	    pr_type = RT_BYTE_ENCODED;
	else
	    pm_usage( usage );
	++argn;
	}

    if ( argn != argc )
	{
	ifp = pm_openr( argv[argn] );
	++argn;
	}
    else
	ifp = stdin;

    if ( argn != argc )
	pm_usage( usage );

    xels = pnm_readpnm( ifp, &cols, &rows, &maxval, &format );

    pm_close( ifp );

    /* Figure out the proper depth and colormap. */
    switch ( PNM_FORMAT_TYPE(format) )
	{
#ifdef PPM
	case PPM_TYPE:
	pm_message( "computing colormap...", 0,0,0,0,0 );
	chv = ppm_computecolorhist( xels, cols, rows, MAXCOLORS, &colors );
	if ( chv == (colorhist_vector) 0 )
	    {
	    pm_message(
		"Too many colors - proceeding to write a 24-bit non-mapped", 0,0,0,0,0 );
	    pm_message(
		"rasterfile.  If you want 8 bits, try doing a 'ppmquant %d'.",
		MAXCOLORS, 0,0,0,0 );
	    depth = 24;
	    pr_type = RT_FORMAT_RGB;
	    pr_colormapP = (colormap_t*) 0;
	    }
	else
	    {
	    pm_message( "%d colors found", colors, 0,0,0,0 );

	    if ( maxval != 255 )
		for ( i = 0; i < colors; ++i )
		    PPM_DEPTH( chv[i].color, chv[i].color, maxval, 255 );

	    /* Force white to slot 0 and black to slot 1, if possible. */
	    PPM_ASSIGN( p, 255, 255, 255 );
	    ppm_addtocolorhist( chv, &colors, MAXCOLORS, &p, 0, 0 );
	    PPM_ASSIGN( p, 0, 0, 0 );
	    ppm_addtocolorhist( chv, &colors, MAXCOLORS, &p, 0, 1 );

	    if ( colors == 2 )
		{
		/* Monochrome. */
		depth = 1;
		pr_colormapP = (colormap_t*) 0;
		}
	    else
		{
		/* Turn the ppm colormap into the appropriate Sun colormap. */
		depth = 8;
		pr_colormapP = make_pr_colormap( chv, colors );
		}
	    cht = ppm_colorhisttocolorhash( chv, colors );
	    ppm_freecolorhist( chv );
	    }

	break;
#endif /*PPM*/

#ifdef PGM
	case PGM_TYPE:
	depth = 8;
	pr_colormapP = (colormap_t*) 0;
	break;
#endif /*PGM*/

	default:
	depth = 1;
	pr_colormapP = (colormap_t*) 0;
	break;
	}

    if ( maxval > 255 && depth != 1 )
	pm_message(
	    "maxval is not 255 - automatically rescaling colors", 0,0,0,0,0 );
    
    /* Allocate space for the Sun-format image. */
    if ( (pr = mem_create(cols, rows, depth)) == (struct pixrect*) 0 )
	pm_error( "unable to create new pixrect", 0,0,0,0,0 );
    data = ( (struct mpr_data*) pr->pr_data )->md_image;
    linesize = ( (struct mpr_data*) pr->pr_data )->md_linebytes;

    /* And compute the Sun image.  The variables at this point are:
    **   cht is null or not
    **   depth is 1, 8, or 24
    */
    for ( row = 0; row < rows; ++row )
	{
	xelrow = xels[row];
	byteP = data;
	switch ( depth )
	    {
	    case 1:
	    *byteP = 0;
	    bitcount = 7;
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		{
		register int color;

                switch ( PNM_FORMAT_TYPE(format) )
                    {
#ifdef PPM
                    case PPM_TYPE:
		    if ( maxval != 255 )
			PPM_DEPTH( *xP, *xP, maxval, 255 );
		    color = ppm_lookupcolor( cht, xP );
		    if ( color == -1 )
			pm_error(
			    "color not found?!?  row=%d col=%d  r=%d g=%d b=%d",
			    row, col, PPM_GETR(*xP), PPM_GETG(*xP),
			    PPM_GETB(*xP) );
                    break;
#endif /*PPM*/
		    default:
		    color = PNM_GET1( *xP );
                    break;
                    }
		if ( ! color )
		    *byteP |= 1 << bitcount;
		--bitcount;
		if ( bitcount < 0 )
		    {
		    ++byteP;
		    *byteP = 0;
		    bitcount = 7;
		    }
		}
	    break;

	    case 8:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		{
		register int color;

                switch ( PNM_FORMAT_TYPE(format) )
                    {
#ifdef PPM
                    case PPM_TYPE:
		    if ( maxval != 255 )
			PPM_DEPTH( *xP, *xP, maxval, 255 );
		    color = ppm_lookupcolor( cht, xP );
		    if ( color == -1 )
			pm_error(
			    "color not found?!?  row=%d col=%d  r=%d g=%d b=%d",
			    row, col, PPM_GETR(*xP), PPM_GETG(*xP),
			    PPM_GETB(*xP) );
                    break;
#endif /*PPM*/
#ifdef PGM
                    case PGM_TYPE:
		    color = PNM_GET1( *xP );
		    if ( maxval != 255 )
			color = color * 255 / maxval;
                    break;
#endif /*PGM*/
		    default:
		    color = PNM_GET1( *xP );
                    }
		*byteP++ = color;
		}
	    break;

#ifdef PPM
	    case 24:
	    /* If depth is 24, we do NOT have a valid cht. */
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		{
		if ( maxval != 255 )
		    PPM_DEPTH( *xP, *xP, maxval, 255 );
		*byteP++ = PPM_GETR( *xP );
		*byteP++ = PPM_GETG( *xP );
		*byteP++ = PPM_GETB( *xP );
		}
	    break;
#endif /*PPM*/

	    default:
	    pm_error( "can't happen", 0,0,0,0,0 );
	    }
	data += linesize;
	}

    /* Finally, write the sucker out. */
    pr_dump( pr, stdout, pr_colormapP, pr_type, 0 );

    exit( 0 );
    }

#ifdef PPM
static colormap_t*
make_pr_colormap( chv, colors )
    colorhist_vector chv;
    int colors;
    {
    colormap_t* pr_colormapP;
    int i;

    pr_colormapP = (colormap_t*) malloc( sizeof(colormap_t) );
    if ( pr_colormapP == (colormap_t*) 0 )
	pm_error( "out of memory", 0,0,0,0,0 );
    pr_colormapP->type = RMT_EQUAL_RGB;
    pr_colormapP->length = MAXCOLORS;
    pr_colormapP->map[0] =
	(unsigned char*) malloc( MAXCOLORS * sizeof(unsigned char) );
    pr_colormapP->map[1] =
	(unsigned char*) malloc( MAXCOLORS * sizeof(unsigned char) );
    pr_colormapP->map[2] =
	(unsigned char*) malloc( MAXCOLORS * sizeof(unsigned char) );
    if ( pr_colormapP->map[0] == 0 || pr_colormapP->map[1] == 0 ||
	 pr_colormapP->map[2] == 0 )
	pm_error( "out of memory", 0,0,0,0,0 );

    for ( i = 0; i < colors; ++i )
	{
	pr_colormapP->map[0][i] = PPM_GETR( chv[i].color );
	pr_colormapP->map[1][i] = PPM_GETG( chv[i].color );
	pr_colormapP->map[2][i] = PPM_GETB( chv[i].color );
	}
    for ( ; i < MAXCOLORS; ++i )
	pr_colormapP->map[0][i] = pr_colormapP->map[1][i] =
	    pr_colormapP->map[2][i] = 0;

    return pr_colormapP;
    }

#ifdef notdef
dump_map( chv, colors )
    colorhist_vector chv;
    int colors;
    {
    int i;

    for ( i = 0; i < colors; ++i )
	pm_message(
	    "%d: %d %d %d  (%d)\n", i, PPM_GETR(chv[i].color),
	    PPM_GETG(chv[i].color), PPM_GETB(chv[i].color), chv[i].count );
    }
#endif /*notdef*/
#endif /*PPM*/
