/*
** tifftopnm.c - converts a Tagged Image File to a portable anymap
**
** Derived by Jef Poskanzer from tif2ras.c, which is:
**
** Copyright (c) 1990 by Sun Microsystems, Inc.
**
** Author: Patrick J. Naughton
** naughton@wind.sun.com
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted,
** provided that the above copyright notice appear in all copies and that
** both that copyright notice and this permission notice appear in
** supporting documentation.
**
** This file is provided AS IS with no warranties of any kind.  The author
** shall have no liability with respect to the infringement of copyrights,
** trade secrets or any patents by this file or any part thereof.  In no
** event will the author be liable for any lost revenue or profits or
** other special, indirect and consequential damages.
*/

#include "pnm.h"
#include <tiffio.h>

void main( argc, argv )
    int argc;
    char* argv[];
    {
    int argn, cols, rows, grayscale, format;
    int numcolors;
    register TIFF* tif;
    TIFFDirectory* td;
    int row, i;
    register int col;
    u_char* buf;
    register u_char* inP;
    register xelval maxval;
    xel* xelrow;
    register xel* xP;
    xel colormap[PNM_MAXMAXVAL+1];
    int headerdump;
    register u_char sample;
    register int bps, bitsleft;
    char* usage = "[-headerdump] [tifffile]";

    pnm_init( &argc, argv );

    argn = 1;
    headerdump = 0;

    if ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
	{
	if ( pm_keymatch( argv[argn], "-headerdump", 2 ) )
	    headerdump = 1;
	else
	    pm_usage( usage );
	++argn;
	}

    if ( argn != argc )
	{
	tif = TIFFOpen( argv[argn], "r" );
	if ( tif == NULL )
	    pm_error( "error opening TIFF file %s", argv[argn], 0,0,0,0 );
	++argn;
	}
    else
	{
	tif = TIFFFdOpen( 0, "Standard Input", "r" );
	if ( tif == NULL )
	    pm_error( "error opening standard input as TIFF file", 0,0,0,0,0 );
	}

    if ( argn != argc )
	pm_usage( usage );

    td = &tif->tif_dir;
    if ( headerdump )
	TIFFPrintDirectory( tif, stderr, 1, 0, 0 );
    bps = td->td_bitspersample;
    if ( bps > 8 )
	pm_error( "can't handle more than 8 bits per sample", 0,0,0,0,0 );

    switch ( td->td_samplesperpixel )
	{
	case 1:
	case 3:
	case 4:
	break;

	default:
	pm_error(
	    "can only handle 1-channel gray scale or 1- or 3-channel color",
	    0,0,0,0,0 );
	}

    cols = td->td_imagewidth;
    rows = td->td_imagelength;

    if ( headerdump )
	{
	pm_message(
	    "%dx%dx%d image", cols, rows, bps * td->td_samplesperpixel, 0,0 );
	pm_message(
	    "%d bits/sample, %d samples/pixel", bps, td->td_samplesperpixel,
	    0,0,0 );
	}

    numcolors = ( 1 << bps );
    if ( numcolors - 1 > PNM_MAXMAXVAL )
	pm_error(
	    "too many colors - try recompiling with a larger PNM_MAXMAXVAL",
	    0,0,0,0,0 );
    maxval = numcolors - 1;

    if ( maxval == 1 && td->td_samplesperpixel == 1 )
	{
	if ( headerdump )
	    pm_message("monochrome", 0,0,0,0,0 );
	grayscale = 1;
	}
    else
	{
	switch (td->td_photometric)
	    {
	    case PHOTOMETRIC_MINISBLACK:
	    if ( headerdump )
		pm_message( "%d graylevels (min=black)", numcolors, 0,0,0,0 );
	    grayscale = 1;
	    break;

	    case PHOTOMETRIC_MINISWHITE:
	    if ( headerdump )
		pm_message( "%d graylevels (min=white)", numcolors, 0,0,0,0 );
	    grayscale = 1;
	    break;

	    case PHOTOMETRIC_PALETTE:
	    if ( headerdump )
		pm_message( "colormapped", 0,0,0,0,0 );
	    maxval = PNM_MAXMAXVAL;
	    grayscale = 0;
#ifdef PPM
	    for ( i = 0; i < numcolors; ++i )
		{
		register xelval r, g, b;
		r = (long) td->td_redcolormap[i] * PNM_MAXMAXVAL / 65535L;
		g = (long) td->td_greencolormap[i] * PNM_MAXMAXVAL / 65535L;
		b = (long) td->td_bluecolormap[i] * PNM_MAXMAXVAL / 65535L;
		PPM_ASSIGN( colormap[i], r, g, b );
		}
#endif /*PPM*/
	    break;

	    case PHOTOMETRIC_RGB:
	    if ( headerdump )
		pm_message( "truecolor", 0,0,0,0,0 );
	    grayscale = 0;
	    break;

	    case PHOTOMETRIC_MASK:
	    pm_error( "don't know how to handle PHOTOMETRIC_MASK", 0,0,0,0,0 );

	    case PHOTOMETRIC_DEPTH:
	    pm_error( "don't know how to handle PHOTOMETRIC_DEPTH", 0,0,0,0,0 );

	    default:
	    pm_error( "unknown photometric: %d", td->td_photometric, 0,0,0,0 );
	    }
	}

    if ( grayscale )
	{
	if ( maxval == 1 )
	    {
	    format = PBM_TYPE;
	    pm_message( "writing PBM file", 0,0,0,0,0 );
	    }
	else
	    {
#ifdef PGM
	    format = PGM_TYPE;
	    pm_message( "writing PGM file", 0,0,0,0,0 );
#else /*PGM*/
	    pm_error(
       "can't read grayscale TIFF file without PGM defined - try reconfiguring",
		0,0,0,0,0 );
#endif /*PGM*/
	    }
	}
    else
	{
#ifdef PPM
	format = PPM_TYPE;
	pm_message( "writing PPM file", 0,0,0,0,0 );
#else /*PPM*/
	pm_error(
	   "can't read color TIFF file without PPM defined - try reconfiguring",
	    0,0,0,0,0 );
#endif /*PPM*/
	}

    buf = (u_char*) malloc(TIFFScanlineSize(tif));
    if ( buf == NULL )
	pm_error( "can't allocate memory for scanline buffer", 0,0,0,0,0 );
    pnm_writepnminit( stdout, cols, rows, maxval, format, 0 );
    xelrow = pnm_allocrow( cols );

#define NEXTSAMPLE \
    { \
    if ( bitsleft == 0 ) \
	{ \
	++inP; \
	bitsleft = 8; \
	} \
    bitsleft -= bps; \
    sample = ( *inP >> bitsleft ) & maxval; \
    }

    for ( row = 0; row < rows; ++row )
	{
	if ( TIFFReadScanline( tif, buf, row, 0 ) < 0 )
	    pm_error( "bad data read on line %d", row, 0,0,0,0 );
	inP = buf;
	bitsleft = 8;
	xP = xelrow;

	switch ( td->td_photometric )
	    {
	    case PHOTOMETRIC_MINISBLACK:
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		NEXTSAMPLE
		PNM_ASSIGN1( *xP, sample );
		}
	    break;

	    case PHOTOMETRIC_MINISWHITE:
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		NEXTSAMPLE
		sample = maxval - sample;
		PNM_ASSIGN1( *xP, sample );
		}
	    break;

	    case PHOTOMETRIC_PALETTE:
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		NEXTSAMPLE
		*xP = colormap[sample];
		}
	    break;

	    case PHOTOMETRIC_RGB:
#ifdef PPM
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		register xelval r, g, b;

		NEXTSAMPLE
		r = sample;
		NEXTSAMPLE
		g = sample;
		NEXTSAMPLE
		b = sample;
		if ( td->td_samplesperpixel == 4 )
		    NEXTSAMPLE		/* skip alpha channel */
		PPM_ASSIGN( *xP, r, g, b );
		}
	    break;
#else /*PPM*/
	    pm_error(
	   "can't read color TIFF file without PPM defined - try reconfiguring",
		0,0,0,0,0 );
#endif /*PPM*/

	    default:
	    pm_error( "unknown photometric: %d", td->td_photometric, 0,0,0,0 );
	    }
	pnm_writepnmrow( stdout, xelrow, cols, maxval, format, 0 );
	}

    exit( 0 );
    }
