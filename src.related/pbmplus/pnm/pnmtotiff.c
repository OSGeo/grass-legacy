/*
** pnmtotiff.c - converts a portable anymap to a Tagged Image File
**
** Derived by Jef Poskanzer from ras2tif.c, which is:
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

#ifdef PPM
#include "ppmcmap.h"
#define MAXCOLORS 256
#endif /*PPM*/

void main( argc, argv )
    int argc;
    char* argv[];
    {
    char* inf = NULL;
    FILE* ifp;
    xel** xels;
    register xel* xP;
#ifdef PPM
    colorhist_vector chv;
    colorhash_table cht;
    unsigned short red[MAXCOLORS], grn[MAXCOLORS], blu[MAXCOLORS];
#endif /*PPM*/
    int cols, rows, format, row, colors, i;
    register int col;
    xelval maxval;
    int grayscale;
    TIFF* tif;
    long rowsperstrip;
    short photometric;
    short samplesperpixel;
    short bitspersample;
    int bytesperrow;
    unsigned char* buf;
    unsigned char* tP;
    char* usage = "[pnmfile]";

    pnm_init( &argc, argv );

    if ( argc > 2 )
	pm_usage( usage );

    if ( argc == 2 )
	{
	inf = argv[1];
	ifp = pm_openr( inf );
	}
    else
	{
	inf = "Standard Input";
	ifp = stdin;
	}

    xels = pnm_readpnm( ifp, &cols, &rows, &maxval, &format );
    pm_close( ifp );

    /* Check for grayscale. */
    switch ( PNM_FORMAT_TYPE(format) )
	{
#ifdef PPM
	case PPM_TYPE:
	pm_message( "computing colormap...", 0,0,0,0,0 );
	chv = ppm_computecolorhist( xels, cols, rows, MAXCOLORS, &colors );
	if ( chv == (colorhist_vector) 0 )
	    {
	    pm_message(
		"Too many colors - proceeding to write a 24-bit RGB file.",
		0,0,0,0,0 );
	    pm_message(
		"If you want an 8-bit palette file, try doing a 'ppmquant %d'.",
		MAXCOLORS, 0,0,0,0 );
	    }
	else
	    {
	    pm_message( "%d colors found", colors, 0,0,0,0 );
	    grayscale = 1;
	    for ( i = 0; i < colors; ++i )
		{
		register xelval r, g, b;

		r = PPM_GETR( chv[i].color );
		g = PPM_GETG( chv[i].color );
		b = PPM_GETB( chv[i].color );
		if ( r != g || g != b )
		    {
		    grayscale = 0;
		    break;
		    }
		}
	    }
	break;
#endif /*PPM*/

	default:
	grayscale = 1;
	break;
	}

    /* Open output file. */
    tif = TIFFFdOpen( 1, "Standard Output", "w" );
    if ( tif == NULL )
	pm_error( "error opening standard output as TIFF file", 0,0,0,0,0 );

    /* Figure out TIFF parameters. */
    switch ( PNM_FORMAT_TYPE(format) )
	{
#ifdef PPM
	case PPM_TYPE:
	if ( chv == (colorhist_vector) 0 )
	    {
	    samplesperpixel = 3;
	    bitspersample = 8;
	    photometric = PHOTOMETRIC_RGB;
	    bytesperrow = cols * 3;
	    }
	else if ( grayscale )
	    {
	    samplesperpixel = 1;
	    bitspersample = pm_maxvaltobits( maxval );
	    photometric = PHOTOMETRIC_MINISBLACK;
	    bytesperrow = ( cols + i - 1 ) / i;
	    }
	else
	    {
	    samplesperpixel = 1;
	    bitspersample = 8;
	    photometric = PHOTOMETRIC_PALETTE;
	    bytesperrow = cols;
	    }
	break;
#endif /*PPM*/
#ifdef PGM
	case PGM_TYPE:
	samplesperpixel = 1;
	bitspersample = pm_maxvaltobits( maxval );
	photometric = PHOTOMETRIC_MINISBLACK;
	i = 8 / bitspersample;
	bytesperrow = ( cols + i - 1 ) / i;
	break;
#endif /*PGM*/
	default:
	samplesperpixel = 1;
	bitspersample = 1;
	photometric = PHOTOMETRIC_MINISBLACK;
	bytesperrow = ( cols + 7 ) / 8;
	break;
	}
    rowsperstrip = (8 * 1024) / bytesperrow;
    buf = (unsigned char*) malloc( bytesperrow );
    if ( buf == (unsigned char*) 0 )
	pm_error( "can't allocate memory for row buffer", 0,0,0,0,0 );

    /* Set TIFF parameters. */
    TIFFSetField( tif, TIFFTAG_IMAGEWIDTH, cols );
    TIFFSetField( tif, TIFFTAG_IMAGELENGTH, rows );
    TIFFSetField( tif, TIFFTAG_BITSPERSAMPLE, bitspersample );
    TIFFSetField( tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT );
    TIFFSetField( tif, TIFFTAG_COMPRESSION, COMPRESSION_LZW );
    TIFFSetField( tif, TIFFTAG_PHOTOMETRIC, photometric );
    TIFFSetField( tif, TIFFTAG_DOCUMENTNAME, inf );
    TIFFSetField( tif, TIFFTAG_IMAGEDESCRIPTION, "converted PNM file" );
    TIFFSetField( tif, TIFFTAG_SAMPLESPERPIXEL, samplesperpixel );
    TIFFSetField( tif, TIFFTAG_ROWSPERSTRIP, rowsperstrip );
    TIFFSetField( tif, TIFFTAG_STRIPBYTECOUNTS, rows / rowsperstrip );
    TIFFSetField( tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG );

#ifdef PPM
    if ( chv == (colorhist_vector) 0 )
	cht = (colorhash_table) 0;
    else
	{
	/* Make TIFF colormap. */
	for ( i = 0; i < colors; ++i )
	    {
	    red[i] = (long) PPM_GETR( chv[i].color ) * 65535L / maxval;
	    grn[i] = (long) PPM_GETG( chv[i].color ) * 65535L / maxval;
	    blu[i] = (long) PPM_GETB( chv[i].color ) * 65535L / maxval;
	    }
	TIFFSetField( tif, TIFFTAG_COLORMAP, red, grn, blu );

	/* Convert color vector to color hash table, for fast lookup. */
	cht = ppm_colorhisttocolorhash( chv, colors );
	ppm_freecolorhist( chv );
	}
#endif /*PPM*/

    /* Now write the TIFF data. */
    for ( row = 0; row < rows; ++row )
	{
#ifdef PPM
	if ( PNM_FORMAT_TYPE(format) == PPM_TYPE && ! grayscale )
	    {
	    if ( cht == (colorhash_table) 0 )
		{
		for ( col = 0, xP = xels[row], tP = buf;
		      col < cols; ++col, ++xP )
		    {
		    register unsigned char s;

		    s = PPM_GETR( *xP );
		    if ( maxval != 255 )
			s = (long) s * 255 / maxval;
		    *tP++ = s;
		    s = PPM_GETG( *xP );
		    if ( maxval != 255 )
			s = (long) s * 255 / maxval;
		    *tP++ = s;
		    s = PPM_GETB( *xP );
		    if ( maxval != 255 )
			s = (long) s * 255 / maxval;
		    *tP++ = s;
		    }
		}
	    else
		{
		for ( col = 0, xP = xels[row], tP = buf;
		      col < cols; ++col, ++xP )
		    {
		    register int s;

		    s = ppm_lookupcolor( cht, xP );
		    if ( s == -1 )
			pm_error(
			    "color not found?!?  row=%d col=%d  r=%d g=%d b=%d",
			    row, col, PPM_GETR( *xP ), PPM_GETG( *xP ),
			    PPM_GETB( *xP ) );
		    *tP++ = (unsigned char) s;
		    }
		}
	    }
	else
#endif /*PPM*/
	    {
	    register xelval bigger_maxval;
	    register int bitshift;
	    register unsigned char byte;
	    register xelval s;

	    bigger_maxval = pm_bitstomaxval( bitspersample );
	    bitshift = 8 - bitspersample;
	    byte = 0;
	    for ( col = 0, xP = xels[row], tP = buf; col < cols; ++col, ++xP )
		{
		s = PNM_GET1( *xP );
		if ( maxval != bigger_maxval )
		    s = (long) s * bigger_maxval / maxval;
		byte |= s << bitshift;
		bitshift -= bitspersample;
		if ( bitshift < 0 )
		    {
		    *tP++ = byte;
		    bitshift = 8 - bitspersample;
		    byte = 0;
		    }
		}
	    if ( bitshift != 8 - bitspersample )
		*tP++ = byte;
	    }

	if ( TIFFWriteScanline( tif, buf, row, 0 ) < 0 )
	    pm_error( "failed a scanline write on row %d", row, 0,0,0,0 );
	}
    TIFFFlushData( tif );
    TIFFClose( tif );

    exit( 0 );
    }
