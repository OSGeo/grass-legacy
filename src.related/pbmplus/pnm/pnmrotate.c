/* pnmrotate.c - read a portable anymap and rotate it by some angle
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
#include <math.h>
#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif /*M_PI*/

#define SCALE 4096
#define HALFSCALE 2048

void main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp;
    xel** xels;
    xel** temp1xels;
    xel** temp2xels;
    xel* newxelrow;
    register xel* xP;
    register xel* nxP;
    xel bgxel, prevxel, x;
    int argn, rows, cols, format, newformat, newrows;
    int tempcols, newcols, yshearjunk, x2shearjunk, row, col, new;
    xelval maxval;
    int antialias;
    float fangle, xshearfac, yshearfac, new0;
    int intnew0;
    register long fracnew0, omfracnew0;
    char* usage = "[-noantialias] <angle> [pnmfile]";

    pnm_init( &argc, argv );

    argn = 1;
    antialias = 1;

    if ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
        {
        if ( pm_keymatch( argv[argn], "-antialias", 2 ) )
            antialias = 1;
        else if ( pm_keymatch( argv[argn], "-noantialias", 2 ) )
            antialias = 0;
        else
            pm_usage( usage );
        ++argn;
        }

    if ( argn == argc )
	pm_usage( usage );
    if ( sscanf( argv[argn], "%f", &fangle ) != 1 )
	pm_usage( usage );
    ++argn;
    if ( fangle < -90.0 || fangle > 90.0 )
	pm_error( "angle must be between -90 and 90", 0,0,0,0,0 );
    fangle = fangle * M_PI / 180.0;	/* convert to radians */

    xshearfac = tan( fangle / 2.0 );
    if ( xshearfac < 0.0 )
	xshearfac = -xshearfac;
    yshearfac = sin( fangle );
    if ( yshearfac < 0.0 )
	yshearfac = -yshearfac;

    if ( argn != argc )
	{
	ifp = pm_openr( argv[argn] );
	++argn;
	}
    else
	ifp = stdin;

    if ( argn != argc )
	pm_usage( usage );

    pnm_pbmmaxval = PNM_MAXMAXVAL;  /* use larger value for better results */
    xels = pnm_readpnm( ifp, &cols, &rows, &maxval, &format );
    pm_close( ifp );

#ifdef PGM
    /* Promote PBM files to PGM. */
    if ( antialias && PNM_FORMAT_TYPE(format) == PBM_TYPE )
	{
        newformat = PGM_TYPE;
	pm_message( "promoting from PBM to PGM - use -noantialias to avoid this", 0,0,0,0,0 );
	}
    else
        newformat = format;
#else /*PGM*/
    newformat = format;
    antialias = 0;      /* no antialiasing possible without PGM */
#endif /*PGM*/

    tempcols = rows * xshearfac + cols + 0.999999;
    yshearjunk = ( tempcols - cols ) * yshearfac;
    newrows = tempcols * yshearfac + rows + 0.999999;
    x2shearjunk = ( newrows - rows - yshearjunk ) * xshearfac;
    newrows -= 2 * yshearjunk;
    newcols = newrows * xshearfac + tempcols + 0.999999 - 2 * x2shearjunk;

    bgxel = pnm_backgroundxel( xels, cols, rows, maxval, format );

    /* First shear X into temp1xels. */
    temp1xels = pnm_allocarray( tempcols, rows );
    for ( row = 0; row < rows; ++row )
	{
	if ( fangle > 0 )
	    new0 = row * xshearfac;
	else
	    new0 = ( rows - row ) * xshearfac;
	intnew0 = (int) new0;

	if ( antialias )
	    {
	    fracnew0 = ( new0 - intnew0 ) * SCALE;
	    omfracnew0 = SCALE - fracnew0;

	    for ( col = 0, nxP = temp1xels[row]; col < tempcols; ++col, ++nxP )
		*nxP = bgxel;

	    prevxel = bgxel;
	    for ( col = 0, nxP = &(temp1xels[row][intnew0]), xP = xels[row];
		  col < cols; ++col, ++nxP, ++xP )
		{
                switch ( PNM_FORMAT_TYPE(format) )
                    {
#ifdef PPM
                    case PPM_TYPE:
		    PPM_ASSIGN( *nxP,
			( fracnew0 * PPM_GETR(prevxel) + omfracnew0 * PPM_GETR(*xP) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETG(prevxel) + omfracnew0 * PPM_GETG(*xP) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETB(prevxel) + omfracnew0 * PPM_GETB(*xP) + HALFSCALE ) / SCALE );
                    break;
#endif /*PPM*/
                    default:
		    PNM_ASSIGN1( *nxP,
			( fracnew0 * PNM_GET1(prevxel) + omfracnew0 * PNM_GET1(*xP) + HALFSCALE ) / SCALE );
                    break;
                    }
		prevxel = *xP;
		}
	    if ( fracnew0 > 0 && intnew0 + cols < tempcols )
		{
                switch ( PNM_FORMAT_TYPE(format) )
                    {
#ifdef PPM
                    case PPM_TYPE:
		    PPM_ASSIGN( *nxP,
			( fracnew0 * PPM_GETR(prevxel) + omfracnew0 * PPM_GETR(bgxel) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETG(prevxel) + omfracnew0 * PPM_GETG(bgxel) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETB(prevxel) + omfracnew0 * PPM_GETB(bgxel) + HALFSCALE ) / SCALE );
                    break;
#endif /*PPM*/
                    default:
                    PNM_ASSIGN1( *nxP,
                        ( fracnew0 * PNM_GET1(prevxel) + omfracnew0 * PNM_GET1(bgxel) + HALFSCALE ) / SCALE );
                    break;
                    }
		}
	    }
	else
	    {
	    for ( col = 0, nxP = temp1xels[row]; col < intnew0; ++col, ++nxP )
		*nxP = bgxel;
	    for ( col = 0, xP = xels[row]; col < cols; ++col, ++nxP, ++xP )
		*nxP = *xP;
	    for ( col = intnew0 + cols; col < tempcols; ++col, ++nxP )
		*nxP = bgxel;
	    }
	}
    pnm_freearray( xels, rows );

    /* Now inverse shear Y from temp1 into temp2. */
    temp2xels = pnm_allocarray( tempcols, newrows );
    for ( col = 0; col < tempcols; ++col )
	{
	if ( fangle > 0 )
	    new0 = ( tempcols - col ) * yshearfac;
	else
	    new0 = col * yshearfac;
	intnew0 = (int) new0;
	fracnew0 = ( new0 - intnew0 ) * SCALE;
	omfracnew0 = SCALE - fracnew0;
	intnew0 -= yshearjunk;

	for ( row = 0; row < newrows; ++row )
	    temp2xels[row][col] = bgxel;

	if ( antialias )
	    {
	    prevxel = bgxel;
	    for ( row = 0; row < rows; ++row )
		{
		new = row + intnew0;
		if ( new >= 0 && new < newrows )
		    {
		    nxP = &(temp2xels[new][col]);
		    x = temp1xels[row][col];
		    switch ( PNM_FORMAT_TYPE(format) )
			{
#ifdef PPM
			case PPM_TYPE:
			PPM_ASSIGN( *nxP,
			    ( fracnew0 * PPM_GETR(prevxel) + omfracnew0 * PPM_GETR(x) + HALFSCALE ) / SCALE,
			    ( fracnew0 * PPM_GETG(prevxel) + omfracnew0 * PPM_GETG(x) + HALFSCALE ) / SCALE,
			    ( fracnew0 * PPM_GETB(prevxel) + omfracnew0 * PPM_GETB(x) + HALFSCALE ) / SCALE );
			break;
#endif /*PPM*/
			default:
			PNM_ASSIGN1( *nxP,
			    ( fracnew0 * PNM_GET1(prevxel) + omfracnew0 * PNM_GET1(x) + HALFSCALE ) / SCALE );
			break;
			}
		    prevxel = x;
		    }
		}
	    if ( fracnew0 > 0 && intnew0 + rows < newrows )
		{
		nxP = &(temp2xels[intnew0 + rows][col]);
                switch ( PNM_FORMAT_TYPE(format) )
                    {
#ifdef PPM
                    case PPM_TYPE:
		    PPM_ASSIGN( *nxP,
			( fracnew0 * PPM_GETR(prevxel) + omfracnew0 * PPM_GETR(bgxel) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETG(prevxel) + omfracnew0 * PPM_GETG(bgxel) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETB(prevxel) + omfracnew0 * PPM_GETB(bgxel) + HALFSCALE ) / SCALE );
                    break;
#endif /*PPM*/
                    default:
		    PNM_ASSIGN1( *nxP,
			( fracnew0 * PNM_GET1(prevxel) + omfracnew0 * PNM_GET1(bgxel) + HALFSCALE ) / SCALE );
                    break;
                    }
		}
	    }
	else
	    {
	    for ( row = 0; row < rows; ++row )
		{
		new = row + intnew0;
		if ( new >= 0 && new < newrows )
		    temp2xels[new][col] = temp1xels[row][col];
		}
	    }
	}
    pnm_freearray( temp1xels, rows );

    /* Finally, shear X from temp2 into newxelrow. */
    pnm_writepnminit( stdout, newcols, newrows, maxval, newformat, 0 );
    newxelrow = pnm_allocrow( newcols );
    for ( row = 0; row < newrows; ++row )
	{
	if ( fangle > 0 )
	    new0 = row * xshearfac;
	else
	    new0 = ( newrows - row ) * xshearfac;
	intnew0 = (int) new0;
	fracnew0 = ( new0 - intnew0 ) * SCALE;
	omfracnew0 = SCALE - fracnew0;
	intnew0 -= x2shearjunk;

	for ( col = 0, nxP = newxelrow; col < newcols; ++col, ++nxP )
	    *nxP = bgxel;

	if ( antialias )
	    {
	    prevxel = bgxel;
	    for ( col = 0, xP = temp2xels[row]; col < tempcols; ++col, ++xP )
		{
		new = intnew0 + col;
		if ( new >= 0 && new < newcols )
		    {
		    nxP = &(newxelrow[new]);
		    switch ( PNM_FORMAT_TYPE(format) )
			{
#ifdef PPM
			case PPM_TYPE:
			PPM_ASSIGN( *nxP,
			    ( fracnew0 * PPM_GETR(prevxel) + omfracnew0 * PPM_GETR(*xP) + HALFSCALE ) / SCALE,
			    ( fracnew0 * PPM_GETG(prevxel) + omfracnew0 * PPM_GETG(*xP) + HALFSCALE ) / SCALE,
			    ( fracnew0 * PPM_GETB(prevxel) + omfracnew0 * PPM_GETB(*xP) + HALFSCALE ) / SCALE );
			break;
#endif /*PPM*/
			default:
			PNM_ASSIGN1( *nxP,
			    ( fracnew0 * PNM_GET1(prevxel) + omfracnew0 * PNM_GET1(*xP) + HALFSCALE ) / SCALE );
			break;
			}
		    prevxel = *xP;
		    }
		}
	    if ( fracnew0 > 0 && intnew0 + tempcols < newcols )
		{
		nxP = &(newxelrow[intnew0 + tempcols]);
                switch ( PNM_FORMAT_TYPE(format) )
                    {
#ifdef PPM
                    case PPM_TYPE:
		    PPM_ASSIGN( *nxP,
			( fracnew0 * PPM_GETR(prevxel) + omfracnew0 * PPM_GETR(bgxel) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETG(prevxel) + omfracnew0 * PPM_GETG(bgxel) + HALFSCALE ) / SCALE,
			( fracnew0 * PPM_GETB(prevxel) + omfracnew0 * PPM_GETB(bgxel) + HALFSCALE ) / SCALE );
                    break;
#endif /*PPM*/
                    default:
		    PNM_ASSIGN1( *nxP,
			( fracnew0 * PNM_GET1(prevxel) + omfracnew0 * PNM_GET1(bgxel) + HALFSCALE ) / SCALE );
                    break;
                    }
		}
	    }
	else
	    {
	    for ( col = 0, xP = temp2xels[row]; col < tempcols; ++col, ++xP )
		{
		new = intnew0 + col;
		if ( new >= 0 && new < newcols )
		    newxelrow[new] = *xP;
		}
	    }

	pnm_writepnmrow( stdout, newxelrow, newcols, maxval, newformat, 0 );
	}

    exit( 0 );
    }
