/* libpnm3.c - pnm utility library part 3
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

#ifdef PPM
#include "ppm.h"
#include "libppm.h"
#endif /*PPM*/

#ifdef PGM
#include "pgm.h"
#include "libpgm.h"
#endif /*PGM*/

#include "pbm.h"
#include "libpbm.h"

#ifdef __STDC__
xel
pnm_backgroundxel( xel** xels, int cols, int rows, xelval maxval, int format )
#else /*__STDC__*/
xel
pnm_backgroundxel( xels, cols, rows, maxval, format )
    xel** xels;
    int cols, rows, format;
    xelval maxval;
#endif /*__STDC__*/
    {
    xel bgxel, ul, ur, ll, lr;

    /* Guess a good background value. */
    ul = xels[0][0];
    ur = xels[0][cols-1];
    ll = xels[rows-1][0];
    lr = xels[rows-1][cols-1];

    /* First check for three corners equal. */
    if ( PNM_EQUAL( ul, ur ) && PNM_EQUAL( ur, ll ) )
	bgxel = ul;
    else if ( PNM_EQUAL( ul, ur ) && PNM_EQUAL( ur, lr ) )
	bgxel = ul;
    else if ( PNM_EQUAL( ul, ll ) && PNM_EQUAL( ll, lr ) )
	bgxel = ul;
    else if ( PNM_EQUAL( ur, ll ) && PNM_EQUAL( ll, lr ) )
	bgxel = ur;
    /* Nope, check for two corners equal. */
    else if ( PNM_EQUAL( ul, ur ) || PNM_EQUAL( ul, ll ) ||
	      PNM_EQUAL( ul, lr ) )
	bgxel = ul;
    else if ( PNM_EQUAL( ur, ll ) || PNM_EQUAL( ur, lr ) )
	bgxel = ur;
    else if ( PNM_EQUAL( ll, lr ) )
	bgxel = ll;
    else
	{
	/* Nope, we have to average the four corners.  This breaks the
	** rules of pnm, but oh well.  Let's try to do it portably. */
	switch ( PNM_FORMAT_TYPE(format) )
	    {
#ifdef PPM
	    case PPM_TYPE:
	    PPM_ASSIGN( bgxel,
		PPM_GETR(ul) + PPM_GETR(ur) + PPM_GETR(ll) + PPM_GETR(lr) / 4,
		PPM_GETG(ul) + PPM_GETG(ur) + PPM_GETG(ll) + PPM_GETG(lr) / 4,
		PPM_GETB(ul) + PPM_GETB(ur) + PPM_GETB(ll) + PPM_GETB(lr) / 4 );
	    break;
#endif /*PPM*/

#ifdef PGM
	    case PGM_TYPE:
	    {
	    gray gul, gur, gll, glr;
	    gul = (gray) PNM_GET1( ul );
	    gur = (gray) PNM_GET1( ur );
	    gll = (gray) PNM_GET1( ll );
	    glr = (gray) PNM_GET1( lr );
	    PNM_ASSIGN1( bgxel, ( ( gul + gur + gll + glr ) / 4 ) );
	    break;
	    }
#endif /*PGM*/

	    case PBM_TYPE:
	    pm_error(
	      "pnm_backgroundxel: four bits no two of which equal each other??",
		0,0,0,0,0 );

	    default:
	    pm_error( "can't happen", 0,0,0,0,0 );
	    }
	}

    return bgxel;
    }

#ifdef __STDC__
xel
pnm_backgroundxelrow( xel* xelrow, int cols, xelval maxval, int format )
#else /*__STDC__*/
xel
pnm_backgroundxelrow( xelrow, cols, maxval, format )
    xel* xelrow;
    int cols, format;
    xelval maxval;
#endif /*__STDC__*/
    {
    xel bgxel, l, r;

    /* Guess a good background value. */
    l = xelrow[0];
    r = xelrow[cols-1];

    /* First check for both corners equal. */
    if ( PNM_EQUAL( l, r ) )
	bgxel = l;
    else
	{
	/* Nope, we have to average the two corners.  This breaks the
	** rules of pnm, but oh well.  Let's try to do it portably. */
	switch ( PNM_FORMAT_TYPE(format) )
	    {
#ifdef PPM
	    case PPM_TYPE:
	    PPM_ASSIGN( bgxel, PPM_GETR(l) + PPM_GETR(r) / 2,
		PPM_GETG(l) + PPM_GETG(r) / 2, PPM_GETB(l) + PPM_GETB(r) / 2 );
	    break;
#endif /*PPM*/

#ifdef PGM
	    case PGM_TYPE:
	    {
	    gray gl, gr;
	    gl = (gray) PNM_GET1( l );
	    gr = (gray) PNM_GET1( r );
	    PNM_ASSIGN1( bgxel, ( ( gl + gr ) / 2 ) );
	    break;
	    }
#endif /*PGM*/

	    case PBM_TYPE:
	    {
	    int col, blacks;

	    /* One black, one white.  Gotta count. */
	    for ( col = 0, blacks = 0; col < cols; ++col )
		{
		if ( PNM_GET1( xelrow[col] ) == 0 )
		    ++blacks;
		}
	    if ( blacks >= cols / 2 )
		PNM_ASSIGN1( bgxel, 0 );
	    else
		PNM_ASSIGN1( bgxel, pnm_pbmmaxval );
	    break;
	    }

	    default:
	    pm_error( "can't happen", 0,0,0,0,0 );
	    }
	}

    return bgxel;
    }

#ifdef __STDC__
xel
pnm_whitexel( xelval maxval, int format )
#else /*__STDC__*/
xel
pnm_whitexel( maxval, format )
    xelval maxval;
    int format;
#endif /*__STDC__*/
    {
    xel x;

    switch ( PNM_FORMAT_TYPE(format) )
	{
#ifdef PPM
	case PPM_TYPE:
	PPM_ASSIGN( x, maxval, maxval, maxval );
	break;
#endif /*PPM*/

#ifdef PGM
	case PGM_TYPE:
	PNM_ASSIGN1( x, maxval );
	break;
#endif /*PGM*/

	case PBM_TYPE:
	PNM_ASSIGN1( x, pnm_pbmmaxval );
	break;

	default:
	pm_error( "can't happen", 0,0,0,0,0 );
	}

    return x;
    }

#ifdef __STDC__
xel
pnm_blackxel( xelval maxval, int format )
#else /*__STDC__*/
xel
pnm_blackxel( maxval, format )
    xelval maxval;
    int format;
#endif /*__STDC__*/
    {
    xel x;

    switch ( PNM_FORMAT_TYPE(format) )
	{
#ifdef PPM
	case PPM_TYPE:
	PPM_ASSIGN( x, 0, 0, 0 );
	break;
#endif /*PPM*/

#ifdef PGM
	case PGM_TYPE:
	PNM_ASSIGN1( x, (xelval) 0 );
	break;
#endif /*PGM*/

	case PBM_TYPE:
	PNM_ASSIGN1( x, (xelval) 0 );
	break;

	default:
	pm_error( "can't happen", 0,0,0,0,0 );
	}

    return x;
    }

#ifdef __STDC__
void
pnm_invertxel( xel* xP, xelval maxval, int format )
#else /*__STDC__*/
void
pnm_invertxel( xP, maxval, format )
    xel* xP;
    xelval maxval;
    int format;
#endif /*__STDC__*/
    {
    switch ( PNM_FORMAT_TYPE(format) )
	{
#ifdef PPM
	case PPM_TYPE:
	PPM_ASSIGN(
	    *xP, maxval - PPM_GETR( *xP ),
	    maxval - PPM_GETG( *xP ), maxval - PPM_GETB( *xP ) );
	break;
#endif /*PPM*/

#ifdef PGM
	case PGM_TYPE:
	PNM_ASSIGN1( *xP, (gray) maxval - (gray) PNM_GET1( *xP ) );
	break;
#endif /*PGM*/

	case PBM_TYPE:
	PNM_ASSIGN1( *xP, ( PNM_GET1( *xP ) == 0 ) ? pnm_pbmmaxval : 0 );
	break;

	default:
	pm_error( "can't happen", 0,0,0,0,0 );
	}
    }

#ifdef __STDC__
void
pnm_promoteformat( xel** xels, int cols, int rows, xelval maxval, int format, xelval newmaxval, int newformat )
#else /*__STDC__*/
void
pnm_promoteformat( xels, cols, rows, maxval, format, newmaxval, newformat )
    xel** xels;
    xelval maxval, newmaxval;
    int cols, rows, format, newformat;
#endif /*__STDC__*/
    {
    int row;

    for ( row = 0; row < rows; ++row )
	pnm_promoteformatrow(
	    xels[row], cols, maxval, format, newmaxval, newformat );
    }

#ifdef __STDC__
void
pnm_promoteformatrow( xel* xelrow, int cols, xelval maxval, int format, xelval newmaxval, int newformat )
#else /*__STDC__*/
void
pnm_promoteformatrow( xelrow, cols, maxval, format, newmaxval, newformat )
    xel* xelrow;
    xelval maxval, newmaxval;
    int cols, format, newformat;
#endif /*__STDC__*/
    {
    register int col;
    register xel* xP;

#ifdef PPM
    if ( ( PNM_FORMAT_TYPE(format) == PPM_TYPE &&
	   ( PNM_FORMAT_TYPE(newformat) == PGM_TYPE ||
	     PNM_FORMAT_TYPE(newformat) == PBM_TYPE ) ) ||
	 ( PNM_FORMAT_TYPE(format) == PGM_TYPE &&
	   PNM_FORMAT_TYPE(newformat) == PBM_TYPE ) )
	pm_error( "pnm_promoteformatrow: can't promote downwards!", 0,0,0,0,0 );
#else /*PPM*/
# ifdef PGM
    if ( PNM_FORMAT_TYPE(format) == PGM_TYPE &&
	 PNM_FORMAT_TYPE(newformat) == PBM_TYPE )
	pm_error( "pnm_promoteformatrow: can't promote downwards!", 0,0,0,0,0 );
# else /*PGM*/
    /* If only PBM is defined, we can just return. */
    return;
# endif /*PGM*/
#endif /*PPM*/

    /* Are we promoting to the same type? */
    if ( PNM_FORMAT_TYPE(format) == PNM_FORMAT_TYPE(newformat) )
	{
	if ( PNM_FORMAT_TYPE(format) == PBM_TYPE )
	    return;
	if ( newmaxval < maxval )
	    pm_error(
		"pnm_promoteformatrow: can't decrease maxval - try using pnmdepth",
		0,0,0,0,0 );
	if ( newmaxval == maxval )
	    return;
	/* Increase maxval. */
	switch ( PNM_FORMAT_TYPE(format) )
	    {
#ifdef PGM
	    case PGM_TYPE:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		PNM_ASSIGN1(
		    *xP, (int) PNM_GET1(*xP) * newmaxval / maxval );
	    break;
#endif /*PGM*/

#ifdef PPM
	    case PPM_TYPE:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		PPM_DEPTH( *xP, *xP, maxval, newmaxval );
	    break;
#endif /*PPM*/

	    default:
	    pm_error( "shouldn't happen", 0,0,0,0,0 );
	    }
	return;
	}

    /* We must be promoting to a higher type. */
    switch ( PNM_FORMAT_TYPE(format) )
	{
	case PBM_TYPE:
	switch ( PNM_FORMAT_TYPE(newformat) )
	    {
#ifdef PGM
	    case PGM_TYPE:
	    /* In the PNM world, PBM files are stored compatibly with PGM. */
	    return;
#endif /*PGM*/

#ifdef PPM
	    case PPM_TYPE:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		if ( PNM_GET1(*xP) == 0 )
		    PPM_ASSIGN( *xP, 0, 0, 0 );
		else
		    PPM_ASSIGN( *xP, newmaxval, newmaxval, newmaxval );
	    break;
#endif /*PPM*/

	    default:
	    pm_error( "can't happen", 0,0,0,0,0 );
	    }
	break;

#ifdef PGM
	case PGM_TYPE:
	switch ( PNM_FORMAT_TYPE(newformat) )
	    {
#ifdef PPM
	    case PPM_TYPE:
	    if ( newmaxval < maxval )
		pm_error(
		    "pnm_promoteformatrow: can't decrease maxval - try using pnmdepth",
		    0,0,0,0,0 );
	    if ( newmaxval == maxval )
		{
		for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		    PPM_ASSIGN(
			*xP, PNM_GET1(*xP), PNM_GET1(*xP), PNM_GET1(*xP) );
		}
	    else
		{ /* Increase maxval. */
		for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		    PPM_ASSIGN(
			*xP, (int) PNM_GET1(*xP) * newmaxval / maxval,
			(int) PNM_GET1(*xP) * newmaxval / maxval,
			(int) PNM_GET1(*xP) * newmaxval / maxval );
		}
	    break;
#endif /*PPM*/

	    default:
	    pm_error( "can't happen", 0,0,0,0,0 );
	    }
	break;
#endif /*PGM*/

	default:
	pm_error( "can't happen", 0,0,0,0,0 );
	}
    }
