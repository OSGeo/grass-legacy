/* pnmgamma.c - perform gamma correction on a portable pixmap
**
** Copyright (C) 1991 by Bill Davidson and Jef Poskanzer.
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
#include <ctype.h>

void buildgamma();

void main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp;
    xel* xelrow;
    register xel* xP;
    xelval maxval;
    int argn, rows, cols, format, newformat, row;
    register int col;
    double ggamma;
    xelval* gtable;
#ifdef PPM
    double rgamma, bgamma;
    xelval* rtable;
    xelval* btable;
    char *usage = "<value> [pnmfile]\n\t\t<redvalue> <greenvalue> <bluevalue> [pnmfile]";
#else /*PPM*/
    char *usage = "<value> [pnmfile]";
#endif /*PPM*/

    pnm_init( &argc, argv );

    argn = 1;

    /* Parse gamma args. */
    if ( argc == 2 || argc == 3 )
	{
	ggamma = atof( argv[argn] );
#ifdef PPM
	rgamma = bgamma = ggamma;
#endif /*PPM*/
	++argn;
	}
#ifdef PPM
    else if ( argc == 4 || argc == 5 )
	{
	rgamma = atof( argv[argn] );
	++argn;
	ggamma = atof( argv[argn] );
	++argn;
	bgamma = atof( argv[argn] );
	++argn;
	}
#endif /*PPM*/
    else
	pm_usage( usage );

    if ( ggamma <= 0.0 )
	pm_usage( usage );
#ifdef PPM
    if ( rgamma <= 0.0 || bgamma <= 0.0 )
	pm_usage( usage );
#endif /*PPM*/

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
    pnm_readpnminit( ifp, &cols, &rows, &maxval, &format );
    xelrow = pnm_allocrow( cols );

#ifdef PGM
    /* Promote PBM files to PGM.  Not that it makes much sense to
    ** gamma-correct PBM files. */
    if ( PNM_FORMAT_TYPE(format) == PBM_TYPE )
	{
        newformat = PGM_TYPE;
	pm_message( "promoting to PGM", 0,0,0,0,0 );
	}
    else
        newformat = format;

#ifdef PPM
    if ( rgamma != ggamma || ggamma != bgamma )
	if ( PNM_FORMAT_TYPE(newformat) == PGM_TYPE )
	    {
	    newformat = PPM_TYPE;
	    pm_message( "promoting to PPM", 0,0,0,0,0 );
	    }
#endif /*PPM*/

#else /*PGM*/
    pm_error( "must have at least PGM defined - try reconfiguring", 0,0,0,0,0 );
#endif /*PGM*/

    /* Allocate space for the tables. */
    gtable = (xelval*) malloc( (maxval+1) * sizeof(xelval) );
    if ( gtable == 0 )
	pm_error( "out of memory", 0,0,0,0,0 );
#ifdef PPM
    rtable = (xelval*) malloc( (maxval+1) * sizeof(xelval) );
    btable = (xelval*) malloc( (maxval+1) * sizeof(xelval) );
    if ( rtable == 0 || btable == 0 )
	pm_error( "out of memory", 0,0,0,0,0 );
#endif /*PPM*/

    /* Build the gamma corection tables. */
    buildgamma( gtable, maxval, ggamma );
#ifdef PPM
    buildgamma( rtable, maxval, rgamma );
    buildgamma( btable, maxval, bgamma );
#endif /*PPM*/

    pnm_writepnminit( stdout, cols, rows, maxval, newformat, 0 );
    for ( row = 0; row < rows; ++row )
	{
	pnm_readpnmrow( ifp, xelrow, cols, maxval, format );

#ifdef PPM
	/* Promote to PPM if differing gammas were specified. */
	if ( rgamma != ggamma || ggamma != bgamma )
	    if ( PNM_FORMAT_TYPE(format) != PPM_TYPE &&
		 PNM_FORMAT_TYPE(newformat) == PPM_TYPE )
		pnm_promoteformatrow(
		    xelrow, cols, maxval, format, maxval, newformat );
#endif /*PPM*/

	switch ( PNM_FORMAT_TYPE(newformat) )
	    {
#ifdef PPM
	    case PPM_TYPE:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		{
		register xelval r, g, b;

		r = PPM_GETR( *xP );
		g = PPM_GETG( *xP );
		b = PPM_GETB( *xP );
		r = rtable[r];
		g = gtable[g];
		b = btable[b];
		PPM_ASSIGN( *xP, r, g, b );
		}
	    break;
#endif /*PPM*/
	    default:
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++xP )
		{
		register xelval g;

		g = PNM_GET1( *xP );
		g = gtable[g];
		PNM_ASSIGN1( *xP, g );
		}
	    break;
	    }

	pnm_writepnmrow( stdout, xelrow, cols, maxval, newformat, 0 );
	}

    pm_close( ifp );

    exit( 0 );
    }

/*
** Builds a gamma table of size maxval+1 for the given gamma value.
**
** This function depends on pow(3m).  If you don't have it, you can
** simulate it with '#define pow(x,y) exp((y)*log(x))' provided that
** you have the exponential function exp(3m) and the natural logarithm
** function log(3m).  I can't believe I actually remembered my log
** identities.
*/

void
buildgamma( table, maxval, gamma )
    xelval table[], maxval;
    double gamma;
    {
    register int i, v;
    double one_over_gamma, index, q;

    one_over_gamma = 1.0 / gamma;
    q = (double) maxval;
    for ( i = 0 ; i <= (int) maxval; ++i )
	{
	index = ( (double) i ) / q;
	v = ( q * pow( index, one_over_gamma ) ) + 0.5;
	if ( v > (int) maxval )
	    v = maxval;
	table[i] = v;
	}
    }
