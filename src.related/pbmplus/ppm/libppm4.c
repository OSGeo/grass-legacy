/* libppm4.c - ppm utility library part 4
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

static void
canonstr( str )
    char* str;
    {
    while ( *str != '\0' )
	{
	if ( *str == ' ' )
	    {
	    (void) strcpy( str, &(str[1]) );
	    continue;
	    }
	if ( isupper( *str ) )
	    *str = tolower( *str );
	++str;
	}
    }

#ifdef __STDC__
pixel
ppm_parsecolor( char* colorname, pixval maxval )
#else /*__STDC__*/
pixel
ppm_parsecolor( colorname, maxval )
    char* colorname;
    pixval maxval;
#endif /*__STDC__*/
    {
    pixel p;
    int imaxval, r, g, b;

    imaxval = maxval;
    if ( colorname[0] == '#' )
	{ /* It's an X-style hexidecimal rgb specifier. */
	int hexit[256], i;

	for ( i = 0; i < 256; ++i )
	    hexit[i] = 1234567890;
	hexit['0'] = 0;
	hexit['1'] = 1;
	hexit['2'] = 2;
	hexit['3'] = 3;
	hexit['4'] = 4;
	hexit['5'] = 5;
	hexit['6'] = 6;
	hexit['7'] = 7;
	hexit['8'] = 8;
	hexit['9'] = 9;
	hexit['a'] = hexit['A'] = 10;
	hexit['b'] = hexit['B'] = 11;
	hexit['c'] = hexit['C'] = 12;
	hexit['d'] = hexit['D'] = 13;
	hexit['e'] = hexit['E'] = 14;
	hexit['f'] = hexit['F'] = 15;

	switch ( strlen( colorname ) )
	    {
	    case 4:
	    r = hexit[colorname[1]];
	    g = hexit[colorname[2]];
	    b = hexit[colorname[3]];
	    if ( imaxval != 15 )
		{
		r = r * imaxval / 15;
		g = g * imaxval / 15;
		b = b * imaxval / 15;
		}
	    break;

	    case 7:
	    r = ( hexit[colorname[1]] << 4 ) + hexit[colorname[2]];
	    g = ( hexit[colorname[3]] << 4 ) + hexit[colorname[4]];
	    b = ( hexit[colorname[5]] << 4 ) + hexit[colorname[6]];
	    if ( imaxval != 255 )
		{
		r = r * imaxval / 255;
		g = g * imaxval / 255;
		b = b * imaxval / 255;
		}
	    break;

	    case 10:
	    r = ( hexit[colorname[1]] << 8 ) + ( hexit[colorname[2]] << 4 ) +
		hexit[colorname[3]];
	    g = ( hexit[colorname[4]] << 8 ) + ( hexit[colorname[5]] << 4 ) +
		hexit[colorname[6]];
	    b = ( hexit[colorname[7]] << 8 ) + ( hexit[colorname[8]] << 4 ) +
		hexit[colorname[9]];
	    if ( imaxval != 4095 )
		{
		r = r * imaxval / 4095;
		g = g * imaxval / 4095;
		b = b * imaxval / 4095;
		}
	    break;

	    case 13:
	    r = ( hexit[colorname[1]] << 12 ) + ( hexit[colorname[2]] << 8 ) +
		( hexit[colorname[3]] << 4 ) + hexit[colorname[4]];
	    g = ( hexit[colorname[5]] << 12 ) + ( hexit[colorname[6]] << 8 ) +
		( hexit[colorname[7]] << 4 ) + hexit[colorname[8]];
	    b = ( hexit[colorname[9]] << 12 ) + ( hexit[colorname[10]] << 8 ) +
		( hexit[colorname[11]] << 4 ) + hexit[colorname[12]];
	    if ( imaxval != 65535 )
		{
		r = r * imaxval / 65535;
		g = g * imaxval / 65535;
		b = b * imaxval / 65535;
		}
	    break;

	    default:
	    pm_error( "invalid hex color specifier - \"%s\"", colorname, 0,0,0,0 );
	    }
	if ( r < 0 || r > imaxval || g < 0 || g > imaxval || b < 0 || b > imaxval )
	    pm_error( "invalid hex color specifier - \"%s\"", colorname, 0,0,0,0 );
	}
    else if ( ( colorname[0] >= '0' && colorname[0] <= '9' ) ||
	      colorname[0] == '.' )
	{ /* It's a decimal/float rgb specifier. */
	float fr, fg, fb;

	if ( sscanf( colorname, "%g,%g,%g", &fr, &fg, &fb ) != 3 )
	    pm_error( "invalid color specifier - \"%s\"", colorname, 0,0,0,0 );
	if ( fr < 0.0 || fr > 1.0 || fg < 0.0 || fg > 1.0 || fb < 0.0 || fb > 1.0 )
	    pm_error( "invalid color specifier - \"%s\" - values must be between 0.0 and 1.0", colorname, 0,0,0,0 );
	r = fr * imaxval;
	g = fg * imaxval;
	b = fb * imaxval;
	}
    else
	{ /* Must be a name from the X-style rgb file. */
#ifndef RGB_DB
	pm_error( "color names database unavailable", 0,0,0,0,0 );
#else /*RGB_DB*/
	FILE* f;
	char buf1[200], buf2[200];

	if ( ( f = fopen( RGB_DB, "r" ) ) == NULL )
	    pm_error( "can't open color names database", 0,0,0,0,0 );
	canonstr( colorname );
	while ( fgets( buf1, sizeof(buf1), f ) != NULL )
	    {
	    if ( sscanf( buf1, "%d %d %d %s", &r, &g, &b, buf2 ) != 4 )
		{
		pm_message(
		    "can't parse color names database line - \"%s\"",
		    buf1, 0,0,0,0 );
		continue;
		}
	    canonstr( buf2 );
	    if ( strcmp( colorname, buf2 ) == 0 )
		goto gotit;
	    }
	(void) fclose( f );
	pm_error( "unknown color - \"%s\"", colorname, 0,0,0,0 );

gotit:
	(void) fclose( f );
	/* Rescale from [0..255] if necessary. */
	if ( imaxval != 255 )
	    {
	    r = r * imaxval / 255;
	    g = g * imaxval / 255;
	    b = b * imaxval / 255;
	    }
#endif /*RGB_DB*/
	}

    PPM_ASSIGN( p, r, g, b );
    return p;
    }

static char colorname[200];

#ifdef __STDC__
char*
ppm_colorname( pixel* colorP, pixval maxval, int hexok )
#else /*__STDC__*/
char*
ppm_colorname( colorP, maxval, hexok )
    pixel* colorP;
    pixval maxval;
    int hexok;
#endif /*__STDC__*/
    {
    int r, g, b;
#ifdef RGB_DB
    FILE* f;
    char buf[200];
    int this_r, this_g, this_b;
    int best_diff, this_diff;
    char this_colorname[200];
#endif /*RGB_DB*/

    if ( maxval == 255 )
	{
	r = PPM_GETR( *colorP );
	g = PPM_GETG( *colorP );
	b = PPM_GETB( *colorP );
	}
    else
	{
	r = (int) PPM_GETR( *colorP ) * 255 / (int) maxval;
	g = (int) PPM_GETG( *colorP ) * 255 / (int) maxval;
	b = (int) PPM_GETB( *colorP ) * 255 / (int) maxval;
	}

#ifdef RGB_DB
    if ( ( f = fopen( RGB_DB, "r" ) ) == NULL )
	pm_error( "can't open color names database", 0,0,0,0,0 );
    best_diff = 32767;
    while ( fgets( buf, sizeof(buf), f ) != NULL )
	{
	if ( sscanf( buf, "%d %d %d %s", &this_r, &this_g, &this_b,
		     this_colorname ) != 4 )
	    {
	    pm_message(
		"can't parse color names database line - \"%s\"",
		buf, 0,0,0,0 );
	    continue;
	    }
	this_diff = abs( r - this_r ) + abs( g - this_g ) + abs( b - this_b );
	if ( this_diff < best_diff )
	    {
	    best_diff = this_diff;
	    (void) strcpy( colorname, this_colorname );
	    }
	}
    (void) fclose( f );
    if ( best_diff != 32767 && ( best_diff == 0 || ! hexok ) )
	return colorname;
#endif /*RGB_DB*/

    /* Color lookup failed; generate an X11-style hex specifier. */
    if ( ! hexok )
	pm_error(
     "color names database required - please recompile with the RGB_DB option",
	    0,0,0,0,0 );
    sprintf( colorname, "#%02x%02x%02x", r, g, b );
    return colorname;
    }
