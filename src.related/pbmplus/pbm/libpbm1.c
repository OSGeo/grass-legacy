/* libpbm1.c - pbm utility library part 1
**
** Copyright (C) 1988 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "pbm.h"
#include "libpbm.h"
#include <varargs.h>

/* Variable-sized arrays. */

char*
pm_allocrow( cols, size )
    int cols;
    {
    register char* itrow;

    itrow = (char*) malloc( cols * size );
    if ( itrow == (char*) 0 )
	pm_error( "out of memory allocating a row", 0,0,0,0,0 );
    return itrow;
    }

void
pm_freerow( itrow )
    char* itrow;
    {
    free( itrow );
    }


char**
pm_allocarray( cols, rows, size )
    int cols, rows;
    int size;
    {
    char** its;
    int i;

    its = (char**) malloc( rows * sizeof(char*) );
    if ( its == (char**) 0 )
	pm_error( "out of memory allocating an array", 0,0,0,0,0 );
    its[0] = (char*) malloc( rows * cols * size );
    if ( its[0] == (char*) 0 )
	pm_error( "out of memory allocating an array", 0,0,0,0,0 );
    for ( i = 1; i < rows; ++i )
	its[i] = &(its[0][i * cols * size]);
    return its;
    }

void
pm_freearray( its, rows )
    char** its;
    int rows;
    {
    free( its[0] );
    free( its );
    }


/* Case-insensitive keyword matcher. */

int
pm_keymatch( str, keyword, minchars )
    char* str;
    char* keyword;
    int minchars;
    {
    register int len;

    len = strlen( str );
    if ( len < minchars )
	return 0;
    while ( --len >= 0 )
	{
	register char c1, c2;

	c1 = *str++;
	c2 = *keyword++;
	if ( c2 == '\0' )
	    return 0;
	if ( isupper( c1 ) )
	    c1 = tolower( c1 );
	if ( isupper( c2 ) )
	    c1 = tolower( c2 );
	if ( c1 != c2 )
	    return 0;
	}
    return 1;
    }


/* Log base two hacks. */

int
pm_maxvaltobits( maxval )
    int maxval;
    {
    if ( maxval <= 1 )
	return 1;
    else if ( maxval <= 3 )
	return 2;
    else if ( maxval <= 7 )
	return 3;
    else if ( maxval <= 15 )
	return 4;
    else if ( maxval <= 31 )
	return 5;
    else if ( maxval <= 63 )
	return 6;
    else if ( maxval <= 127 )
	return 7;
    else if ( maxval <= 255 )
	return 8;
    else if ( maxval <= 511 )
	return 9;
    else if ( maxval <= 1023 )
	return 10;
    else if ( maxval <= 2047 )
	return 11;
    else if ( maxval <= 4095 )
	return 12;
    else if ( maxval <= 8191 )
	return 13;
    else if ( maxval <= 16383 )
	return 14;
    else if ( maxval <= 32767 )
	return 15;
    else if ( maxval <= 65535 )
	return 16;
    else
	pm_error( "maxval of %d is too large!", maxval, 0,0,0,0 );
    }

int
pm_bitstomaxval( bits )
    int bits;
    {
    return ( 1 << bits ) - 1;
    }


/* Initialization. */

static char* pm_progname;

void
pm_init( argcP, argv )
    int* argcP;
    char* argv[];
    {
    pm_progname = rindex( argv[0], '/');
    if ( pm_progname == NULL )
	pm_progname = argv[0];
    else
	++pm_progname;
    }

void
pbm_init( argcP, argv )
    int* argcP;
    char* argv[];
    {
    pm_init( argcP, argv );
    }

/* Error handling. */

/* I'd use varargs here, but it can't be done portably, because (a) vfprintf()
** is not very widespread, and (b) varargs itself is not powerful enough to
** allow me to include a portable vfprintf().
**
** So instead, we have the gross hack of a fixed number of args.  It's not
** even clear how portable this hack is, but it does seem to work everywhere
** I've tried it.
*/

void
pm_message( fmt, v1, v2, v3, v4, v5 )
    char* fmt;
    char *v1, *v2, *v3, *v4, *v5;
    {
    fprintf( stderr, "%s: ", pm_progname );
    fprintf( stderr, fmt, v1, v2, v3, v4, v5 );
    fputc( '\n', stderr );
    }

void
pm_error( fmt, v1, v2, v3, v4, v5 )
    char* fmt;
    char *v1, *v2, *v3, *v4, *v5;
    {
    pm_message( fmt, v1, v2, v3, v4, v5 );
    exit( 1 );
    }

void
pm_perror( reason )
    char* reason;
    {
    extern char* sys_errlist[];
    extern int errno;
    char* e;

    e = sys_errlist[errno];

    if ( reason != 0 && reason[0] != '\0' )
	pm_error( "%s - %s", reason, e, 0,0,0 );
    else
	pm_error( "%s", e, 0,0,0,0 );
    }

void
pm_usage( usage )
    char* usage;
    {
    fprintf( stderr, "usage:  %s %s\n", pm_progname, usage );
    exit( 1 );
    }


/* File open/close that handles "-" as stdin and checks errors. */

FILE*
pm_openr( name )
    char* name;
    {
    FILE* f;

    if ( strcmp( name, "-" ) == 0 )
	f = stdin;
    else
	{
#ifdef MSDOS
	f = fopen( name, "rb" );
#else /*MSDOS*/
	f = fopen( name, "r" );
#endif /*MSDOS*/
	if ( f == NULL )
	    {
	    pm_perror( name );
	    exit( 1 );
	    }
	}
    return f;
    }

FILE*
pm_openw( name )
    char* name;
    {
    FILE* f;

#ifdef MSDOS
    f = fopen( name, "wb" );
#else /*MSDOS*/
    f = fopen( name, "w" );
#endif /*MSDOS*/
    if ( f == NULL )
	{
	pm_perror( name );
	exit( 1 );
	}
    return f;
    }

void
pm_close( f )
    FILE* f;
    {
    if ( f != stdin )
	if ( fclose( f ) != 0 )
	    pm_perror( "fclose" );
    }

/* Broken putc() fix. */

#ifdef PBMPLUS_BROKENPUTC2
int
putc( c, stream )
    char c;
    FILE* stream;
    {
    return fwrite( &c, 1, 1, stream ) == 1 ? c : EOF;
    }
#endif /*PBMPLUS_BROKENPUTC2*/

/* Endian I/O.
*/

int
pm_readbigshort( in, sP )
    FILE* in;
    short* sP;
    {
    int c;

    if ( (c = getc( in )) == EOF )
	return -1;
    *sP = ( c & 0xff ) << 8;
    if ( (c = getc( in )) == EOF )
	return -1;
    *sP |= c & 0xff;
    return 0;
    }

#ifdef __STDC__
int
pm_writebigshort( FILE* out, short s )
#else /*__STDC__*/
int
pm_writebigshort( out, s )
    FILE* out;
    short s;
#endif /*__STDC__*/
    {
    if ( putc( ( s >> 8 ) & 0xff, out ) == EOF )
	return -1;
    if ( putc( s & 0xff, out ) == EOF )
	return -1;
    return 0;
    }

int
pm_readbiglong( in, lP )
    FILE* in;
    long* lP;
    {
    int c;

    if ( (c = getc( in )) == EOF )
	return -1;
    *lP = ( c & 0xff ) << 24;
    if ( (c = getc( in )) == EOF )
	return -1;
    *lP |= ( c & 0xff ) << 16;
    if ( (c = getc( in )) == EOF )
	return -1;
    *lP |= ( c & 0xff ) << 8;
    if ( (c = getc( in )) == EOF )
	return -1;
    *lP |= c & 0xff;
    return 0;
    }

int
pm_writebiglong( out, l )
    FILE* out;
    long l;
    {
    if ( putc( ( l >> 24 ) & 0xff, out ) == EOF )
	return -1;
    if ( putc( ( l >> 16 ) & 0xff, out ) == EOF )
	return -1;
    if ( putc( ( l >> 8 ) & 0xff, out ) == EOF )
	return -1;
    if ( putc( l & 0xff, out ) == EOF )
	return -1;
    return 0;
    }

int
pm_readlittleshort( in, sP )
    FILE* in;
    short* sP;
    {
    int c;

    if ( (c = getc( in )) == EOF )
	return -1;
    *sP = c & 0xff;
    if ( (c = getc( in )) == EOF )
	return -1;
    *sP |= ( c & 0xff ) << 8;
    return 0;
    }

#ifdef __STDC__
int
pm_writelittleshort( FILE* out, short s )
#else /*__STDC__*/
int
pm_writelittleshort( out, s )
    FILE* out;
    short s;
#endif /*__STDC__*/
    {
    if ( putc( s & 0xff, out ) == EOF )
	return -1;
    if ( putc( ( s >> 8 ) & 0xff, out ) == EOF )
	return -1;
    return 0;
    }

int
pm_readlittlelong( in, lP )
    FILE* in;
    long* lP;
    {
    int c;

    if ( (c = getc( in )) == EOF )
	return -1;
    *lP = c & 0xff;
    if ( (c = getc( in )) == EOF )
	return -1;
    *lP |= ( c & 0xff ) << 8;
    if ( (c = getc( in )) == EOF )
	return -1;
    *lP |= ( c & 0xff ) << 16;
    if ( (c = getc( in )) == EOF )
	return -1;
    *lP |= ( c & 0xff ) << 24;
    return 0;
    }

int
pm_writelittlelong( out, l )
    FILE* out;
    long l;
    {
    if ( putc( l & 0xff, out ) == EOF )
	return -1;
    if ( putc( ( l >> 8 ) & 0xff, out ) == EOF )
	return -1;
    if ( putc( ( l >> 16 ) & 0xff, out ) == EOF )
	return -1;
    if ( putc( ( l >> 24 ) & 0xff, out ) == EOF )
	return -1;
    return 0;
    }
