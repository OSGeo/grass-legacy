#include <stdio.h>
/*
**  Written by Dave Gerdes  9/1988
**  US Army Construction Engineering Research Lab
*/


/* 
** 
**  This code is a quick hack to allow the writing of portable
**  binary data files.
**  The approach is to take known values and compare them against
**  the current machine's internal representation.   A cross reference
**  table is then built, and then all file reads and writes must go through
**  through these routines to correct the numbers if need be.
**
**  As long as the byte switching is symetrical, the conversion routines
**  will work both directions.

**  The integer test patterns are quite simple, and their choice was
**  arbitrary, but the float and double valued were more critical.

**  I did not have a specification for IEEE to go by, so it is possible
**  that I have missed something.  My criteria were:
**
**  First, true IEEE numbers had to be chosen to avoid getting an FPE.
**  Second, every byte in the test pattern had to be unique.   And
**  finally, the number had to not be sensitive to rounding by the 
**  specific hardware implementation.
**
**  By experimentation it was found that the number  1.3333  met
**  all these criteria for both floats and doubles

**  See the discourse at the end of this file for more information
**  
**
*/

typedef int plus_t;


#define TEST_PATTERN 1.3333
#define LONG_TEST 0x01020304		/* not used */
#define SHORT_TEST 0x0102		/* not used */
#define DBL_SIZ  8
#define FLT_SIZ  4
#define LNG_SIZ  4
#define SHRT_SIZ 2

/*
** assumptions:
**    double =    8 byte IEEE
**    float  =    4 byte IEEE
**    long   =    4 byte int
**    short  =    2 byte int
**
*/
union type_conv {
	double d;
	float  f;
	long   l;
	short  s;
	unsigned char   c[DBL_SIZ];
};
static union type_conv u, u2;

static char *buffer = NULL;
static int buf_alloced = 0;

static first_time = 1;
/* dbl_cmpr holds the bytes of an IEEE representation of  TEST_PATTERN */
static unsigned char dbl_cmpr[]={0x3f, 0xf5, 0x55, 0x32, 0x61, 0x7c, 0x1b,0xda};

/* flt_cmpr holds the bytes of an IEEE representation of  TEST_PATTERN */
static unsigned char flt_cmpr[] =  { 0x3f, 0xaa, 0xa9, 0x93 };
static unsigned char lng_cmpr[] =  { 0x01, 0x02, 0x03, 0x04 };
static unsigned char shrt_cmpr[] = { 0x01, 0x02 };

static unsigned char dbl_cnvrt[DBL_SIZ];
static unsigned char flt_cnvrt[FLT_SIZ];
static unsigned char lng_cnvrt[LNG_SIZ];
static unsigned char shrt_cnvrt[SHRT_SIZ];

static int dbl_quick, flt_quick, lng_quick, shrt_quick;


main ()
{
    return Checkout();
}

static 
Checkout ()
{
    register int i;
    int tmp;
    short s;
    long l;
    int ret = 0;

/* TODO   This code needs to affect flags in library */
    if (sizeof (double) != DBL_SIZ) 
	fprintf (stderr, "Warning: Double is size %d\n", sizeof (double)),ret=1;
    if (sizeof (float) != FLT_SIZ) 
	fprintf (stderr, "Warning: Float  is size %d\n", sizeof (float)),ret=1;
    if (sizeof (long) != LNG_SIZ) 
	fprintf (stderr, "Warning: Long   is size %d\n", sizeof (long)),ret=1;
    if (sizeof (short) != SHRT_SIZ) 
	fprintf (stderr, "Warning: Short  is size %d\n", sizeof (short)),ret=1;


    u.d = TEST_PATTERN;
    for (i = 0 ; i < DBL_SIZ ; i++)
    {
	tmp = find_offset (dbl_cmpr, u.c[i], DBL_SIZ);
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in double\n", u.c[i]),ret=1;
	dbl_cnvrt[i] = tmp;
    }
    u.f = TEST_PATTERN;
    for (i = 0 ; i < FLT_SIZ ; i++)
    {
	tmp = find_offset (flt_cmpr, u.c[i], FLT_SIZ);
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in float\n", u.c[i]),ret=1;
	flt_cnvrt[i] = tmp;
    }

    /* see if double is normal */
    if (dbl_cnvrt[0] == 0 && dbl_cnvrt[DBL_SIZ-1] == DBL_SIZ-1)
	dbl_quick = 1;
    else
	dbl_quick = 0;

    /* see if float is normal */
    if (flt_cnvrt[0] == 0 && flt_cnvrt[FLT_SIZ-1] == FLT_SIZ-1)
	flt_quick = 1;
    else
	flt_quick = 0;
    
    first_time = 0;

#ifdef FOO	/* 3.0 way of doing it */

    /* This was a real cute way of using the conversion routines
    ** themselves to check the values, but with all the init stuff and
    ** head stuff, I decided to simplify things
    */

    /* set up if need to do short and long conversions */
    shrt_quick = 0;
    s = SHORT_TEST;
    dig__short_convert (&s, &s, 1, NULL);
    if (s == SHORT_TEST)
	shrt_quick = 1;
    else
	shrt_quick = 0;

    lng_quick = 0;
    l = LONG_TEST;
    dig__long_convert (&l, &l, 1, NULL);
    if (l == LONG_TEST)
	lng_quick = 1;
    else
	lng_quick = 0;

#else	/* 4.0 way of doing it */

    u.l = LONG_TEST;
    for (i = 0 ; i < LNG_SIZ ; i++)
    {
	tmp = find_offset (lng_cmpr, u.c[i], LNG_SIZ);
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in long\n", u.c[i]),ret=1;
	lng_cnvrt[i] = tmp;
    }

    /* see if long is normal */
    if (lng_cnvrt[0] == 0 && lng_cnvrt[LNG_SIZ-1] == LNG_SIZ-1)
	lng_quick = 1;
    else
	lng_quick = 0;

    u.s = SHORT_TEST;
    for (i = 0 ; i < SHRT_SIZ ; i++)
    {
	tmp = find_offset (shrt_cmpr, u.c[i], SHRT_SIZ);
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in short\n", u.c[i]),ret=1;
	shrt_cnvrt[i] = tmp;
    }

    /* see if short is normal */
    if (shrt_cnvrt[0] == 0 && shrt_cnvrt[SHRT_SIZ-1] == SHRT_SIZ-1)
	shrt_quick = 1;
    else
	shrt_quick = 0;
#endif

    return ret;

}

/*
** match search_value against each char in basis. 
** return offset or -1 if not found
*/
static
find_offset (basis, search_value, size)
    unsigned char *basis;
    unsigned char search_value;
    int size;
{
    register int i;

    for (i = 0 ; i < size ; i++)
	if (basis[i] == search_value)
	    return (i);
    return (-1);
}



/*

    The 3.0 dig, and dig_plus files are inherently non-portable.  This 
can be seen in moving files between a SUN 386i and other SUN machines.
The recommended way to transport files was always to convert to ASCII
(b.a.vect) and copy the ASCII files:  dig_ascii and dig_att to the 
destination machine.

    The problem lies in the way that different architectures internally
represent data.   If a number is internally store as  0x01020304 on
a 680x0 family machine, the same number will be stored as
0x04030201 on an 80386 class machine.

    The CERL port of GRASS to the Compaq 386 already has code to deal
with this incompatibility.  This code converts all files that are written
out to conform to the 680x0 standard.  These binary files can then be 
shared between machines without conversion.
    This code is designed to work with the majority of computers in use
today that fit the following requirements:
    byte     ==  8 bits
    int      ==  4 bytes
    long     ==  4 bytes
    double   ==  IEEE standard 64 bit
    float    ==  IEEE standard 32 bit
    bytes can be swapped around in any reasonable way, but bits within each
    byte must be maintained in normal high to low ordering:  76543210

    If this ability is desired on a SUN 386i, for example, you simply
define the compiler flag  CERL_PORTABLE in the src/CMD/makehead  file
and recompile all of the mapdev programs.


    Binary DLG files are NOT supported by this code, and will continue to
be non-portable between different architectures.
    

 -dave gerdes
*/
