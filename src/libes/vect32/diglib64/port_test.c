#include <stdio.h>
#include "portable.h.in"
/*
**  Moved this out of the run-time environment.  The portability parts
**   are known at compile-time.   Bill Hughes 12/1999
**  Adapted from code
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

#define TEST_PATTERN 1.3333
#define LONG_TEST 0x01020304		/* not used */
#define SHORT_TEST 0x0102		/* not used */

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
	unsigned char   c[PORT_DOUBLE];
};
static union type_conv u;

/* dbl_cmpr holds the bytes of an IEEE representation of  TEST_PATTERN */
static unsigned char dbl_cmpr[]={0x3f, 0xf5, 0x55, 0x32, 0x61, 0x7c, 0x1b,0xda};

/* flt_cmpr holds the bytes of an IEEE representation of  TEST_PATTERN */
static unsigned char flt_cmpr[] =  { 0x3f, 0xaa, 0xa9, 0x93 };
static unsigned char lng_cmpr[] =  { 0x01, 0x02, 0x03, 0x04 };
static unsigned char shrt_cmpr[] = { 0x01, 0x02 };

static unsigned char dbl_cnvrt[sizeof(double)];
static unsigned char flt_cnvrt[sizeof(float)];
static unsigned char lng_cnvrt[sizeof(long)];
static unsigned char shrt_cnvrt[sizeof(short)];

static int dbl_quick, flt_quick, lng_quick, shrt_quick;

static int find_offset(unsigned char *,unsigned char,int);

int main (int argc, char **argv)
{
    register int i;
    int tmp;
    int ret = 0;

    printf("/* Native Machine Size tests */\n");
    printf("#define NATIVE_DOUBLE %d\n",sizeof (double));
    printf("#define NATIVE_FLOAT %d\n",sizeof (float));
    printf("#define NATIVE_LONG %d\n",sizeof (long));
    printf("#define NATIVE_INT %d\n",sizeof (int));
    printf("#define NATIVE_SHORT %d\n",sizeof (short));

    printf("\n/* Translation matrices for PVF data */\n");
    u.d = TEST_PATTERN;
    for (i = 0 ; i < PORT_DOUBLE ; i++)
    {
	tmp = find_offset (dbl_cmpr, u.c[i], sizeof(double));
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in double\n", u.c[i]);
	dbl_cnvrt[i] = tmp;
    }

    u.f = TEST_PATTERN;
    for (i = 0 ; i < PORT_FLOAT; i++)
    {
	tmp = find_offset (flt_cmpr, u.c[i], sizeof(float));
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in float\n", u.c[i]);
	flt_cnvrt[i] = tmp;
    }
    
    u.l = LONG_TEST;
    for (i = 0 ; i < PORT_LONG; i++)
    {
	tmp = find_offset (lng_cmpr, u.c[i], sizeof(long));
	if (-1 == tmp)
	    fprintf (stderr,"ERROR, could not find '%x' in long\n", u.c[i]);
	lng_cnvrt[i] = tmp;
    }

    u.s = SHORT_TEST;
    for (i = 0 ; i < PORT_SHORT; i++)
    {
	tmp = find_offset (shrt_cmpr, u.c[i], sizeof(short));
	if (-1 == tmp)
	    fprintf (stderr, "ERROR, could not find '%x' in short\n", u.c[i]);
	shrt_cnvrt[i] = tmp;
    }

    dumpflags();
    return 0;
}

/*
** match search_value against each char in basis. 
** return offset or -1 if not found
*/
static int find_offset(unsigned char *basis,unsigned char search_value,int size)
{
    register int i;

    for (i = 0 ; i < size ; i++)
	if (basis[i] == search_value)
	    return (i);
    return (-1);
}

int dumpflags (void)
{
    int i;

	fprintf (stdout, "/* Double format: */\nstatic int dbl_cnvrt[] = {");
        i = 0;
	while (i < PORT_DOUBLE) {
	    fprintf (stdout, "%d", dbl_cnvrt[i]);
            if (++i < PORT_DOUBLE) fprintf (stdout,", ");
        }
        fprintf (stdout, "};\n\n");

	fprintf (stdout, "/* Float format : */\nstatic int flt_cnvrt[] = {");
	i = 0;
	while (i < PORT_FLOAT) {
	    fprintf (stdout, "%d", flt_cnvrt[i]);
            if (++i < PORT_FLOAT) fprintf (stdout,", ");
        }
        fprintf (stdout, "};\n\n");

	fprintf (stdout, "/* Long format  : */\nstatic int lng_cnvrt[] = {");
	i = 0;
	while (i < PORT_LONG) {
	    fprintf (stdout, "%d", lng_cnvrt[i]);
            if (++i < PORT_LONG) fprintf (stdout,", ");
        }
        fprintf (stdout, "};\n\n");

	fprintf (stdout, "/* Short format : */\nstatic int shrt_cnvrt[] = {");
	i = 0;
	while (i < PORT_SHORT) {
	    fprintf (stdout, "%d", shrt_cnvrt[i]);
            if (++i < PORT_SHORT) fprintf (stdout,", ");
        }
        fprintf (stdout, "};\n\n");

	return 0;
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
