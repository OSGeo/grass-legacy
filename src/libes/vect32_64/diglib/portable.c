#include "digit.h"
#include "portable.h"
#include "gis.h"
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
static union type_conv u, u2;

static char *buffer = NULL;
static int buf_alloced = 0;

static unsigned char dbl_cnvrt[PORT_DOUBLE];
static unsigned char flt_cnvrt[PORT_FLOAT];
static unsigned char lng_cnvrt[PORT_LONG];
static unsigned char shrt_cnvrt[PORT_SHORT];

static int buf_alloc(int);

double *dig__double_convert (
    double *in,double *out,
    int count,
    struct dig_head *head)
{
    register int i, j;

    /* Pull 'count' doubles out of the incoming buffer 'in' */
    /*  and store them in the outgoing buffer 'out'         */
 
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (double));
	out = (double *) buffer;
    }

    if (!head->portable) 
    {
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
	return (out);  /* dont have to do anything */
    }

#if NATIVE_END_DBL = 1
    if (in != out)
        for (i = 0 ; i < count ; i ++)
	    out[i] = in[i];
#else
    /* TODO  change this to avoid FP errors when copying to u.d and out[i]  */
    for (i = 0 ; i < count ; i++)
    {
	u.d = in[i];
	for (j = 0 ; j < PORT_DOUBLE ; j++)
	    u2.c[head->dbl_cnvrt[j]] = u.c[j];
	out[i] = u2.d;
    }
#endif
    return (out);
}

float *dig__float_convert (
    float *in,float *out,
    int count,
    struct dig_head *head)
{
    register int i, j;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (float));
	out = (float *) buffer;
    }

    if (!head->portable) 
    {
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
	return (out);  /* dont have to do anything */
    }

#if NATIVE_END_FLOAT = 1
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
#else
    for (i = 0 ; i < count ; i++)
    {
	u.f = in[i];
	for (j = 0 ; j < PORT_FLOAT ; j++)
	    u2.c[head->flt_cnvrt[j]] = u.c[j];
	out[i] = u2.f;
    }
#endif
    return (out);
}

#ifdef INCLUDE_SHORT
short *dig__short_convert (
    short *in,short *out,
    int count,
    struct dig_head *head)
{
    register int i, j;
    register short tmp;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (short));
	out = (short *) buffer;
    }

    if (!head->portable) 
    {
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
	return (out);  /* dont have to do anything */
    }

#if NATIVE_END_SHORT = 1
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
#else
    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= PORT_SHORT ; j++)
	{
	    u2.c[PORT_SHORT-j] = (unsigned char) tmp & 0xff;
	    tmp >>= 8;
	}
	out[i] = u2.s;
    }
#endif
    return (out);
}
#endif

long *dig__long_convert (
    long *in,long *out,
    int count,
    struct dig_head *head)
{
    register int i, j;
    register long tmp;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (long));
	out = (long *) buffer;
    }

    if (!head->portable) 
    {
	if (in != out)
	{
	    for (i = 0 ; i < count ; i++)
		out[i] = in[i];
	}
	return (out);  /* dont have to do anything */
    }

#if NATIVE_END_LONG = 1
    if (in != out) {
        for (i = 0 ; i < count ; i++)
    	out[i] = in[i];
    }
#else
    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= PORT_LONG ; j++)
	{
	    u2.c[PORT_LONG-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = u2.l;
    }
#endif
    return (out);
}

/*
** in can NOT == out
** take an array of ints and return an array of converted longs
*/
long *dig__int_convert (
    int *in,
    long *out,
    int count,
    struct dig_head *head)
{
    register int i, j;
    register long tmp;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if ((long *)in == out)
	G_fatal_error ("Programmer error with dig__int_convert()");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (long));
	out = (long *) buffer;
    }

    if (!head->portable) 
    {
	for (i = 0 ; i < count ; i++)
	    out[i] = in[i];
	return (out);  /* dont have to do any conversion */
    }

#if NATIVE_END_LONG = 1
    for (i = 0 ; i < count ; i++)
        out[i] = in[i];
    return (out);  /* dont have to do any conversion */
#else 
    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= PORT_LONG ; j++)
	{
	    u2.c[PORT_LONG-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = u2.l;
    }
#endif
    return (out);
}

/*
** in can NOT == out
** take an array of ints and return an array of converted longs
*/
long *dig__plus_t_convert (
    plus_t *in,
    long *out,
    int count,
    struct dig_head *head)
{
    register int i, j;
    register long tmp;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if ((long *)in == out)
	G_fatal_error ("Programmer error with dig__plus_t_convert()");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (long));
	out = (long *) buffer;
    }

    if (!head->portable) 
    {
	for (i = 0 ; i < count ; i++)
	    out[i] = in[i];
	return (out);  /* dont have to do any conversion */
    }
#if NATIVE_END_LONG == 1
    for (i = 0 ; i < count ; i++)
        out[i] = in[i];
    return (out);  /* dont have to do any conversion */
#else
    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= PORT_LONG ; j++)
	{
	    u2.c[PORT_LONG-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = u2.l;
    }
#endif
    return (out);
}

/*
** in can NOT == out
** take an array of longs and return an array of converted ints
*/
int *dig__long_convert_to_int (
    long *in,
    int *out,
    int count,
    struct dig_head *head)
{
    register int i, j;
    register long tmp;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (in == (long *)out)
	G_fatal_error ("Programmer error with dig__plus_t_convert()");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (int));
	out = (int *) buffer;
    }

    if (head->lng_quick || !head->portable) 
    {
	for (i = 0 ; i < count ; i++)
	    out[i] = (int) in[i];
	return (out);  /* dont have to do any conversion */
    }
#if NATIVE_END_LONG == 1
	for (i = 0 ; i < count ; i++)
	    out[i] = (int) in[i];
	return (out);  /* dont have to do any conversion */
#else
    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= PORT_LONG ; j++)
	{
	    u2.c[PORT_LONG-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = (int) u2.l;
    }
#endif
    return (out);
}
/*
** in can NOT == out
** take an array of longs and return an array of converted plus_t's
*/
plus_t *dig__long_convert_to_plus_t (
    long *in,
    plus_t *out,
    int count,
    struct dig_head *head)
{
    register int i, j;
    register long tmp;

    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (in == (long *)out)
	G_fatal_error ("Programmer error with dig__plus_t_convert()");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (plus_t));
	out = (plus_t *) buffer;
    }

    if (head->lng_quick || !head->portable) 
    {
	for (i = 0 ; i < count ; i++)
	    out[i] = (plus_t) in[i];
	return (out);  /* dont have to do any conversion */
    }
#if NATIVE_END_LONG == 1
    for (i = 0 ; i < count ; i++)
        out[i] = (plus_t) in[i];
    return (out);  /* dont have to do any conversion */
#else
    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= PORT_LONG ; j++)
	{
	    u2.c[PORT_LONG-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = (plus_t) u2.l;
    }
#endif
    return (out);
}

int dig__fill_head_portable (struct dig_head *head)
{
    dig_struct_copy (dbl_cnvrt, head->dbl_cnvrt, NATIVE_DOUBLE);
    dig_struct_copy (flt_cnvrt, head->flt_cnvrt, NATIVE_FLOAT);
    dig_struct_copy (lng_cnvrt, head->lng_cnvrt, NATIVE_LONG);
    dig_struct_copy (shrt_cnvrt, head->shrt_cnvrt, NATIVE_SHORT);

    head->dbl_quick  = NATIVE_END_DBL;
    head->flt_quick  = NATIVE_END_FLT;
    head->lng_quick  = NATIVE_END_LNG;
    head->shrt_quick = NATIVE_END_SHRT;

    return 0;
}

/*  allows others to take advantage of existing conversion buffer
**  This should really not be used, except I use it in
**   dig__fread_port_I ()  before calling dig__long_convert_to_int ()
*/
char *dig__convert_buffer (int needed)
{
    if (0 > buf_alloc (needed))
    {
	dig_out_of_memory ();
	return NULL;
    }
    return buffer;
}

static int buf_alloc (int needed)
{
    char *p;
    int cnt;

    if (needed <= buf_alloced)
	return (0);
    cnt = buf_alloced;
    p = dig__alloc_space (needed, &cnt, 100, buffer, 1);
    if (p == NULL)
	return (dig_out_of_memory ());
    buffer = p;
    buf_alloced = cnt;
    return (0);
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
