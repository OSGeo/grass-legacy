#include "Vect.h"
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
	unsigned char   c[DBL_SIZ];
};
static union type_conv u, u2;

static char *buffer = NULL;
static int buf_alloced = 0;

static int first_time = 1;
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

static int buf_alloc(int);
static int find_offset(unsigned char *,unsigned char,int);
static int Checkout(void);

/*
struct dig_head Def_head;

static int init_def_head ()
{
    dig_struct_copy  (flt_cnvrt, Def_head.flt_cnvrt, FLT_SIZ);
    dig_struct_copy  (dbl_cnvrt, Def_head.dbl_cnvrt, DBL_SIZ);
    dig_struct_copy  (lng_cnvrt, Def_head.lng_cnvrt, LNG_SIZ);
    dig_struct_copy  (shrt_cnvrt, Def_head.shrt_cnvrt, SHRT_SIZ);

    Def_head.dbl_quick = dbl_quick;
    Def_head.flt_quick = dbl_quick;
    Def_head.lng_quick = dbl_quick;
    Def_head.shrt_quick = dbl_quick;
}
*/

int dig__Init_portable_code (int portable)
{
    static int First = 1;
    static int ret = 0;
	
    if (portable)
    {
	if (First)
	{
	    First = 0;
	    ret = Checkout();
	}
    }
    else
	first_time = 0;

    return ret;
}

static int Checkout ()
{
    register int i;
    int tmp;
    int ret = 0;

/* TODO   This code needs to affect flags in library */
    if (sizeof (double) != DBL_SIZ) 
	fprintf (stderr, "Warning: Double is size %ld\n", sizeof (double)),ret=1;
    if (sizeof (float) != FLT_SIZ) 
	fprintf (stderr, "Warning: Float  is size %ld\n", sizeof (float)),ret=1;
    if (sizeof (long) != LNG_SIZ) 
	fprintf (stderr, "Warning: Long   is size %ld\n", sizeof (long)),ret=1;
    if (sizeof (short) != SHRT_SIZ) 
	fprintf (stderr, "Warning: Short  is size %ld\n", sizeof (short)),ret=1;

    /*
    init_def_head ();
    */

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
static int find_offset(unsigned char *basis,unsigned char search_value,int size)
{
    register int i;

    for (i = 0 ; i < size ; i++)
	if (basis[i] == search_value)
	    return (i);
    return (-1);
}


double * dig__double_convert (
    double *in,double *out,
    int count,
    struct dig_head *head)
{
    register int i, j;

    if (first_time)
	Checkout ();
	
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (double));
	out = (double *) buffer;
    }

    if (head->dbl_quick || !head->portable) 
    {
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
	return (out);  /* dont have to do anything */
    }


    /* TODO  change this to avoid FP errors when copying to u.d and out[i]  */
    for (i = 0 ; i < count ; i++)
    {
	u.d = in[i];
	for (j = 0 ; j < DBL_SIZ ; j++)
	    u2.c[head->dbl_cnvrt[j]] = u.c[j];
	out[i] = u2.d;
    }
    return (out);
}

float *dig__float_convert (
    float *in,float *out,
    int count,
    struct dig_head *head)
{
    register int i, j;

    if (first_time)
	Checkout ();
	
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (float));
	out = (float *) buffer;
    }

    if (head->flt_quick || !head->portable) 
    {
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
	return (out);  /* dont have to do anything */
    }


    for (i = 0 ; i < count ; i++)
    {
	u.f = in[i];
	for (j = 0 ; j < FLT_SIZ ; j++)
	    u2.c[head->flt_cnvrt[j]] = u.c[j];
	out[i] = u2.f;
    }
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

    if (first_time)
	Checkout ();
	
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (short));
	out = (short *) buffer;
    }

    if (head->shrt_quick || !head->portable) 
    {
	if (in != out)
	    for (i = 0 ; i < count ; i ++)
		out[i] = in[i];
	return (out);  /* dont have to do anything */
    }

    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= SHRT_SIZ ; j++)
	{
	    u2.c[SHRT_SIZ-j] = (unsigned char) tmp & 0xff;
	    tmp >>= 8;
	}
	out[i] = u2.s;
    }
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

    if (first_time)
	Checkout ();
	
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if (out == NULL)
    {
	buf_alloc (count * sizeof (long));
	out = (long *) buffer;
    }

    if (head->lng_quick || !head->portable) 
    {
	if (in != out)
	{
	    for (i = 0 ; i < count ; i++)
		out[i] = in[i];
	}
	return (out);  /* dont have to do anything */
    }

    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= LNG_SIZ ; j++)
	{
	    u2.c[LNG_SIZ-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = u2.l;
    }
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

    if (first_time)
	Checkout ();
	
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if ((long *)in == out)
	G_fatal_error ("Programmer error with dig__int_convert()");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (long));
	out = (long *) buffer;
    }

    if (head->lng_quick || !head->portable) 
    {
	for (i = 0 ; i < count ; i++)
	    out[i] = in[i];
	return (out);  /* dont have to do any conversion */
    }

    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= LNG_SIZ ; j++)
	{
	    u2.c[LNG_SIZ-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = u2.l;
    }
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

    if (first_time)
	Checkout ();
	
    if (head == NULL) fprintf (stderr, "ERROR: convert called w/ head=NULL\n");
    /*if (head == NULL) head = &Def_head;*/

    if ((long *)in == out)
	G_fatal_error ("Programmer error with dig__plus_t_convert()");

    if (out == NULL)
    {
	buf_alloc (count * sizeof (long));
	out = (long *) buffer;
    }

    if (head->lng_quick || !head->portable) 
    {
	for (i = 0 ; i < count ; i++)
	    out[i] = in[i];
	return (out);  /* dont have to do any conversion */
    }

    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= LNG_SIZ ; j++)
	{
	    u2.c[LNG_SIZ-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = u2.l;
    }
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

    if (first_time)
	Checkout ();
	
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

    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= LNG_SIZ ; j++)
	{
	    u2.c[LNG_SIZ-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = (int) u2.l;
    }
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

    if (first_time)
	Checkout ();
	
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

    for (i = 0 ; i < count ; i++)
    {
	tmp = in[i];
	for (j = 1 ; j <= LNG_SIZ ; j++)
	{
	    u2.c[LNG_SIZ-j] = (unsigned char) (tmp & 0xff);
	    tmp >>= 8;
	}
	out[i] = (plus_t) u2.l;
    }
    return (out);
}

int dig__fill_head_portable (struct dig_head *head)
{
    dig_struct_copy (dbl_cnvrt, head->dbl_cnvrt, DBL_SIZ);
    dig_struct_copy (flt_cnvrt, head->flt_cnvrt, FLT_SIZ);
    dig_struct_copy (lng_cnvrt, head->lng_cnvrt, LNG_SIZ);
    dig_struct_copy (shrt_cnvrt, head->shrt_cnvrt, SHRT_SIZ);

    head->dbl_quick  = dbl_quick;
    head->flt_quick  = flt_quick;
    head->lng_quick  = lng_quick;
    head->shrt_quick = shrt_quick;

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
