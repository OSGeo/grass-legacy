#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include "lock.h"
#include "interface.h"

#ifndef NO_LOCKING
#include <sys/types.h>
#include <sys/stat.h>
#endif

static char *me ;
static int ascii_int = 0 ;
static int ascii_float = 0 ;
static char trace ;

static unsigned char *xalloc (int); 
static int send ( void *,int); 
static int sends (char *); 
static int sendi (int);
static int sendf (double );
static int rec (void *,int);
static int rec_opcode (char *);
static int recs (char *,int);
static int reci ();
static double recf ();
static int envi (char *,int);
static double envf (char *);
static int TRACE (char *);

int paint_interface (int argc, char *argv[])
{
/* environment values */
    double hres, vres;  	/* printer resolution */
    int nchars;			/* number of text chars */

/* these parms are temporary (i hope). They tell paint map
   how to relate the color boxes to the text number placed
   under the boxes
*/
    int blocksize, blockspace, nblocks;
    int textspace, textfudge;

/* textscale allows the printer to tell paint map
   to reduce/enlarge its graphic text characters
*/
    double textscale;

    char opcode ;
    float red, grn, blu ;
    int rlevel, glevel, blevel;
    int nrows, ncols, count, nc;
    unsigned char *databuf, *dp ;
    int databuflen;
    int i,j;
    char *env ;

    umask(0);

    j = 0;
    for (i = 0; argv[0][i]; i++)
	if (argv[0][i] == '/')
	    j = i + 1;
    me = &argv[0][j];

    if(trace = (getenv ("PAINT_TRACE")  != NULL))
    {
	fprintf (stderr, "PAINT: %s driver started\n", me);
	paint_delay(5);
    }

/* allocate the data buffer */
    databuf = xalloc (databuflen = 1024) ;

/* get the configuration parameters */
    hres       = envf ("HRES");
    vres       = envf ("VRES");
    nchars     = envi ("NCHARS",1);

/* get the others */
    textscale  = envf ("TEXTSCALE");
    textspace  = envi ("TEXTSPACE",1);
    textfudge  = envi ("TEXTFUDGE",0);
    blocksize  = envi ("BLOCKSIZE",1);
    blockspace = envi ("BLOCKSPACE",1);
    nblocks    = envi ("NBLOCKS",1);


/* determine transfer format for integers and floats */
    env = getenv ("TRANSPARENT");
    if (env == NULL)
	env = "n";
    send (env, 1);
    switch (*env)
    {
    case 'i':
    case 'I':
	ascii_int = 1;
	break;
    case 'f':
    case 'F':
	ascii_float = 1;
	break;
    case 'a':
    case 'A':
	ascii_float = 1;
	ascii_int = 1;
	break;
    }

    while (rec_opcode (&opcode))
    switch (opcode)
    {
    case HRES:
	TRACE("HRES");
	sendf (hres);
	break;
    case VRES:
	TRACE("VRES");
	sendf (vres);
	break;
    case NCHARS:
	TRACE("NCHARS");
	sendi (nchars);
	break;
    case NPIXELS:
	TRACE("NPIXELS");
	Pnpixels (&nrows, &ncols);
	sendi (nrows);
	sendi (ncols);
	break;
    case BLOCKSIZE:
	TRACE("BLOCKSIZE");
	sendi (blocksize);
	break;
    case BLOCKSPACE:
	TRACE("BLOCKSPACE");
	sendi (blockspace);
	break;
    case TEXTFUDGE:
	TRACE("TEXTFUDGE");
	sendi (textfudge);
	break;
    case TEXTSPACE:
	TRACE("TEXTSPACE");
	sendi (textspace);
	break;
    case NBLOCKS:
	TRACE("NBLOCKS");
	sendi (nblocks);
	break;
    case NCOLORS:
	TRACE("NCOLORS");
	sendi (Pncolors());
	break ;
    case PICTSIZE:
	TRACE("PICTSIZE");
	nrows = reci ();
	ncols = reci ();

	if (ncols * 2 > databuflen)
	{
	    free (databuf);
	    databuf = xalloc (databuflen = ncols * 2) ;
	}

	Ppictsize (nrows, ncols);
	break ;
    case ALPHA:
	TRACE("ALPHA");
	Palpha();
	break;
    case TEXT:
	TRACE("TEXT");
	recs (databuf,databuflen);
	Ptext(databuf);
	break;
    case TEXTSCALE:
	TRACE("TEXTSCALE");
	sendf (textscale);
	break ;
    case RASTER:
	TRACE("RASTER");
	Praster();
	break;
    case DATA:
	TRACE("DATA");
	rec (databuf, ncols);
	Pdata (databuf, ncols);
	break;
    case RLE:
	TRACE("RLE");
	count = 0 ;
	nc = 0 ;
	dp = databuf ;
	while (nc < ncols)
	{
	    rec (dp, 2);
	    count++;
	    nc += *dp;
	    dp += 2;
	}
	if (nc != ncols)
	{
	    char msg[100];
	    sprintf (msg,"RLE (got %d, should be %d)",nc,ncols);
	    paint_error (msg);
	}

	Prle (databuf, count);
	break;
    case COLORTABLE:
	TRACE("COLORTABLE");
	paint_colortable (reci());
	break;
    case COLORLEVELS:
	TRACE("COLORLEVELS");
	Pcolorlevels (&rlevel, &glevel, &blevel);
	sendi (rlevel);
	sendi (glevel);
	sendi (blevel);
	break;
    case COLORMULT:
	TRACE("COLORMULT");
	Pcolormultipliers (&rlevel, &glevel, &blevel);
	sendi (rlevel);
	sendi (glevel);
	sendi (blevel);
	break;
    case COLORVALUE:
	TRACE("COLORVALUE");
	Pcolorvalue (reci(), &red, &grn, &blu);
	sendf ((double) red);
	sendf ((double) grn);
	sendf ((double) blu);
	break;
    case COLORNUM:
	TRACE("COLORNUM");
	red = recf ();
	grn = recf ();
	blu = recf ();
	sendi (Pcolornum (red, grn, blu));
	break;
    case FLUSH:
	TRACE("FLUSH");
	Pflush();
	break;
    case LOCK:
	env = getenv("MAPLP");
	sprintf (databuf,"LOCK(%s)",env?env:"");
	TRACE(databuf);
	i = paint_lock (env,0);
	sendi (i);
	if (i) exit(0);
	break;
    case OPEN:
	env = getenv("MAPLP");
	sprintf (databuf,"LOCK(%s)",env?env:"");
	TRACE(databuf);
	paint_lock (env,1);
	sprintf (databuf,"OPEN(%s)",env?env:"");
	TRACE(databuf);
	Popen (env);
	TRACE("INIT");
	Pinit();
	TRACE("ok");
	if (trace) paint_delay(3);
	break;
    case CLOSE:
	TRACE("FINISH");
	Pfinish();
	TRACE("CLOSE");
	Pclose();
	paint_unlock();
	exit(0);
    case RAW:
	TRACE("RAW");
	while ((nc = read (0, databuf, databuflen)) > 0)
	    Pout (databuf, nc);
	Pclose ();
	if (nc < 0)
	    paint_error ("read error");
	exit(0);
    default:
	{
	    char msg[100];
	    sprintf (msg, "unrecognized interface code %d(%03o)", opcode, opcode);
	    paint_error (msg);
	}
    }
    exit(1);	/* EOF for opcode */
}

static unsigned char *xalloc (int n)
{
    unsigned char *buf;

    buf = (unsigned char *) malloc (n);
    if (buf != NULL)
	return buf ;

    paint_error ("No Memory");

    return 0;
}

static int send (void *buf,int n)
{
    if(write (1, buf, n) != n)
	paint_error ("write error");

    return 0;
}

static int sends (char *s)
{
    send (s, strlen (s) + 1);

    return 0;
}

static int sendi (int i)
{
    if (ascii_int)
    {
	char buf[30];
	sprintf (buf, "%d", i);
	sends (buf);
    }
    else
    {
	int ii;
	ii = i;
	send (&ii, sizeof ii);
    }

    return 0;
}

static int sendf ( double f)
{
    if (ascii_float)
    {
	char buf[30];

	sprintf (buf, "%f", f);
	sends (buf);
    }
    else
    {
	double ff;

	ff = f;
	send (&ff, sizeof ff);
    }

    return 0;
}

static int rec (void *buf,int n)
{
    int i;
    char *cbuf = buf;
    
    while (n > 0)
    {
	i = read (0, cbuf, n);

	if (i == 0)
	    paint_error ("unexpected EOF");
	if (i < 0)
	    paint_error ("read error");
	cbuf += i;
	n -= i;
    }

    return 1;
}

static int 
rec_opcode (char *opcode)
{
    if (trace)
    {
	fprintf (stderr, "PAINT CODE: "); fflush(stderr);
    }
    switch (read (0, opcode, 1))
    {
    case 0: return 0;	/* EOF */
    case 1: return 1;	/* OK */
    }
    paint_error ("read error");

    return 0;
}

static int 
recs (char *buf, int n)
{
    char c;
    do
    {
	rec (&c, 1);
	if (--n > 0)
	    *buf++ = c;
    }
    while (c) ;
    *buf = 0;

    return 0;
}

static int 
reci (void)
{
    int i;

    if (ascii_int)
    {
	char buf[30];
	recs (buf, sizeof buf);
	sscanf (buf, "%d", &i);
    }
    else
	rec (&i, sizeof i);
    return i;
}

static double recf (void)
{
    double f;

    if (ascii_float)
    {
	char buf[100];

	recs (buf, sizeof buf);
	sscanf (buf, "%lf", &f);
    }
    else
	rec (&f, sizeof f);
    return f;
}

static int envi (char *name, int min)
{
    int i;
    char *env;
    char msg[100];

    i = 0 ;
    env = getenv (name);
    if(env == NULL)
    {
	sprintf (msg, "%s not set", name);
	paint_error (name);
    }
    if (sscanf (env, "%d%1s", &i, msg) != 1 || i < min)
    {
	sprintf (msg, "%s=%s - illegal value", name, env);
	paint_error (msg);
    }

    if (trace) fprintf (stderr, "PAINT: %s=%d\n", name,i);
    return i;
}

static double envf (char *name)
{
    double f;
    char *env;
    char msg[100];

    f = 0.0 ;
    env = getenv (name);
    if(env == NULL)
    {
	sprintf (msg, "%s not set", name);
	paint_error (msg);
    }
    if (sscanf (env, "%lf%1s", &f, msg) != 1 || f <= 0.0)
    {
	sprintf (msg, "%s=%s - illegal value", name, env);
	paint_error (msg);
    }

    if (trace) fprintf (stderr, "PAINT: %s=%f\n", name,f);
    return f;
}

static int TRACE (char *msg)
{
    if (trace) fprintf (stderr, "%s\n", msg);

    return 0;
}

/**** external routines that can be used by driver code ***/

/* error: print error message on stderr and die */
int paint_error (char *msg)
{
    fprintf (stderr, "ERROR: %s driver: %s\n", me, msg);
    fflush (stderr);
    exit(1);
}

int paint_delay (int n)
{
    long stop;
    stop = time(0) + n ;
    while (time(0) < stop) ;

    return 0;
}


static char lockfile_name[300];
static int locked = 0;

int paint_lock (char *maplp, int fatal)
{
#ifndef NO_LOCKING
    struct stat buf;
    char *GISBASE;

    if (locked) return 0;
    if (maplp == NULL || *maplp == 0) return 0;


/* find the inode for the port */
    if (stat (maplp, &buf) < 0)
	return 0;
    GISBASE = getenv ("GISBASE");
    if (GISBASE == NULL)
    {
	if (!fatal) return 2;
	paint_error ("GISBASE: variable not set");
    }

    sprintf (lockfile_name, "%s/locks/paint.%d", GISBASE, buf.st_ino);


    locked = 1;
    switch (lock_file (lockfile_name, getpid()))
    {
    case 1: return 0;  /* OK */
    case 0: if(!fatal) return 1;
	    paint_error ("printer already in use"); break;
    default: if(!fatal) return 2;
	    paint_error ("unable to create lock"); break;
    }
    exit(0);
#else
    return 0;
#endif
}

int paint_unlock (void)
{
    if (locked) unlink (lockfile_name);
    locked = 0;

    return 0;
}

int paint_colortable (int n)
{
    unsigned char red[TABLE_SIZE], grn[TABLE_SIZE], blu[TABLE_SIZE];
    unsigned char table[TABLE_SIZE];
    float r,g,b;
    int x;
    int i;

/*
 * receive n colors. note: the interface is designed so that n is
 * never larger than TABLE_SIZE
 */
    rec (red, n);
    rec (grn, n);
    rec (blu, n);

/* translate the colors */
    for (i = 0; i < n; i++)
    {
	x = (int) red[i];
	r = (float)x / 255.0;

	x = (int) grn[i];
	g = (float)x / 255.0;

	x = (int) blu[i];
	b = (float)x / 255.0;

	x = Pcolornum (r, g, b);
	if (x < 0 || x > 255) x = 0;
	table[i] = x;
    }

/* send back the table */
    send (table, n);

    return 0;
}
