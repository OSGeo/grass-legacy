#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "local_proto.h"

/******************************************************************/
/*                                                                */
/* getinfo - import info component of e00 - M. Wurtz (1998-10-10) */
/*                                                                */
/******************************************************************/

/* #define DEBUG for reading IFO section alone */

/*****************************************************************************

We can here determine wether the coverage is line, point, or polygon
related.  Table AAT only means line, AAT + PAT means polygon, PAT alone
means polygon or point (depending on the existence of an ARC section)

Arc/Info suffixes recognised :

.aat	Arc Attribute Table : extract attributes for line coverages
.pat	Polygon or Point Attribute Table : ditto
.acode	Arc Lookup Table : ignored
.pcode	Polygon or Point Lookup Table : ignored
.bnd	Boundary : set default Windows and create one named after the coverage
.tic	TIC Marks : ignored (could be used for reg files in Grass ?)
.sta	Stats for Grid file : ignored (could be used to find the best size of
	Grass raster file: 1, 2 or 4 bytes, or scaling a floating number grid)
.vat	Value Table for Grid file : ignored (but should be used for cats file
	if more than 2 fields ?)
.lut	Lookup (color) Table for Grid file : ignored (should be used to create
	the colr file for Grass raster... if only i know the A/I color sheme !)

*******************************************************************************/

struct Item {
    char fname[18];		/* name of item			   */
    int fpos;			/* position in data line	   */
    int fsize;			/* size for reading		   */
    int ftype;	         	/* type of data			   */
    char filename[64];		/* file for storing attributes	   */
    struct Categories cats;	/* for managing multiple attributs */
};

struct Info {
    char tname[34];
    char aifile[4];     /* XX if Arc/info file, spaces otherwise */
    int uitems;         /* number of usable items in this table  */
    int nitems;         /* number of items in this table         */
    int ldr;            /* length of data record                 */
    long ndr;           /* number of data records                */
    long length;        /* total lenght for one data line        */
    struct Item *item;  /* One per field...                      */
};
void igntbl(struct Info);
void gdbtbl(char *, struct Info, int, int);
void getbnd(struct Info, int);
void getpataat(char *, struct Info, int, int);

#ifdef DEBUG

int debug = 9;		/* debug level (verbosity) */
double scale = 1.0;	/* scale of coordinates (Cf PRJ) */
FILE *fdlog;

/* Debug works only with a non-compressed file... */

# define read_e00_line(x) fgets( x, 100, stdin)

int usecovnum = 1;
int usedatabase = 1;
int
main( void) {
    char line[100];
    extern int getinfo( char*, int);

    fdlog = stdout;
    do {
	fgets( line, 100, stdin);
    } while (strncmp( line, "IFO", 3));
    printf( "Cover_type = %d\n", getinfo( "xxx", 0));
}

#else

extern int debug;               /* debug level (verbosity) */
extern double scale;            /* scale of coordinates (Cf PRJ) */
extern FILE *fdlog;		/* log file descriptor */
extern int usecovnum;		/* coverage-# instead of coverage-ID */
extern int usedatabase;		/* Want ODBC or PosgreSQL ? */

#endif

/* read a line of data. may be splitted in e00 file      */
/* each input line must be padded with space to 80 chars */
/* CR and LF must be thrown away			 */

void getinfoline( char *s,  int size)
{
    char *p;
    int l;

    p = s;
    l = 0;
    read_e00_line( s);
    while( l < size) {
	if (*p =='\r' || *p == '\n' || *p == 0) {
	    while ((l%80 || p == s) && l < size) {
		l++;
		*p++ = ' ';
	    }
	    if (l == size)
		break;
	    else {
		read_e00_line( p);
		if (*p =='\r' || *p == '\n' || *p == 0) { /* if empty line */
		    l++;
		    *p++ = ' ';
		    *p = 0;
		}
	    }
	} else {
	    l++;
	    p++;
	}
    }
    *p = 0;
}

/* ignore a table (but read all data lines) */

void igntbl( struct Info info)
{
    char *infoline;
    int i;

    infoline = G_malloc( info.length + 3);
    if (debug > 2)
	fprintf( fdlog, "------------ Start of table %s (ignored) \n",
		info.tname);
    for (i = 0; i < info.ndr; i++) {
	getinfoline( infoline, info.length);
	if (debug > 5)
	    fprintf( fdlog, "%s (%ld)\n", infoline, strlen( infoline));
    }
    if (debug > 2)
	fprintf( fdlog, "End of table %s -------------\n", info.tname);
    G_free( infoline);
}

/* copy a table in database format */

void gdbtbl( char *name, struct Info info, int ncatmin, int flag)
{
    if (ncatmin == 0)
	igntbl( info);		/* still to do !!! */
    else
	getpataat( name, info, ncatmin, flag);
}

/* get boundaries and create a new region for import */

void getbnd( struct Info info, int flag)
{
    char *infoline;
    double xmin, ymin;
    double xmax, ymax;
    struct Cell_head region;    /* region and cellhd structure */

    if (debug)
	fprintf( fdlog, "INFO table .BND\n");

	/* We assume here that xmin,...ymax are always in same order ! */

    infoline = G_malloc( info.length + 3);
    getinfoline( infoline, info.length);
    sscanf( infoline, "%lf%lf%lf%lf", &xmin, &ymin, &xmax, &ymax);

    if (debug > 3) {
        fprintf( fdlog, "xmin = %f, ymin = %f\n", xmin, ymin);
        fprintf( fdlog, "xmax = %f, ymax = %f\n", xmax, ymax);
    }

    if (flag == 0)		/* Only analyse */
        return;
    if (debug)
        fprintf( fdlog, "Creating region\n");
    if (flag == 1)              /* We could create a WIND file */
        G_get_window( &region);
    else {
        G_get_default_window( &region);
        region.proj = 99;
        region.zone = 0;
    }
    region.east = xmax * scale;         /* scale is 1.0 but PRJ modify it */
    region.west = xmin * scale;         /* to be corrected : we must read */
    region.north = ymax * scale;        /* PRJ section before GRD section */
    region.south = ymin * scale;
    G_adjust_Cell_head( &region, 0, 0); /* compute default nb of rows and cols */

    if (flag > 1)			/* We could create a WIND file */
        G_put_window( &region);
    G_set_window( &region);

    G_free( infoline);
}

/* get Arc or Poly/Points attribute table => create dig_cats + dig_att files */

void getpataat( char *name, struct Info info, int ncatmin, int flag)
{
    char *infoline;
    char filename[80];
    int i, l;
    int id;		/* Ident of feature (not Arc/Info # !) */
    char label[1024];	/* Using malloc would be better...     */

    if (info.uitems <= ncatmin) {	/* no attribute other than topology */
	igntbl( info);
	return;
    }

    infoline = G_malloc( info.length + 3);
    for (i = ncatmin; i < info.uitems; i++) {
	if (info.uitems-ncatmin == 1)
	    strcpy( info.item[i].filename, name);
	else
	    sprintf( info.item[i].filename, "%s.%s", name, info.item[i].fname);
	G_init_cats( (CELL)info.ndr, info.item[i].filename, &info.item[i].cats);
	if (debug > 2)
	    fprintf( stderr, "Creating cats file \"%s\"\n", info.item[i].filename);
    }
    if (debug > 2)
	fprintf( stderr, "Done : ");
    for (l = 0; l < info.ndr; l++) {
	getinfoline( infoline, info.length);
	strncpy( label, &infoline[info.item[ncatmin-1-usecovnum].fpos],
		 info.item[ncatmin-1-usecovnum].fsize);
	label[info.item[ncatmin-1-usecovnum].fsize] = 0;
	sscanf( label, "%d", &id);
	if (debug > 2 && (l*100/(info.ndr-1))%2 == 0)
	    fprintf( stderr, "%4ld%%\b\b\b\b\b", l*100/(info.ndr-1));
	for (i = ncatmin; i < info.uitems; i++) {
	    strncpy( label, &infoline[info.item[i].fpos], info.item[i].fsize);
	    label[info.item[i].fsize] = 0;
	    G_set_cat( (CELL)id, label, &info.item[i].cats);
	}
    }
    if (debug > 2)
	fprintf( stderr, "\n");
    for (i = ncatmin; i < info.uitems; i++) {
	if (debug)
	    fprintf( fdlog, "Writing cats file \"%s\"\n", info.item[i].filename);
	if (flag != 0)
	    G_write_vector_cats( info.item[i].filename, &info.item[i].cats);
	G_free_cats( &info.item[i].cats);
    }
    G_free( infoline);
}

/* read INFO tables, keep only the most usefull */

int getinfo( char *name, int flag)
{
    char line[84], tmp[12], *p;
    int cover_type = 0; /* 1 if AAT, 2 if PAT, 3 if both */
    struct Info info;	/* Info tables                   */
    int i;

    read_e00_line( line);
    do {
	strncpy( info.tname, line, 32); info.tname[32] = 0;
	p = strchr( info.tname, ' ');
	*p = 0;
	strncpy( info.aifile, line+32, 2); info.aifile[2] = 0;
	strncpy( tmp, line+34, 4) ; tmp[4] = 0; info.uitems = atoi( tmp);
	strncpy( tmp, line+38, 4) ; tmp[4] = 0; info.nitems = atoi( tmp);
	strncpy( tmp, line+42, 4) ; tmp[4] = 0; info.ldr = atoi( tmp);
	strncpy( tmp, line+46, 11) ; tmp[11] = 0; info.ndr = atol( tmp);
	if (debug > 0)
	    fprintf( fdlog, "%s %s %d %d %ld\n", info.tname, info.aifile,
		     info.nitems, info.ldr, info.ndr);
	p = strchr( info.tname, '.');
	if (p == 0)
	    p = info.tname;
	else
	    p++;

	G_tolcase( p);
	info.length = 0;
	info.item = (struct Item *)G_malloc( info.nitems * sizeof( struct Item));

	for (i = 0; i < info.nitems; i++) {
	    read_e00_line( line);
	    sscanf( line, "%16s%d%*d%*d%*d%*d%*d%d",info.item[i].fname,
		    &info.item[i].fsize, &info.item[i].ftype);

	/* we could ignore all other fields including output format        */
	/* data following table specification have allways same length :   */
	/* 6 for short (2 bytes) integers, 11 for long (4 bytes) integers, */
	/* 14 for float, 24 for double and fsize for strings */

	    G_tolcase( info.item[i].fname);
	    if (info.item[i].ftype == 60)
		info.item[i].fsize = (info.item[i].fsize == 4 ? 14:24);
	    if (info.item[i].ftype == 50)
                info.item[i].fsize = (info.item[i].fsize == 2 ? 6:11);
	    if (info.item[i].ftype == 40)
		info.item[i].fsize = 14;
	    if (info.item[i].ftype == 10)
		info.item[i].fsize = 8;
	    if (i < info.uitems)
		info.length += info.item[i].fsize;
	    if (i == 0)
		info.item[i].fpos = 0;
	    else
		info.item[i].fpos = info.item[i-1].fpos + info.item[i-1].fsize;
	}
	if (strcmp( p, "aat") == 0) {
	    if (usedatabase)
		gdbtbl( name, info, 7, flag);
	    else
		getpataat( name, info, 7, flag);
	    cover_type += 1;
	}
	else if (strcmp( p, "pat") == 0) {
	    if (usedatabase)
		gdbtbl( name, info, 4, flag);
	    else
		getpataat( name, info, 4, flag);
	    cover_type += 2;
	}
	else if (strcmp( p ,"bnd") == 0)
	    getbnd( info, flag);
	else if (strcmp( p ,"tic") == 0)
	    igntbl( info);
	else if (strcmp( p ,"sta") == 0)
	    igntbl( info);
	else if (strcmp( p ,"vat") == 0)
	    igntbl( info);
	else if (strcmp( p ,"lut") == 0)
	    igntbl( info);
	else if (strcmp( p ,"acode") == 0)
	    igntbl( info);
	else if (strcmp( p ,"pcode") == 0)
	    igntbl( info);
	else
	    gdbtbl( name, info, 0, flag);

	read_e00_line( line);

    } while (strncmp( line, "EOI", 3));

    G_free( info.item);
    if (debug)
	fprintf( fdlog, "getinfo returns Cover_type = %d\n", cover_type);
    return cover_type;
}
