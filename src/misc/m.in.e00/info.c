#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"

/******************************************************************/
/*                                                                */
/* getinfo - import info component of e00 - M. Wurtz (1998-10-10) */
/*                                                                */
/******************************************************************/

/* #define DEBUG for reading IFO section alone */

/*****************************************************************************

We can here determine wether the coverage is line, point, or polygon
related. table AAT only means line, AAT + PAT means polygon, PAT alone
means polygon or point (depending on the existence of an ARC section)

suffixe recognised (all others are silently ignored) :

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
.lut	Lookup (color) Table for Grid file : ignored (can be used to create
	the colr file for Grass raster... if only i know the A/I color sheme !)

*******************************************************************************/

struct Item {
    char fname[16];		/* name of item			   */
    int fpos;			/* position in data line	   */
    int fsize;			/* size for reading		   */
    int ftype;	         	/* type of data			   */
    char filename[64];		/* file for storing attributes	   */
    struct Categories cats;	/* for managing multiple attributs */
};

struct Info {
    char tname[36];
    char aifile[4];     /* XX if Arc/info file, spaces otherwise */
    int nitems;         /* number of items in this table         */
    int ldr;            /* length of data record                 */
    int ndr;            /* number of data records                */
    int length;         /* total lenght for one data line        */
    struct Item *item;  /* One per field...                      */
};
void igntbl(struct Info);
void gdbtbl(char *, char *, struct Info);
void getbnd(struct Info, int);
void getpataat(char *, struct Info, int, int);

#ifdef DEBUG
int debug = 9;               /* debug level (verbosity) */
double scale = 1;            /* scale of coordinates (Cf PRJ) */
FILE *fde00, *fdlog;
#else
extern int debug;               /* debug level (verbosity) */
extern double scale;            /* scale of coordinates (Cf PRJ) */
extern FILE *fde00, *fdlog;     /* input and log file descriptors */
#endif

extern enum {ANALYSE, RASTER, LINES, VECTOR, ALL} todo;

/* read a line of data. may be splitted in e00 file      */
/* each input line must be padded with space to 80 chars */
/* CR and LF must be thrown away			 */

void getline( char *s,  int size)
{
    char *p;
    int l;

    p = s;
    l = 0;
    fgets( s, size + 3, fde00);					/* +3 (CR, LF, \000) */
    while( l < size) {
	if (*p =='\r' || *p == '\n' || *p == 0) {
	    while ((l%80 || p == s) && l < size) {
		l++;
		*p++ = ' ';
	    }
	    if (l == size)
		break;
	    else {
		fgets( p, size - l + 3, fde00);
		if (*p =='\r' || *p == '\n' || *p == 0) {	/* if empty line */
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
    char *line;
    int i;

    line = G_malloc( info.length + 3);
    if (debug > 5)
	fprintf( fdlog, "------------ Start of table %s (ignored) \n", info.tname);
    for (i = 0; i < info.ndr; i++) {
	getline( line, info.length);
	if (debug > 6)
	    fprintf( fdlog, "%s (%ld)\n", line, strlen( line));
    }
    if (debug > 5)
	fprintf( fdlog, "End of table %s -------------\n", info.tname);
    G_free( line);
}

/* copy a table in database format */

void gdbtbl( char *name, char *p, struct Info info)
{
    igntbl( info);		/* still to do !!! */
}

/* get boundaries and create a new region for import */

void getbnd( struct Info info, int flag)
{
    char *line;
    double xmin, ymin;
    double xmax, ymax;
    struct Cell_head region;    /* region and cellhd structure */

    if (debug)
	fprintf( fdlog, "INFO table .BND\n");

	/* We assume here that xmin,...ymax are always in same order ! */

    line = G_malloc( info.length + 3);
    getline( line, info.length);
    sscanf( line, "%lf%lf%lf%lf", &xmin, &ymin, &xmax, &ymax);

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

    G_free( line);
}

/* get Arc or Poly/Points attribute table => create dig_cats + dig_att files */

void getpataat( char *name, struct Info info, int ncatmin, int flag)
{
    char *line;
    int i, l;
    int id;		/* Ident of feature (not Arc/Info # !) */
    char label[1024];	/* Using malloc would be better...     */

    if (info.nitems <= ncatmin) {
	igntbl( info);
	return;
    }
    
    line = G_malloc( info.length + 3);
    for (i = ncatmin; i < info.nitems; i++) {
	if (info.nitems-ncatmin == 1)
	    strcpy( info.item[i].filename, name);
	else
	    sprintf( info.item[i].filename, "%s.%s", name, info.item[i].fname);
	G_init_cats( (CELL)info.ndr, info.item[i].filename, &info.item[i].cats);
    }
    for (l = 0; l < info.ndr; l++) {
	getline( line, info.length);
	strncpy( label, &line[info.item[ncatmin-1].fpos],
		 info.item[ncatmin-1].fsize);
	label[info.item[ncatmin-1].fsize] = 0;
	sscanf( label, "%d", &id);
	for (i = ncatmin; i < info.nitems; i++) {
	    strncpy( label, &line[info.item[i].fpos], info.item[i].fsize);
	    label[info.item[i].fsize] = 0;
	    G_set_cat( (CELL)id, label, &info.item[i].cats);
	}
    }
    for (i = ncatmin; i < info.nitems; i++) {
	if (debug)
	    fprintf( fdlog, "Writing cats file \"%s\"\n", info.item[i].filename);
	if (flag != 0)
	    G_write_vector_cats( info.item[i].filename, &info.item[i].cats);
	G_free_cats( &info.item[i].cats);
    }
    G_free( line);
}

/* read INFO tables, keep only the most usefull */

int getinfo( char *name, int flag)
{
    char line[84], *p;
    int cover_type = 0; /* 1 if AAT, 2 if PAT, 3 if both */
    struct Info info;	/* Info tables                   */
    int i;
    
    if (fgets( line, 84, fde00) == NULL)
	G_fatal_error( "End of file unexpected");
    do {
	sscanf( line, "%32c%2c%4d%*4d%4d%11d",
		info.tname, info.aifile, &info.nitems, &info.ldr, &info.ndr);
	p = strchr( info.tname, ' ');
	*p = 0;
	info.aifile[2] = 0;
	if (debug > 0)
	    fprintf( fdlog, "%s %s %d %d %d\n", info.tname, info.aifile,
		     info.nitems, info.ldr, info.ndr);
	p = strchr( info.tname, '.');
	if (p == 0)
	    p = info.tname;
	else
	    p++;

	info.length = 0;
	info.item = (struct Item *)G_malloc( info.nitems * sizeof( struct Item));

	for (i = 0; i < info.nitems; i++) {
	    fscanf( fde00, "%16s%d%*d%*d%*d%*d%*d%d",info.item[i].fname,
		    &info.item[i].fsize, &info.item[i].ftype);
	    fgets( line, 81, fde00);

	/* we could ignore all other fields including output format           */
	/* data following table specification have allways same lenght :      */
	/* 11 for integers, 14 for float, 24 for double and fsize for strings */

	    if (info.item[i].ftype == 60)
		info.item[i].fsize = (info.item[i].fsize == 4 ? 14:24);
	    if (info.item[i].ftype == 50)
                info.item[i].fsize = 11;
	    info.length += info.item[i].fsize;
	    if (i == 0)
		info.item[i].fpos = 0;
	    else
		info.item[i].fpos = info.item[i-1].fpos + info.item[i-1].fsize;
	}
	if (strcmp( p, "AAT") == 0) {
	    getpataat( name, info, 7, flag);
	    cover_type += 1;
	}
	else if (strcmp( p, "PAT") == 0) {
	    getpataat( name, info, 4, flag);
	    cover_type += 2;
	}
	else if (strcmp( p ,"BND") == 0)
	    getbnd( info, flag);
	else if (strcmp( p ,"TIC") == 0)
	    igntbl( info);
	else
	    gdbtbl( name, p, info);

	if (fgets( line, 1024, fde00) == NULL)
	    G_fatal_error( "End of file unexpected");
    } while (strncmp( line, "EOI", 3));

    G_free( info.item);
    if (debug)
	fprintf( fdlog, "getinfo returns Cover_type = %d\n", cover_type);
    return cover_type;
}

#ifdef DEBUG

int 
main (void) {
    char line[100];

    fde00 = stdin;
    fdlog = stdout;
    do {
	fgets( line, 100, stdin);
    } while (strncmp( line, "IFO", 3));
    fprintf (stdout, "Cover_type = %d\n", getinfo( "xxx", 0));
}
#endif
