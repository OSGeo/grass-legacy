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
.bnd	Boundary : set default Windows and creates one named after the coverage
.tic	TIC Marks : ignored (could be used for reg files in Grass ?)
.sta	Stats for Grid file : ignored (could be used to find the best size of
	Grass raster file: 1, 2 or 4 bytes, or scaling a floating number grid)
.vat	Value Table for Grid file : ignored (but should be used for cats file
	if more than 2 fields ?)
.lut	Lookup (color) Table for Grid file : ignored (should be used to create
	the colr file for Grass raster... if only i know the A/I color sheme !)

*******************************************************************************/
struct idstr
{
	int id;
	char *str;
};

FILE *fde00;
struct Item {
    char fname[18];		/* name of item			   */
    int fpos;			/* position in data line	   */
    int fsize;			/* size for reading		   */
    int ftype;	         	/* type of data			   */
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
void gdbtbl(char *, struct Info, char **, int, int);
void getbnd(struct Info, int);
void get_info_att(char *, struct Info, char **, int, int);
int itemOK(char *, char**);
void insert_pair(int, char *, struct idstr **, int);

#ifdef DEBUG

int debug = 9;		/* debug level (verbosity) */
double scale = 1.0;	/* scale of coordinates (Cf PRJ) */
FILE *fdlog;

/* Debug works only with a non-compressed file... */

# define read_e00_line(x) fgets( x, 100, stdin)

int usecovnum = 1;
int
main( void) {
    char line[100];
    extern int getinfo( char*, int, int);

    fdlog = stdout;
    do {
	fgets( line, 100, stdin);
    } while (strncmp( line, "IFO", 3));
    printf( "Cover_type = %d\n", getinfo( "xxx", 2, 0));
}

#else

extern int debug;               /* debug level (verbosity) */
extern double scale;            /* scale of coordinates (Cf PRJ) */
extern FILE *fdlog;		/* log file descriptor */
extern int usecovnum;		/* coverage-# instead of coverage-ID */

#endif

void insert_pair(int id, char *label, struct idstr **pairs, int n)
{
	int i;
	struct idstr *l,*r;

	if(label==0)return;

	l=(struct idstr *)G_malloc(sizeof(struct idstr));
	l->id=id;
	l->str=G_malloc(1+strlen(label));
	strcpy(l->str,label);

	for(i=0; i<n && pairs[i] && pairs[i]->id<id; i++);

	if(i<n && !pairs[i])
	{
		pairs[i]=l;
		return;
	}

	while(i<n && pairs[i])
	{
		r=pairs[i];
		pairs[i++]=l;
		l=r;
	}
	return;
}

/* check a given item name to see if it's on the optional list of 
   requested items */
int itemOK(char *item, char **itemlist)
{
	int i;
	if(!item) return 0;
	if(!itemlist)return 1;
	for(i=0;*itemlist[i];i++)
	    if(strcmp(itemlist[i],item)==0)return 1;
	return 0;

}

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
	if (*p =='\r' || *p == '\n' || *p == 0) 
        { /* if we're at the end of the input line */
	    while ((l%80 || p == s) && l < size) 
            { /* overwrite with ' ' to multiple of 80 characters */
		l++;
		*p++ = ' ';
	    }
	    if (l == size)
		break;
	    else 
            { /* read the next input line */
		read_e00_line( p);
		if (*p =='\r' || *p == '\n' || *p == 0) 
                { /* if empty line */
		    l++;
		    *p++ = ' ';
		    *p = 0;
		}
	    }
	} 
        else 
        {
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

void gdbtbl( char *name, struct Info info, char **itemlist, int ncatmin, int flag_write)
{
    if (ncatmin == 0)
	igntbl( info);		/* still to do !!! */
    else
	get_info_att( name, info, itemlist, ncatmin, flag_write);
}

/* get boundaries and create a new region for import */

void getbnd( struct Info info, int flag_write)
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

    if (flag_write == 0)		/* Only analyse */
        return;
    if (debug)
        fprintf( fdlog, "Creating region\n");
    if (flag_write == 1)
        G_get_window( &region);
    else {				/* We should use PRJ section here */
        G_get_default_window( &region);
        region.proj = 99;
        region.zone = 0;
    }
    region.east = xmax * scale;         /* scale is 1.0 but PRJ modify it */
    region.west = xmin * scale;         /* to be corrected : we must read */
    region.north = ymax * scale;        /* PRJ section before GRD section */
    region.south = ymin * scale;
    G_adjust_Cell_head( &region, 0, 0); /* compute default nb of rows and cols */

    if (flag_write > 1)			/* We could create a WIND file */
        G_put_window( &region);
    G_set_window( &region);

    G_free( infoline);
}

/* get Arc or Poly/Points attribute table => create dig_cats + dig_att files */
/* called once for each info table */

void get_info_att( char *name, struct Info info, char **itemlist, int ncatmin, int flag_write)
{
    int i, l, cnt;
    int fileposition;
    int id;		/* Ident of feature (not Arc/Info # !) */
    char label[1024];	/* Using malloc would be better...     */
    char filename[64];
    struct Categories cats;
    char *infoline;
    int firstid, lastid;

    if (info.uitems <= ncatmin) {	/* no attribute other than topology */
	igntbl( info);
	return;
    }

    fileposition=ftell(fde00);
    infoline=G_malloc(3+info.length);

/* Repeat for each item (column) in the table */
    for (i = ncatmin; i < info.uitems; i++) {

        if (debug > 2)
            fprintf(stderr, "Checking item %s\n",info.item[i].fname);

        if(!itemOK(info.item[i].fname,itemlist))continue;

        fseek(fde00,fileposition,SEEK_SET);

	if (info.uitems-ncatmin == 1 && ncatmin != 1)
	    strcpy( filename, name);
	else
	    sprintf( filename, "%s.%s", name, info.item[i].fname);

        if (debug > 2)
	    fprintf( stderr, "Done : ");

	G_init_cats( info.ndr, info.item[i].fname, &cats);

/* Repeat for each data record in the table. */
        for (l = 0; l < info.ndr; l++) {

/* read the data record */
    	    getinfoline( infoline, info.length);

/* determine the record id.  Used as cat # for all data in the record */
	    strncpy( label, &infoline[info.item[ncatmin-1-usecovnum].fpos],
		 info.item[ncatmin-1-usecovnum].fsize);
	    label[info.item[ncatmin-1-usecovnum].fsize] = 0;
	    sscanf( label, "%d", &id);

	    if (debug > 6)
	        fprintf( fdlog, "%d", id);
	    if (debug > 2 && (l*100/(info.ndr-1))%2 == 0)
	        fprintf( stderr, "%4ld%%\b\b\b\b\b", l*100/(info.ndr-1));

/* get the cat label */
	    strncpy( label, &infoline[info.item[i].fpos], info.item[i].fsize);
	    label[info.item[i].fsize] = 0;

/* insert the id and label into the cats file */
            G_set_raster_cat(&id,&id,label,&cats,CELL_TYPE);

	    if (debug > 6)
		   fprintf( fdlog, "\t%s", label);
	}
	if (debug > 6)
	    fprintf( fdlog, "\n"); 
        if (debug > 2)
	    fprintf( stderr, "\n");

/* set the category ranges into the cats structure 
	*label=0;
	cnt=0;
	for(l=0;l<info.ndr;l++)
	{
		G_set_c_raster_cat(&pairs[l]->id,&pairs[l]->id, pairs[l]->str, &cats);
		if(strcmp(pairs[l]->str,label)==0)
		{
			lastid=pairs[l]->id;
			if(l<info.ndr-1)continue;
		}
		if(l>0)G_set_c_raster_cat(&firstid, &lastid, label, &cats);
		strcpy(label,pairs[l]->str);
		firstid=lastid=pairs[l]->id;
	}
*/

/* write the cats files */
	if (debug)
	    fprintf( fdlog, "Writing cats file \"%s\"\n", filename);
	if (flag_write != 0) {
	    if (ncatmin == 1)
		G_write_raster_cats(filename, &cats);
	    else
		G_write_vector_cats(filename, &cats);
	}
	G_free_cats( &cats);
    }

/* if no items were found then read to the section end */
    if(ftell(fde00)==fileposition)
    {
        G_warning("No category support");
        for(l=0;l<info.ndr;l++)
           getinfoline(infoline,info.length);
    }
    G_free(infoline);

}

/* read INFO tables, keep only the most usefull */

int getinfo( char *name, char **itemlist, int cat_management, int flag_write)
{
    char line[84], tmp[12], *p;
    int cover_type = 0; /* 1 if AAT, 2 if PAT, 3 if both */
    struct Info info;	/* Info tables                   */
    int i;

/* Repeat for each line until reaching "EOI" Each loop covers a different
   type of table */
    read_e00_line( line);
    do {

/* fill in an info structure from the contents of the line.  */
/* Fields have fixed lengths */
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

/* set p to point to the suffix in info.tname */
	p = strchr( info.tname, '.');
	if (p == 0)
	    p = info.tname;
	else
	    p++;

	G_tolcase( p);

/* alloc storage for the item structure */
	info.length = 0;
	info.item = (struct Item *)G_malloc( info.nitems * sizeof( struct Item));
/* read an additional line for each item and fill in the item structure */
	for (i = 0; i < info.nitems; i++) {
	    read_e00_line( line);
	    sscanf( line, "%16s%3d%*15c%3d",info.item[i].fname,
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

/* use the tname suffix to determine the type of data for this item, and */
/* read all attributes.  Called once for each info structure */

/* arc attribute table */
	if (strcmp( p, "aat") == 0) {
	    if (cat_management == 3)
		gdbtbl( name, info, itemlist, 7, flag_write);
	    else
		get_info_att( name, info, itemlist, 7, flag_write);
	    cover_type += 1;
	}
/* point or polygon attribute table */
	else if (strcmp( p, "pat") == 0) {
	    if (cat_management == 3)
		gdbtbl( name, info, itemlist, 4, flag_write);
	    else
		get_info_att( name, info, itemlist, 4, flag_write);
	    cover_type += 2;
	}
/* window boundary */
	else if (strcmp( p ,"bnd") == 0)	/* coverage boundaries */
	    getbnd( info, flag_write);
/* tic marks (ignored) */
	else if (strcmp( p ,"tic") == 0)	/* tick marks */
	    igntbl( info);
/* statistics lookup table for grid (ignored) */
	else if (strcmp( p ,"sta") == 0)	/* stats on grid */
	    igntbl( info);
/* value lookup table for grid */
	else if (strcmp( p ,"vat") == 0) {	/* value table (grid) */
	    usecovnum = 0;			/* first field = index */
	    get_info_att( name, info, itemlist, 1, flag_write);
	}
/* color lookup table for grid (ignored) */
	else if (strcmp( p ,"lut") == 0)	/* look-up table */
	    igntbl( info);
/* arc lookup table (ignored) */
	else if (strcmp( p ,"acode") == 0)	/* arc attributes */
	    igntbl( info);
/* point or polygon lookup table (ignored) */
	else if (strcmp( p ,"pcode") == 0)	/* polygon attributes */
	    igntbl( info);
	else
	    gdbtbl( name, info, itemlist, 0, flag_write);	/* non graphic tables */

/* free the info items list.  RM moved this from after the 
   do...while loop */
        G_free( info.item);

/* read a new line and repeat */
	read_e00_line( line);

    } while (strncmp( line, "EOI", 3));

    if (debug)
	fprintf( fdlog, "getinfo returns Cover_type = %d\n", cover_type);
    return cover_type;
}
