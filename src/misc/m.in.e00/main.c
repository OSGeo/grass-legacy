#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <ctype.h>
#include <math.h>
#include <signal.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

/******************************************************************/
/*                                                                */
/* m.in.e00 -- import an ESRI e00 archive - M. Wurtz (v1.1) 11/99 */
/*                                                                */
/* This program is an attempt to read a .e00 file                 */
/* Since e00 is NOT a public format, this program                 */
/* is mainly based on analysis of existing files.                 */
/*                                                                */
/* There is then no waranty about this program and you are        */
/* warned that it will run at your own risks.                     */
/*                                                                */
/******************************************************************/

enum {ANALYSE, RASTER, LINES, VECTOR, ALL} todo;

int debug = 0;			/* debug level (verbosity) */
FILE *fde00, *fdlog;		/* input and log file descriptors */

int compressed;			/* 1 if e00 file is compressed, 0 else */
int current_position;		/* Where are we in input file ? */
int usecovnum = 1;		/* set to 1 if we want link table by COVER#  */
int usedatabase = 0;		/* set to 1 if we use an attributes database */
double scale = 1.0;

extern void getraster( char*, int, int);
extern long read_e00_line( char*);
extern int getinfo( char*, int);
extern int getarcs( char*, int, int);
extern void getproj( void);
extern void getsites( char*, int);
extern void getlabels( char*, int, int);

int main( int argc, char *argv[]) 
{
    extern void skip_arc( int);
    extern void skip_dat( void);
    extern void skip_lab( int);
    extern void skip_msk( void);
    extern void skip_pal( int);
    extern void skip_txt( int);

    char line[84];		/* line buffer for reading */
    char name[80], *p, *q;	/* name of output files */

    char *infile, *newmapset;
    long offset_grd = 0,
	 offset_arc = 0,
	 offset_lab = 0,	/* offset and precision of grid */
	 offset_pal = 0;	/* values and coordinates for   */
    int  prec_grd, prec_arc,	/* each section in e00 file :   */
	 prec_lab, prec_pal;	/* 0 = float, 1 = double        */
    int cover_type;		/* type of coverage (line, point, area) */
    int cover = 0;		/* 1 if AAT, 2 if PAT, 3 if both        */

    char buf[1024];
    char msg[256];

    struct {
	struct Option *input, *mapset, *action, *verbose, *logfile;
    } parm;
    struct {
	struct Flag *db, *link, *support;
    } flag;

    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of .e00 file to be imported";

    parm.mapset = G_define_option() ;
    parm.mapset->key        = "mapset";
    parm.mapset->type       = TYPE_STRING;
    parm.mapset->required   = NO;
    parm.mapset->description= "Name of mapset to hold resulting files (Default = current)";

    parm.action = G_define_option() ;
    parm.action->key        = "action";
    parm.action->type       = TYPE_STRING;
    parm.action->required   = NO;
    parm.action->description= "What to do on input file" ;
    parm.action->options    = "analyse,raster,lines,vector,all";
    parm.action->answer     = "all";

    parm.verbose = G_define_option() ;
    parm.verbose->key        = "verbose";
    parm.verbose->type       = TYPE_INTEGER;
    parm.verbose->required   = NO;
    parm.verbose->description= "Debugging level : 0 (silent) - 9 (verbose)" ;
    parm.verbose->answer     = "0" ;

    parm.logfile = G_define_option() ;
    parm.logfile->key        = "logfile";
    parm.logfile->type       = TYPE_STRING;
    parm.logfile->required   = NO;
    parm.logfile->description= "Name of file where log operations";

    flag.link = G_define_flag() ;
    flag.link->key         = 'i';
    flag.link->description = "Link attributes by coverage-ID not by coverage-#" ;

    flag.db = G_define_flag() ;	/* not working yet... */
    flag.db->key           = 'd';
    flag.db->description   = "Use database for storing attributes" ;

    flag.support = G_define_flag();
    flag.support->key = 's';
    flag.support->description = "Automatically run \"v.support\" on newly created vector file."; 


    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    newmapset = parm.mapset->answer;

    debug = atoi( parm.verbose->answer);
    if (parm.logfile->answer == NULL)
	fdlog = stderr;
    else
	if ((fdlog = fopen( parm.logfile->answer, "w")) == NULL) {    
	    sprintf (msg, "Cannot open log file \"%s\"", parm.logfile->answer);
	    G_fatal_error( msg);
	}
    switch (parm.action->answer[0]){
	case 'a': if (parm.action->answer[1] == 'l')
		     todo = ALL;
		  else
		     todo = ANALYSE;
		  break;
	case 'r': todo = RASTER; break;
	case 'l': todo = LINES; break;
	case 'v': todo = VECTOR; break;
    }

    if (flag.link->answer)
	usecovnum = 0;
    usedatabase = flag.db->answer;

    if ((todo == ANALYSE) && (debug < 5))
	debug = 5;

    if (debug > 5)
	fprintf( fdlog, "input=%s\nnewmapset=%s\naction=%s\nverbose=%d\n",
	    infile, newmapset, parm.action->answer, debug);

    /* Open input file and verify that's a good e00 file */

    fde00 = fopen (infile, "r");
    if (fde00 == NULL)
    {
	sprintf (msg, "%s - not found\n", infile);
	G_fatal_error (msg);
    }

    fgets( line, 84, fde00);
    if (strncmp( line, "EXP", 3)) {
	sprintf( msg, "\"%s\" is not an Arc-Info Export file !\n", infile);
	G_fatal_error (msg);
    }
    switch (line[5]) {
	case '0': compressed = 0;
		  break;
	case '1': compressed = 1;
		  break;
	default:  sprintf( msg, "Cannot handle \"%s\" : type %c\n", infile,
			line[5]);
		  G_fatal_error (msg);
    }

    if (debug)
	fprintf( fdlog, "\"%s\" successfully opened\n", infile);

    /* Create a mapset and made it current mapset for this program */

    if (newmapset != NULL) {
	if (G_legal_filename( newmapset) < 0) {
	    sprintf (msg, "MAPSET <%s> - illegal name\n", newmapset);
	    G_fatal_error( msg);
	}
	if (todo == ANALYSE)
	    fprintf( fdlog, "Mapset %s not created (analyse only)\n", newmapset);
	else {    
	    sprintf( msg, "%s/%s", G_location_path(), newmapset);
	    if (access( msg, F_OK) == -1)
		if (mkdir( msg, 0755) == -1) {
		    sprintf( msg, "Cannot create MAPSET %s", newmapset);
		    G_fatal_error( msg);
		}
	    G__setenv( "MAPSET", newmapset);
	    if (debug > 2)
		fprintf( fdlog, "Mapset \"%s\" created for import\n", G_mapset());
	}
    }

    /* extract name from header (cut .E00 and rewind until first separator) */

    p = strrchr( line, '.');
    if (p == NULL) {		/* too bad to be stopped here */
	p = strrchr( infile, '/');
	if (p == (char *) NULL)
	    p = infile;		/* we don't need complete path */
	strcpy( name, p);
	p = strchr( name, '.');	/* strip .e00 at end of name */
	if (p && p != name)
	    *p = 0;
    } else {
	*p-- = 0;
	while ((*p == '_') || isalnum( *p))
	    p--;
	p++; q = name;
	while (*p)
	    *q++ = tolower( *p++);
	*q = 0;
    }

    if (debug > 4)
	fprintf( fdlog, "Name of output file is \"%s\"\n", name);

    /* main loop through the archive */

    do {
	current_position = read_e00_line( line);
	if (debug)
	    fprintf( fdlog, "%s\n", line);

	if (!strncmp( line, "GRD  ", 5)) {      /* GRID SECTION */
	    if (todo == RASTER || todo == ALL || todo == ANALYSE) {
		offset_grd = current_position;
		prec_grd = line[5] - '2';
		if (debug > 2)
		    fprintf( fdlog, "GRD found at offset %ld\n", offset_grd);
		ignore( "EOG", 0);
	    } else
		ignore( "EOG", 1);
	    continue;
	}

	if (!strncmp( line, "ARC  ", 5)) {	/* ARC SECTION */
	    if (todo == VECTOR || todo == LINES || todo == ALL) {
		offset_arc = current_position;
		prec_arc = line[5] - '2';
		if (debug > 2)
		    fprintf( fdlog, "ARC found at offset %ld\n", offset_arc);
	    }
	    skip_arc( prec_arc);
	    continue;
	}

	if (!strncmp( line, "PAL  ", 5) ||
	    !strncmp( line, "PFF  ", 5)) {      /* POLYGON TOPOLOGY */
	    offset_pal = current_position;
	    prec_pal = line[5] - '2';
	    if (debug > 2)
		fprintf( fdlog, "P%c%c found at offset %ld\n",
			line[1], line[2], offset_pal);
	    skip_pal( prec_pal);		/* to see later ?   */
	    continue;
	}

	if (!strncmp( line, "CNT  ", 5)) {      /* CENTROID SECTION */
	    skip_dat();				/* is it realy useful ? */
	    continue;
	}

	if (!strncmp( line, "LAB  ", 5)) {      /* LABEL SECTION */
	    if (todo == VECTOR || todo == LINES || todo == ALL) {
		offset_lab = current_position;
		prec_lab = line[5] - '2';
		if (debug > 2)
		    fprintf( fdlog, "LAB found at offset %ld\n", offset_lab);
	    }
	    skip_lab( prec_lab);
	    continue;
	}

	if (!strncmp( line, "IFO  ", 5)) {     /* INFO SECTION */
	    if (todo == VECTOR || todo == ALL) /* Allways at end, but we want */
		if (todo == ANALYSE)	       /* it just after projection to */
		    cover = getinfo( name, 0); /* find wether it's a polygone */
		else			       /* line or point coverage      */
		    cover = getinfo( name, 1 + (newmapset != NULL));
	    else
		ignore( "EOI", 1);
	    continue;
	}

	if (!strncmp( line, "RPL  ", 5)) {      /* Specific to regions */
	    ignore( "JABBERWOCKY", 1);          /* Contains PAL formated data */
	    continue;				/* for each subclass */
	}

	if (!strncmp( line, "RXP  ", 5)) {      /* Specific to regions */
	    ignore( "JABBERWOCKY", 1);          /* Seems to link regions IDs */
	    continue;				/* to PAL polygons IDs */
	}

	if (!strncmp( line, "TXT  ", 5)) {      /* Annotations (text) */
	    skip_txt( line[5] - '2');           /* To be imported ? */
	    continue;				/* Does anybody have an idea? */
	}

	if (!strncmp( line, "TX6  ", 5)) {      /* Other kind of annotations */
	    ignore( "JABBERWOCKY", 1);          /* not same termination  */
	    continue;				/* Other differences ? */
	}

	if (!strncmp( line, "TX7  ", 5)) {      /* Very close from TX6 */
	    ignore( "JABBERWOCKY", 1);          /* So same questions and */
	    continue;				/* same rules... */
	}

	if (!strncmp( line, "LNK  ", 5)) {      /* UNKNOW KEYWORD SECTION */
	    ignore( "END OF LINK DATA", 1);     /* Don't know what to do with */
	    continue;				/* Does anybody have an idea? */
	}

	if (!strncmp( line, "SIN  ", 5)) {      /* SPATIAL INDEX SECTION */
	    ignore( "EOX", 1);                  /* Noting to do with it */
	    continue;
	}

	if (!strncmp( line, "CLN  ", 5) ||      /* Line pattern and palette  */
	    !strncmp( line, "CSH  ", 5)) {	/* Shade pattern and palette */
	    ignore( "EOS", 1);                  /* End same as e00 archive ! */
	    continue;
	}

	if (!strncmp( line, "FNT  ", 5)) {      /* Font description ? */
	    ignore( "EOF", 1);                  /* Noting to do with it */
	    continue;
	}

	if (!strncmp( line, "MSK  ", 5)) {      /* Mask description ? */
	    skip_msk();				/* Noting to do with it */
	    continue;
	}

	if (!strncmp( line, "TOL  ", 5)) {      /* TOLERANCE SECTION */
	    skip_dat();				/* should we really use it ? */
	    continue;
	}

	if (!strncmp( line, "PLT  ", 5)) {      /* PLOT SECTION */
		ignore( "EOP", 1);              /* why should we import it ? */
	    continue;
	}

	if (!strncmp( line, "LOG  ", 5)) {      /* LOG SECTION */
		ignore( "EOL", 1);              /* nothing to do with it */
	    continue;
	}

	if (!strncmp( line, "PRJ  ", 5)) {      /* PROJECTION INFOS */
	    if (newmapset)			/* We need them first */
		getproj();
	    else {
		if (debug > 2)
		    fprintf( fdlog, "Current Mapset : Ignoring projection data\n");
		do {
		    read_e00_line( line);
		    if (debug > 3 && *line != '~')
			fprintf( fdlog, "%s\n", line);
		    if (!strncmp( line, "Units", 5))
			sscanf( line+6, "%lf", &scale);
		} while (strncmp( line, "EOP", 3));
		scale = 1.0 / scale;
		if (debug > 2)
		    fprintf( fdlog, "Scale used = %f\n", scale);
	    }
	    continue;
	}
    } while (strncmp( line, "EOS", 3));

    /* extracting usefull information as noted before */

    if (offset_grd > 0) {
	fseek( fde00, offset_grd, SEEK_SET);
	if (todo == RASTER || todo == ALL)
	    getraster( name, 1 + (newmapset != NULL), prec_grd);
	else
	    getraster( name, 0, prec_grd);
    }

    switch (cover) {
	case 1 : cover_type = LINE;
		 break;
	case 2 : if (offset_arc != 0)
		    cover_type = AREA;
		 else
		    cover_type = DOT;
		 break;
	case 3 : if (offset_pal != 0 || offset_lab != 0)
		    cover_type = AREA;
		 else
		    cover_type = LINE;
		 break;
	default: if (offset_arc != 0)
		    cover_type = LINE;
		 else
		    cover_type = DOT;
		 break;
    }
    if (offset_arc != 0) {
	fseek( fde00, offset_arc, SEEK_SET);
	cover_type = getarcs( name, cover_type, prec_arc);
    }
    if (offset_lab != 0) {
	fseek( fde00, offset_lab, SEEK_SET);
	if (cover_type == DOT)
	    getsites( name, prec_lab);
	else
	    getlabels( name, cover_type, prec_lab);
    }
    if (debug)
	fprintf( fdlog, "Import of %s complete\n", name);
    
   /* If "-s" flag is passed as argument then run "v.support" on */
   /* newly created vector file (output).                        */
   if (flag.support->answer)
    {
     sprintf(buf,"%s/bin/v.support map=%s", G_gisbase(), name);
     G_system(buf);
     fprintf(stderr, "Done .\n");
    }

    exit(0);
}

/* ignoring input data up to the line beginning with the argument string */

void ignore( char *end, int flag)
{
    /* flag   {0,1}  indicate whether to print debug messages */
    int l;
    char line[84];		/* line buffer for reading */

    l = strlen( end);
    if (debug > 2 && flag)
	fprintf( fdlog, "Ignoring data\n");
    if (debug > 5 && flag)
	fprintf( fdlog, "Start of data ignored ---------->\n");

    do {
	read_e00_line( line);
	if (debug > 5 && flag)
	    fprintf( fdlog, "%s\n", line);
    } while (strncmp( line, end, l));

    if (debug > 5 && flag)
	fprintf( fdlog, "------------> End of data ignored\n");
}

/* Skip all numeric data until a line beginning with -1 */
/* we must have different functions for each section,   */
/* objects having different and variable size (ARC, LAB */
/* and PAL). lines of coordinates are silently ignored  */

/* Skip all ARC data until a arc # of -1 */

void skip_arc( int prec)
{
    int i, covnum, npts;
    char line[84];		/* line buffer for reading */
    long nbl, nbp;		/* number of lines and total of points */

    nbp = nbl = 0L;
    while (1) {
	read_e00_line( line);
	sscanf( line, "%d %*d %*d %*d %*d %*d %d", &covnum, &npts);
        if (covnum == -1)
	    break;
	nbl++; nbp +=npts;
	if (prec == 0)
	    npts = (npts+1)/2;	/* number of coordinate lines */
	for (i = 0; i < npts; i++)
	    read_e00_line( line);
    }
    if (debug)
	fprintf( fdlog, "Arc coverage : %ld arcs (%ld points)\n",
			nbl, nbp);
}

/* Skip all data until a line beginning with -1 (TOL and CNT sections) */

void skip_dat( void)
{
    int j;
    char line[84];		/* line buffer for reading */

    while (1) {
	read_e00_line( line);
	sscanf( line, "%d", &j);
        if (j == -1)
	    break;
    }
}

/* Skip all LAB data until a arc # of -1 */

void skip_lab( int prec)
{
    int covid;
    char line[84];		/* line buffer for reading */
    long nbl = 0;		/* number of label points */

    while (1) {
	read_e00_line( line);
	sscanf( line, "%d", &covid);
        if (covid == -1)
	    break;
	nbl++;
	read_e00_line( line);
	if (prec)		  /* two lines of coordinates */
	    read_e00_line( line); /* in double precision */
    }
    if (debug)
	fprintf( fdlog, "Label table : %ld entries\n", nbl);
}

/* Skip MSK data -- number of lines to skip may be false... */

void skip_msk()
{
    char line[84];
    double xmin, ymin, xmax, ymax, res, sk;
    long xsize, ysize, nskip;

    read_e00_line( line);
    sscanf( line, "%lf %lf %lf", &xmin, &ymin, &xmax);
    read_e00_line( line);
    sscanf( line, "%lf %lf %ld %ld", &ymax, &res, &xsize, &ysize);
    sk = ((ymax-ymin)/res) * ((xmax-xmin)/res) / 32.0;
    nskip = (long) ceil( sk/7.0);
    if (debug)
	fprintf( fdlog, "lines to skip : %ld (%ld x %ld)\n",
		 nskip, xsize, ysize);
    while (nskip--)
	read_e00_line( line);
}

/* Skip all PAL data until a line beginning with -1 */

void skip_pal( int prec)
{
    char line[84];		/* line buffer for reading */
    int i, narcs, nbp, nba;	/* counts arcs and polygons */

    nbp = nba = 0;
    for(;;) {
	read_e00_line( line);
	sscanf( line, "%d", &narcs);
	if (prec)		  /* two lines of coordinates */
	    read_e00_line( line); /* in double precision */
	if (narcs == -1)
	    break;
	nbp++; nba += narcs;
	for (i = (narcs + 1) / 2; i; i--)
	    read_e00_line( line);
    }
    if (debug)
	fprintf( fdlog, "PAL : %d polygons (%d arcs referenced)\n", nbp, nba);
}

/* Skip TXT data  until a line beginning with -1 */

void skip_txt( int prec)
{
    char line[84];		/* line buffer for reading */
    int i, n, nskip, nbt;

    nbt = 0;
    if (prec)
	nskip = 7;
    else
	nskip = 5;

    for(;;) {
	read_e00_line( line);
	sscanf( line, "%d", &n);
	if (n == -1)
	    break;
	nbt++;
	for( i=0; i < nskip; i++)
	    read_e00_line( line);
    }
    if (debug)
	fprintf( fdlog, "Annotations : %d texts found\n", nbt);
}
