#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include "gis.h"
#include "Vect.h"

/******************************************************************/
/*                                                                */
/* m.in.e00 -- import an ESRI e00 archive - M. Wurtz (1998-10-10) */
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

double scale = 1.0;

main(argc,argv) 
int   argc;
char *argv[];
{
    extern void getraster( char*, int);
    extern void ignore( char*, int);
    extern void skip_data();
    extern void skip_pal();
    extern int getinfo( char*, int);
    extern int getarcs( char*, int);
    extern void getlabels( char*, int);

    char line[1024];		/* line buffer for reading */
    char name[80], *p, *q;	/* name of output files */

    char *infile, *newmapset, *title;
    long offset_grd = 0,
	 offset_arc = 0,
	 offset_lab = 0,
	 offset_pal = 0;
    int cover_type;		/* type of coverage (line, point, area) */
    int cover;			/* 1 if AAT, 2 if PAT, 3 if both */

    char buf[256];

    struct {
	struct Option *input, *mapset, *logfile, *action, *verbose;
    } parm;

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
	    sprintf (buf, "Cannot open log file \"%s\"", parm.logfile->answer);
	    G_fatal_error( buf);
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
    if ((todo == ANALYSE) && (debug < 5))
	debug = 5;

    if (debug > 5)
	fprintf( fdlog, "input=%s\nnewmapset=%s\naction=%s\nverbose=%d\n",
	    infile, newmapset, parm.action->answer, debug);

    /* Open input file and verify that's a good e00 file */

    fde00 = fopen (infile, "r");
    if (fde00 == NULL)
    {
	sprintf (buf, "%s - not found\n", infile);
	G_fatal_error (buf);
    }

    fgets( line, 1024, fde00);
    if (strncmp( line, "EXP", 3)) {
	sprintf( buf, "\"%s\" is not an Arc-Info Ascii Export file !\n", infile);
	G_fatal_error (buf);
    }
    if (strtol( line+4, (char *)NULL, 10)) {
	sprintf( buf, "Cannot handle \"%s\" : Binary export file...\n", infile);
	G_fatal_error (buf);
    }

    if (debug)
	fprintf( fdlog, "\"%s\" successfully opened\n", infile);

    /* Create a mapset and made it current mapset for this program */

    if (newmapset != NULL) {
	if (G_legal_filename( newmapset) < 0) {
	    sprintf (buf, "MAPSET <%s> - illegal name\n", newmapset);
	    G_fatal_error( buf);
	}
	if (todo == ANALYSE)
	    fprintf( fdlog, "Mapset %s not created (analyse only)\n", newmapset);
	else {    
	    sprintf( buf, "%s/%s", G_location_path(), newmapset);
	    if (access( buf, F_OK) == -1)
		if (mkdir( buf, 0755) == -1) {
		    sprintf( buf, "Cannot create MAPSET %s", newmapset);
		    G_fatal_error( buf);
		}
	    G__setenv( "MAPSET", newmapset);
	    if (debug > 2)
		fprintf( fdlog, "Mapset \"%s\" created for import\n", G_mapset());
	}
    }

    /* extract name from header (cut .E00 and rewind until first separator) */

    p = strrchr( line, '.');
    if (p == NULL)
	strcpy( name, infile);	/* too bad to be stopped here */
    else {
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
	fgets( line, 1024, fde00);
	if (debug)
	    fprintf( fdlog, line);

	if (!strncmp( line, "GRD  ", 5)) {      /* GRID SECTION */
	    if (todo == RASTER || todo == ALL || todo == ANALYSE) {
		offset_grd = ftell( fde00);
		if (debug > 2)
		    fprintf( fdlog, "GRD found at offset %ld\n", offset_grd);
		ignore( "EOG", 0);
	    } else
		ignore( "EOG", 1);
	    continue;
	}

	if (!strncmp( line, "ARC  ", 5)) {	/* ARC SECTION */
	    if (todo == VECTOR || todo == LINES || todo == ALL) {
		offset_arc = ftell( fde00);
		if (debug > 2)
		    fprintf( fdlog, "ARC found at offset %ld\n", offset_arc);
	    }
	    skip_data();
	    continue;
	}

	if (!strncmp( line, "PAL  ", 5)) {      /* POLYGON TOPOLOGY */
	    offset_pal = ftell( fde00);
	    skip_pal();				/* to see later ?   */
	    continue;
	}

	if (!strncmp( line, "CNT  ", 5)) {      /* CENTROID SECTION */
	    skip_data();			/* to see later ?   */
	    continue;
	}

	if (!strncmp( line, "LAB  ", 5)) {      /* LABEL SECTION */
	    if (todo == VECTOR || todo == LINES || todo == ALL) {
		offset_lab = ftell( fde00);
		if (debug > 2)
		    fprintf( fdlog, "LAB found at offset %ld\n", offset_lab);
	    }
	    skip_data();
	    continue;
	}

	if (!strncmp( line, "IFO  ", 5)) {      /* INFO SECTION */
	    if (todo == VECTOR || todo == ALL) 	/* Allways at end, but we want */
		if (todo == ANALYSE)		/* it just after projection to */
		    cover = getinfo( name, 0);	/* find wether it's a polygone */
		else				/* line or point coverage      */
		    cover = getinfo( name, 1 + (newmapset != NULL));
	    else
		ignore( "EOI", 1);
	    continue;
	}

	if (!strncmp( line, "RPL  ", 5)) {      /* UNKNOW KEYWORD SECTION */
	    ignore( "JABBERWOCKY", 1);          /* Don't know what to do with */
	    continue;				/* Does anybody have an idea? */
	}

	if (!strncmp( line, "RXP  ", 5)) {      /* UNKNOW KEYWORD SECTION */
	    ignore( "JABBERWOCKY", 1);          /* Don't know what to do with */
	    continue;				/* Does anybody have an idea? */
	}

	if (!strncmp( line, "TX6  ", 5)) {      /* UNKNOW KEYWORD SECTION */
	    ignore( "JABBERWOCKY", 1);          /* Don't know what to do with */
	    continue;				/* Does anybody have an idea? */
	}

	if (!strncmp( line, "LNK  ", 5)) {      /* UNKNOW KEYWORD SECTION */
	    ignore( "END OF LINK DATA", 1);     /* Don't know what to do with */
	    continue;				/* Does anybody have an idea? */
	}

	if (!strncmp( line, "SIN  ", 5)) {      /* SPATIAL INDEX SECTION */
	    ignore( "EOX", 1);                  /* Don't know what to do with */
	    continue;
	}

	if (!strncmp( line, "TOL  ", 5)) {      /* TOLERANCE SECTION */
	    skip_data();                        /* should we really yse that ? */
	    continue;
	}

	if (!strncmp( line, "PLT  ", 5)) {      /* PLOT SECTION */
		ignore( "EOP", 1);              /* why should we import this ? */
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
		    fgets( line, 1024, fde00);
		    if (debug > 3 && *line != '~')
			fprintf( fdlog, line);
		    if (!strncmp( line, "Units", 5))
			sscanf( line+6, "%lf", &scale);
		} while (strncmp( line, "EOP", 3));
		scale = 1.0 / scale;
		if (debug > 2)
		    fprintf( fdlog, "Scale used = %lf\n", scale);
	    }
	    continue;
	}
    } while (strncmp( line, "EOS", 3));

    /* extracting usefull information as noted before */

    if (offset_grd > 0) {
	fseek( fde00, offset_grd, SEEK_SET);
	if (todo == RASTER || todo == ALL)
	    getraster( name, 1 + (newmapset != NULL));
	else
	    getraster( name, 0);
    }

    switch (cover) {
	case 1 : cover_type = LINE;
		 break;
	case 2 : if (offset_arc != 0)
		    cover_type = AREA;
		 else
		    cover_type = DOT;
		 break;
	case 3 : if (offset_pal != 0)
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
	cover_type = getarcs( name, cover_type);
    }
    if (offset_lab != 0) {
	fseek( fde00, offset_lab, SEEK_SET);
	if (cover_type == DOT)
	    getsites( name);
	else
	    getlabels( name, cover_type);
    }
    exit(0);
}

/* ignoring input data up to the line beginning with the argument string */

void ignore( char *end, int flag)
{
    int l;
    char line[1024];		/* line buffer for reading */

    l = strlen( end);
    if (debug > 2 && flag)
	fprintf( fdlog, "Ignoring data\n");
    if (debug > 5 && flag)
	fprintf( fdlog, "Start of data ignored ---------->\n");

    do {
	if (fgets( line, 1024, fde00) == NULL)
	    G_fatal_error( "End of file unexpected");
	if (debug > 5 && flag)
	    fprintf( fdlog, line);
    } while (strncmp( line, end, l));

    if (debug > 5 && flag)
	fprintf( fdlog, "------------> End of data ignored\n");
}

/* Skip all numeric data until a line beginning with -1 */
/* it seems that PAL section in double precision has a  */
/* line of coordinates after... it is silently ignored  */
/* by the main loop, but appears in the log file...     */

void skip_data()
{
    int j;
    char line[1024];		/* line buffer for reading */

    while (fgets( line, 1024, fde00) != NULL) {
	sscanf( line, "%d", &j);
        if (j == -1)
	    break;
    }
}

/* Skip all PAL data until a line beginning with -1. We */
/* cannot use skip_data, because the structure of this  */
/* section... it can be some -1 value at start of line  */

void skip_pal()
{
    int j;
    char line[84];		/* line buffer for reading */
    int i, n;
    double x1, y1, x2, y2;

    for(;;) {
	fscanf( fde00, "%d %lf %lf %lf %lf", &n, &x1, &y1, &x2, &y2);
	if (n == -1) {
	    fgets( line, 84, fde00);
	    break;
	}
	for (i = n*3; i; i--)
	    fscanf( fde00, "%d", &j);
    }
}
