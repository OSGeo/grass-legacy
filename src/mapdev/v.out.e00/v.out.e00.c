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

/******************************************************************/
/*                                                                */
/* v.out.e00 -- export lines/areas to an ESRI e00 archive         */
/*  - M. Wurtz (v1.1) jan 2000                                    */
/*                                                                */
/* This program is an attempt to write a .e00 file                */
/* Since e00 is NOT a public format, this program                 */
/* is mainly based on analysis of existing files.                 */
/*                                                                */
/* There is then no warranty about this program and you are       */
/* warned that it will run at your own risks.                     */
/*                                                                */
/******************************************************************/

FILE *fde00;			/* output file descriptor */

int usedatabase = 0;		/* set to 1 if we use an attributes database */
double scale = 1.0;

struct Univ {
    plus_t N1;
    plus_t N2;
    plus_t area;
    int line;
    struct Univ *prev;
    struct Univ *next;
};

struct Univ *stack = (struct Univ*)NULL,
	 *universe = (struct Univ*)NULL;

int main( int argc, char *argv[]) 
{
    char *infile, *outfile;	/* name of output files */
    char msg[128];		/* for error messages */
    char name[32], nm[32], *p;	/* name of cover */
    char nnu[32], nid[32];
    int i, j, k, l, n, m;
    int level;			/* level of vector file (should be at least 2 */
    int nareas, nlines;		/* number of areas and lines */
    double x, y, *px, *py;
    double xmin, ymin, xmax, ymax;
    double ybase, *length, *perim, *lareas, *area;

    struct Map_info map;
    struct line_pnts *points;
    struct Cell_head region;
    P_ISLE *pisle;

    void add_line_universe( int, struct P_line);
    struct Univ *pu, *current, *segment;
    int npts, np_old;
    
	struct GModule *module;
    struct {
	struct Option *input, *output;
    } parm;

    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Write an Arc-Info line/polygon coverage in e00 format.";

    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of vector file to be exported";

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = NO;
    parm.output->description= "Name of .e00 output file";


    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    outfile = parm.output->answer;
    strncpy( name, infile, 27);
    for (p=name; *p; p++) {
	if (!isalnum( *p))
	    if (p == name)
		*p = 'X';
	    else
		*p = 0;
    }
    G_toucase( name);

    /* Open input file */

    if ((level = Vect_open_old( &map, infile, G_mapset())) < 0)
	G_fatal_error( "Vector file not found\n");
    if (level == 1)
	G_fatal_error( "Need to run v.support to build topology\n");
    points = Vect_new_line_struct();

    /* Open output file */

    if (outfile == NULL)
	fde00 = stdout;
    else
	if ((fde00 = fopen( outfile, "w")) == NULL) {    
	    sprintf (msg, "Cannot open output file \"%s\"", outfile);
	    G_fatal_error( msg);
	}

    fprintf( fde00, "EXP  0 %s/dig/%s\n", G_location_path(), infile);

    G_get_window( &region);
    xmin = region.east;
    xmax = region.west;
    ymin = region.south;
    ymax = region.north;

    /* change left/right area number accordingly to Arc/info usage */

    if ((nareas = V2_num_areas( &map)) != 0) {
	for (i=1; i<=nareas; i++) {
	    if (map.Area[i].alive == 0)
		continue;
	    for (j=0; j<map.Area[i].n_isles; j++) {
		k = map.Area[i].isles[j];
		if (map.Isle[k].alive != 0) {
		    for (n=0; n<map.Isle[k].n_lines; n++) {
			m = map.Isle[k].lines[n];
			if (m<0)
			    m = -m;
			if (map.Line[m].left < 0)
			    map.Line[m].left = i;
			if (map.Line[m].right < 0)
			    map.Line[m].right = i;
		    }
		}
	    }
	}
    }

    /* ARC SECTION */

    fprintf( fde00, "ARC  3\n");
    nlines = V2_num_lines( &map);
    length = (double*)G_malloc( (nlines+1)*sizeof(double));
    lareas = (double*)G_malloc( (nlines+1)*sizeof(double));
    stack = universe = (struct Univ*)NULL;

    for (i=1; i <= nlines; i++) {
	V2_read_line( &map, points, i);

	if (map.Line[i].W < xmin)
	    xmin = map.Line[i].W;
	if (map.Line[i].E > xmax)
	    xmax = map.Line[i].E;
	if (map.Line[i].S < ymin)
	    ymin = map.Line[i].S;
	if (map.Line[i].N > ymax)
	    ymax = map.Line[i].N;

	/* Universe polygon is always 1 for Arc/Info (?) */

	if (map.Line[i].type == LINE)
	    map.Line[i].left = map.Line[i].right = 0;

	if (map.Line[i].left < 0)
	    map.Line[i].left = 1;
	else
	    map.Line[i].left += 1;

	if (map.Line[i].right < 0)
	    map.Line[i].right = 1;
	else
	    map.Line[i].right += 1;

	if (map.Line[i].left == 1)
	    add_line_universe( -i, map.Line[i]);
	if (map.Line[i].right == 1)
	    add_line_universe( i, map.Line[i]);

	/* print header for each line */
	fprintf( fde00, "%10ld%10ld%10ld%10ld%10ld%10ld%10ld\n",
		i, map.Att[map.Line[i].att].cat, map.Line[i].N1,
		map.Line[i].N2, map.Line[i].left,
		map.Line[i].right, points->n_points);
	px = points->x; py = points->y;
	length[i] = lareas[i] = 0.0;
	ybase = (region.south + region.north)/2.0;
	for (j=0; j < points->n_points; j++) {
	    /* Compute length of lines for AAT and PAT tables (perimeter) */
	    if (j > 0) {
		length[i] += sqrt( ((*px - *(px-1)) * (*px - *(px-1)))
			      +((*py - *(py-1)) * (*py - *(py-1))));
		lareas[i] += (*px - *(px-1))*(((*py + *(py-1))/2) - ybase);
	    }
	    /* print coordinates of line */
	    fprintf( fde00, "%21.14lE%21.14lE\n", *px++, *py++);
	}
    }
    fprintf( fde00, "%10ld%10ld%10ld%10ld%10ld%10ld%10ld\n",
	    -1, 0L, 0L, 0L, 0L, 0L, 0L);

    if (nareas == 0)
	goto no_area;

    /* CNT SECTION */

    fprintf( fde00, "CNT  3\n");
    
    i=1;
    while (map.Area[i].alive == 0 || (j=map.Area[i].att) <= 0)
	i++;
    x = map.Att[j].x;
    y = map.Att[j].y;
    fprintf( fde00, "%10d%21.14lE%21.14lE\n", 0, x, y);

    for (i=1; i <= nareas; i++) {
	if (map.Area[i].alive == 0 || (j=map.Area[i].att) <= 0)
	    continue;
	x = map.Att[j].x;
	y = map.Att[j].y;
	fprintf( fde00, "%10d%21.14lE%21.14lE\n", 1, x, y);
/*************
	fprintf( fde00, "%10d\n", map.Att[j].cat);
*************/
	fprintf( fde00, "%10d\n", i);
    }
    fprintf( fde00, "%10ld%10ld%10ld%10ld%10ld%10ld%10ld\n",
	    -1, 0L, 0L, 0L, 0L, 0L, 0L);

    /* LAB SECTION */

    fprintf( fde00, "LAB  3\n");

    for (i=1; i <= nareas; i++) {
	if (map.Area[i].alive == 0 || (j=map.Area[i].att) <= 0)
	    continue;
	x = map.Att[j].x;
	y = map.Att[j].y;
	fprintf( fde00, "%10d%10d%21.14lE%21.14lE\n", map.Att[j].cat, i, x, y);
	fprintf( fde00, "%21.14lE%21.14lE\n", x, y);
	fprintf( fde00, "%21.14lE%21.14lE\n", x, y);
    }
    fprintf( fde00, "%10d%10d%21.14lE%21.14lE\n", -1, 0, 0.0, 0.0);

    /* PAL SECTION */

    fprintf( fde00, "PAL  3\n");
    perim = (double*)G_malloc( (nareas+1)*sizeof(double));
    area = (double*)G_malloc( (nareas+1)*sizeof(double));

    perim[0] = area[0] = 0.0;

    /* compute and write universe polygon... */

    npts = 2;
    segment = current = universe->next;
    while (stack != (struct Univ *)NULL) {
	while (current->N2 != segment->N1) {
	    np_old = npts;
	    for (pu=stack; pu != (struct Univ *)NULL; pu=pu->next) {
		if (current->N2 == pu->N1) {
		    current->next = pu;
		    if (pu->prev != (struct Univ *)NULL)
			pu->prev->next = pu->next;
		    else
			stack = pu->next;
		    if (pu->next != (struct Univ *)NULL)
			pu->next->prev = pu->prev;
		    npts++;
		    if (current->next == (struct Univ *)NULL)
			break;
		    current = current->next;
		    current->next = (struct Univ *)NULL;
		    break;
		}
	    }
	    if (npts == np_old) {	/* oops, a loop! should never happen */
		fprintf( stderr,
			"Oops ! loop while creating universe polygon\n");
		break;
	    }
	}
	/* new segment, put 0 0 0 here */
	if (stack == (struct Univ *)NULL)
	    break;
	pu = (struct Univ *)G_malloc( sizeof(struct Univ));
	current->next = pu;
	pu->line = 0;
	pu->N1 = 0;
	pu->area = 0;
	pu->next = segment = current = stack;
	stack = stack->next;
	stack->prev = (struct Univ *)NULL;
	current->next = (struct Univ *)NULL;
	npts += 2;
    }

    fprintf( fde00, "%10d%21.14lE%21.14lE\n", npts, xmin, ymin);
    fprintf( fde00, "%21.14lE%21.14lE\n", xmax, ymax);

    current = universe; l=1;
    while (current != (struct Univ*)NULL) {
	fprintf( fde00, "%10d%10d%10d", current->line, current->N1, current->area);
	if (current->line > 0) {
	    perim[0] += length[current->line];
	    area[0] += lareas[current->line];
	} else {
	    perim[0] += length[-current->line];
	    area[0] -= lareas[-current->line];
	}

	if (l=1-l)
	    fputc( '\n', fde00);
	current = current->next;
    }
    if (l==0)
	fputc( '\n', fde00);

    for (i=1; i <= nareas; i++) {
	if (map.Area[i].alive == 0)
	    continue;
	perim[i] = area[i] = 0.0;
	n = map.Area[i].n_lines;
	if (map.Area[i].n_isles > 0)
	    for (j=0; j<map.Area[i].n_isles; j++) {
		k = map.Area[i].isles[j];
		if (map.Isle[k].alive != 0)
		    n += (1+map.Isle[k].n_lines);
	    }
	fprintf( fde00, "%10d%21.14lE%21.14lE\n", n,
		 map.Area[i].W, map.Area[i].S);
	fprintf( fde00, "%21.14lE%21.14lE\n", map.Area[i].E, map.Area[i].N);
	l = 1;
	for (j=0; j<map.Area[i].n_lines; j++) {
	    k = map.Area[i].lines[j];
	    if (k > 0)
		fprintf( fde00, "%10d%10d%10d", k,
			 map.Line[k].N1, map.Line[k].left);
	    else
		fprintf( fde00, "%10d%10d%10d", k,
			 map.Line[-k].N2, map.Line[-k].right);
	    if (k > 0) {
		perim[i] += length[k];
		area[i] += lareas[k];
	    } else {
		perim[i] += length[-k];
		area[i] -= lareas[-k];
	    }

	    if (l=1-l)
		fputc( '\n', fde00);
	}
	if (map.Area[i].n_isles > 0)
	    for (j=0; j<map.Area[i].n_isles; j++) {
		k = map.Area[i].isles[j];
		if (map.Isle[k].alive != 0) {
		    fprintf( fde00, "%10d%10d%10d", 0, 0, 0);
		    if (l=1-l)
			fputc( '\n', fde00);
		    for (n=0; n < map.Isle[k].n_lines; n++) {
		    m = map.Isle[k].lines[n];
		    if (m > 0)
			fprintf( fde00, "%10d%10d%10d", m,
				 map.Line[m].N1, map.Line[m].left);
		    else
			fprintf( fde00, "%10d%10d%10d", m,
				 map.Line[-m].N2, map.Line[-m].right);
		    if (m > 0) {
			perim[0] += length[m];
			area[0] += lareas[m];
		    } else {
			perim[0] += length[-m];
			area[0] -= lareas[-m];
		    }
		    if (l=1-l)
			fputc( '\n', fde00);
		    }
		}
	    }

	if (l==0)
	    fputc( '\n', fde00);
    }

    fprintf( fde00, "%10ld%10ld%10ld%10ld%10ld%10ld%10ld\n",
	    -1, 0L, 0L, 0L, 0L, 0L, 0L);
    fprintf( fde00, "%21.14lE%21.14lE\n", 0.0, 0.0);

no_area:

    /* no TOLERANCE SECTION */
    /* must put e00write_tol() here */

    /* no SPATIAL INDEX SECTION */
    /* must put e00write_sin() here */

    /* no LOG SECTION */
    /* must put e00write_log() here */

    /* no PROJECTION INFOS */
    /* must put e00write_prj() here */

    /* INFO SECTION */
    /* BND table */

    fprintf( fde00, "IFO  2\n");
    strcpy( nm, name);
    strncat( nm, ".BND", 31);

    fprintf( fde00, "%-32.32sXX   4   4  16         1\n", nm);

    fprintf( fde00, "%s\n%s\n%s\n%s\n",
      "XMIN              4-1   14-1  12 3 60-1  -1  -1-1                   1-",
      "YMIN              4-1   54-1  12 3 60-1  -1  -1-1                   2-",
      "XMAX              4-1   94-1  12 3 60-1  -1  -1-1                   3-",
      "YMAX              4-1  134-1  12 3 60-1  -1  -1-1                   4-");
    fprintf( fde00, "%14.7lE%14.7lE%14.7lE%14.7lE\n", xmin, ymin, xmax, ymax);

    /* AAT table */

    strcpy( nm, name);
    strncat( nm, ".AAT", 31);
    strncpy( nnu, name, 13);
    strncpy( nid, name, 13);
    strncat( nnu, "#", 31);
    strncat( nid, "-ID", 31);

    fprintf( fde00, "%-32.32sXX   7   7  28%10ld\n", nm, nlines);
    fprintf( fde00, "%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n",
	"FNODE#", "  4-1   14-1   5-1 50-1  -1  -1-1                   1-",
	"TNODE#", "  4-1   54-1   5-1 50-1  -1  -1-1                   2-",
	"LPOLY#", "  4-1   94-1   5-1 50-1  -1  -1-1                   3-",
	"RPOLY#", "  4-1  134-1   5-1 50-1  -1  -1-1                   4-");
    fprintf( fde00, "%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n",
	"LENGTH", "  4-1  174-1  12 3 60-1  -1  -1-1                   5-",
	nnu, "  4-1  214-1   5-1 50-1  -1  -1-1                   6-",
	nid, "  4-1  254-1   5-1 50-1  -1  -1-1                   7-");

    for (i=1; i <= nlines; i++) {
	V2_read_line( &map, points, i);
	fprintf( fde00, "%11d%11d%11d%11d%14.7lE%11d%11d\n",
	  map.Line[i].N1, map.Line[i].N2, map.Line[i].left,
	  map.Line[i].right, length[i], i, map.Att[map.Line[i].att].cat);
    }

    /* PAT table */

    if (nareas == 0)
	goto finish;
    strcpy( nm, name);
    strncat( nm, ".PAT", 31);

    fprintf( fde00, "%-32.32sXX   4   4  16%10ld\n", nm, nareas);
    fprintf( fde00, "%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n",
	"AREA", "  4-1   14-1  12 3 60-1  -1  -1-1                   1-",
	"PERIMETER", "  4-1   54-1  12 3 60-1  -1  -1-1                   2-",
	nnu, "  4-1   94-1   5-1 50-1  -1  -1-1                   3-",
	nid, "  4-1  134-1   5-1 50-1  -1  -1-1                   4-");

    /* write universe polygon... */

    fprintf( fde00, "%14.7lE%14.7lE%11ld%11ld\n",
	  area[0], perim[0], 1, 0);
    for (i=1; i<=nareas; i++) {
	fprintf( fde00, "%14.7lE%14.7lE%11ld%11ld\n",
	  area[i], perim[i], i+1, map.Att[map.Area[i].att].cat);
    }

finish:
    fprintf( fde00, "EOI\nEOS\n");
    fclose( fde00);
    Vect_destroy_line_struct( points);
    Vect_close( &map);
}

void add_line_universe( int l, struct P_line p_line) {
    struct Univ *new;
    
    new = (struct Univ *)G_malloc( sizeof(struct Univ));
    new->line = l;
    if (l < 0) {
	new->N1 = p_line.N2;
	new->N2 = p_line.N1;
	new->area = p_line.right;
    } else {
	new->N1 = p_line.N1;
	new->N2 = p_line.N2;
	new->area = p_line.left;
    }
    new->prev = new->next = (struct Univ *)NULL;
    if (universe == (struct Univ *)NULL) {
	universe = (struct Univ *)G_malloc( sizeof(struct Univ));
	universe->line = 0;
	universe->N1 = 0;
	universe->area = 0;
	universe->next = new;
	universe->N2 = new->N1;
	universe->prev = (struct Univ *)NULL;
	return;
    }
    if (stack != (struct Univ *)NULL) {
	new->next = stack;
	stack->prev = new;
    }
    stack = new;
    return;
}
