/* Cell-file area extraction */
/*   Input/output and line tracing routines */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* November 1987 - January 1988 */

/* input is a cell file found in the normal GRASS way */
/* outputs are binary digit files and a supplemental area file */
/* to be used to improve the dlg labelling process */

/* Global variables: */
/*    direction     indicates whether we should use fptr or bptr to */
/*                  move to the "next" point on the line */
/*    first_read    flag to indicate that we haven't read from input */
/*                  file yet */
/*    last_read     flag to indicate we have reached EOF on input */
/*    in_file_d     input cell file descriptor */
/*    ascii_digit   output ascii digit file */
/*    cell_name     input cell file name */
/*    dig_name      output digit file name */
/*    mapset        input raster(cell) map mapset */
/*    tmp_digit     temporary file where area information is stored before */
/*                  being remapped */
/*    row_length    length of each row of the cell file (i.e., number of */
/*                  columns) */
/*    n_rows        number of rows in the cell file */
/*    row_count     number of the row just read in--used to prevent reading */
/*                  beyond end of the cell file */
/*    equivs        pointer to allocated equivalence table made by */
/*                  write_equiv() */
/*    areas         pointer to allocated array of area information passed */
/*                  from bound.c */
/*    total_areas   number of distinct areas found */
/*    which_outputs which output files are to be generated */
/*    error_prefix  our name as found from the argument list */
/*    smooth_flag   this is 0 for no smoothing, 1 for smoothing of lines */

/* Entry points: */
/*    write_line    write a line out to the digit files */
/*    lab_digit     file pointer to attribute file */
/*    lab_name      name of the attribute file */
/*    write_area    make table of area mappings and write dlg label file */
/*    read_row      read another row of data--handles putting a "no data" */
/*                  boundary around the edges of the file */
/*    syntax        check syntax of command line and compile which_outputs */
/*    open_file     open input and output files */
/*    close_file    close input and output files */
/*    show          debugging routine to print out everything imaginable */
/*                  about a COOR structure */

#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "extr_areas.h"

#define BACKWARD 1
#define FORWARD 2
#define OPEN 1
#define END 2
#define LOOP 3
#define BINARY 1
#define ASCII 2
#define LABEL 4
#define AREAS 8

#define SMOOTH 1
#define NO_SMOOTH 0

#define CATNUM 0
#define CATLABEL 1

static int write_ln(struct COOR *, struct COOR *, int);
static int write_smooth_ln(struct COOR *, struct COOR *, int);
static struct COOR *move(struct COOR *);
static struct COOR *find_end(struct COOR *, int, int *, int *);
static int at_end(struct COOR *);
static int blank_line(CELL *);
static FILE *open_it(char *);
static int fill_head();

static struct Map_info Map;
static struct line_pnts *Points;

static struct Cell_head cell_head;
static int which_outputs;
static char *error_prefix;
static int direction;
static int first_read, last_read;
static char cell_name[256];
static char lab_name[256];
static char dig_name[256];
static char *mapset;
static FILE *lab_digit;
static int in_file_d;
static int row_length, row_count, n_rows, total_areas;
static int *equivs;
/* static struct area_table *areas; */
static int smooth_flag;
static int value_flag;
static struct dig_head head;


int io_init(void)
{
    Points = Vect_new_line_struct();

    return 0;
}

/* write_line - attempt to write a line to output */
/* just returns if line is not completed yet */

int write_line(struct COOR *seed)
{
    struct COOR *point, *line_begin, *line_end;
    int dir, line_type, n, n1;

    point = seed;
    if (dir = at_end(point)) {	/* already have one end of line */
	line_begin = point;
	line_end = find_end(point, dir, &line_type, &n);
	if (line_type == OPEN)
	    return (-1);	/* unfinished line */
	direction = dir;
    } else {			/* in middle of a line */

	line_end = find_end(point, FORWARD, &line_type, &n);
	if (line_type == OPEN)	/* line not finished */
	    return (-1);
	if (line_type == END) {	/* found one end at least *//* look for other one */
	    line_begin = find_end(point, BACKWARD, &line_type, &n1);
	    if (line_type == OPEN)	/* line not finished */
		return (-1);
	    if (line_type == LOOP) {	/* this should NEVER be the case */
		/*
		   fprintf(stderr,"%s:  write_line:  found half a loop!\n",error_prefix);
		 */
		return (-1);
	    }
	    direction = at_end(line_begin);	/* found both ends now; total length */
	    n += n1;		/*   is sum of distances to each end */
	} else {		/* line_type = LOOP by default */
	    /* already have correct length */
	    line_begin = line_end;	/* end and beginning are the same */
	    direction = FORWARD;	/* direction is arbitrary */
	}
    }
    if (smooth_flag == SMOOTH)
	write_smooth_ln(line_begin, line_end, n);
    else
	write_ln(line_begin, line_end, n);
    return (0);
}


/* write_ln - actual writing part of write_line */
/* writes binary and ASCII digit files and supplemental file */
static int write_ln( struct COOR *line_begin, struct COOR *line_end,	/* start and end point of line */
		    int n	/* number of points to write */
    )
{
    double x;
    double y;
    struct COOR *p, *last;
    int i, type;

    Vect_reset_line(Points);

    n++;			/* %% 6.4.88 */
    type = AREA;

    p = line_begin;
    y = cell_head.north - (double) p->row * cell_head.ns_res;
    x = cell_head.west + (double) p->col * cell_head.ew_res;

    if (which_outputs & BINARY)
	Vect_append_point(Points, x, y);

    for (i = 1; i < n; i++) {
	last = p;
	if ((p = move(p)) == NULPTR) {	/* this should NEVER happen */
	    fprintf(stderr,
		    "%s:  write_line:  line terminated unexpectedly\n",
		    error_prefix);
	    fprintf(stderr, "  previous (%d) point %p (%d,%d,%d) %p %p\n",
		    direction, last, last->row, last->col, last->node,
		    last->fptr, last->bptr);

	    exit(-1);
	}
	y = cell_head.north - p->row * cell_head.ns_res;
	x = cell_head.west + p->col * cell_head.ew_res;

	if (which_outputs & BINARY)
	    Vect_append_point(Points, x, y);

/*	free(last);*/
    }

    /* now free all the pointers */
    p = line_begin;

    for (i = 1; i < n; i++) {
	if (i < 10)
	    fprintf(stdout, " row: %d col: %d\n", p->row, p->col);
	last = p;
	if ((p = move(p)) == NULPTR)
	    break;
	if (last == p)
	    break;
	if (last->fptr != NULPTR)
	    if (last->fptr->fptr == last)
		last->fptr->fptr = NULPTR;
	/* it can be NULL after the previous line, even though before it wasn't */
	if (last->fptr != NULPTR)
	    if (last->fptr->bptr == last)
		last->fptr->bptr = NULPTR;
	if (last->bptr != NULPTR)
	    if (last->bptr->fptr == last)
		last->bptr->fptr = NULPTR;
	if (last->bptr != NULPTR)
	    if (last->bptr->bptr == last)
		last->bptr->bptr = NULPTR;
	G_free(last);
    }				/* end of for i */
    if (p != NULPTR)
	G_free(p);

    if (which_outputs & BINARY)
	Vect_write_line(&Map, type, Points);

    return 0;
}


/* write_smooth_ln - actual writing part of write_line for smoothed lines */
/* writes binary and ASCII digit files and supplemental file */
#define SNAP_THRESH 0.00001

static int write_smooth_ln( struct COOR *line_begin, struct COOR *line_end,	/* start and end point of line */
			   int n	/* number of points to write */
    )
{
    double x, y;
    double dx, dy;
    int idx, idy;
    struct COOR *p, *last;
    int i, total, type;

    Vect_reset_line(Points);
    n++;			/* %% 6.4.88 */
    type = AREA;

    p = line_begin;
    /* allocate the arrays and get the first point */

    y = cell_head.north - (double) p->row * cell_head.ns_res;
    x = cell_head.west + (double) p->col * cell_head.ew_res;
    Vect_append_point(Points, x, y);



    /* generate the list of smoothed points, may be duplicate points */
    total = 1;
    /*
       fprintf (stdout,"Writing line...%d points...\n", n);
     */
    for (i = 1; i < n; i++) {
	if (i < 10)
	    fprintf(stdout, " row: %d col: %d\n", p->row, p->col);
	last = p;
	if ((p = move(p)) == NULPTR) {	/* this should NEVER happen */
	    fprintf(stderr,
		    "%s:  write_line:  line terminated unexpectedly\n",
		    error_prefix);
	    fprintf(stderr, "  previous (%d) point %p (%d,%d,%d) %p %p\n",
		    direction, last, last->row, last->col, last->node,
		    last->fptr, last->bptr);
	    exit(-1);
	}

	idy = (p->row - last->row);
	idx = (p->col - last->col);
	dy = (idy > 0) ? 0.5 : ((idy < 0) ? -0.5 : 0.0);	/* dy = 0.0, 0.5, or -0.5 */
	dx = (idx > 0) ? 0.5 : ((idx < 0) ? -0.5 : 0.0);	/* dx = 0.0, 0.5, or -0.5 */
	y = cell_head.north - (last->row + dy) * cell_head.ns_res;
	x = cell_head.west + (last->col + dx) * cell_head.ew_res;
	total++;
	Vect_append_point(Points, x, y);

	y = cell_head.north - (p->row - dy) * cell_head.ns_res;
	x = cell_head.west + (p->col - dx) * cell_head.ew_res;
	total++;
	Vect_append_point(Points, x, y);

	/* free(last); */
    }				/* end of for i */

    y = cell_head.north - (double) p->row * cell_head.ns_res;
    x = cell_head.west + (double) p->col * cell_head.ew_res;
    total++;
    Vect_append_point(Points, x, y);

    /* strip out the duplicate points from the list */

    y = cell_head.north - (double) p->row * cell_head.ns_res;
    x = cell_head.west + (double) p->col * cell_head.ew_res;
    total++;
    Vect_append_point(Points, x, y);

/*DEBUG fprintf (stderr, "Total: %d  n_points: %d\n", total, Points->n_points); */

    /* strip out the duplicate points from the list */
/* TODO, -dpg  this may not be all that necessary?
    n = 1;
    xp = xarray+1; 
    yp = yarray+1;
    for (i=1; i<total; i++)
    {
	if ((fabs(*xp - *(xp-1)) < SNAP_THRESH) &&
	    (fabs(*yp - *(yp-1)) < SNAP_THRESH)) 
	{
	    xp++;
	    yp++;
	}
	else 
	{
	    xarray[n] = *xp++;
	    yarray[n] = *yp++;
	    n++;
	}
    }
*/

    /* write files */
    if (which_outputs & BINARY)
	Vect_write_line(&Map, type, Points);

    /* now free all thwe pointers */
    p = line_begin;

    for (i = 1; i < n; i++) {
	if (i < 10)
	    fprintf(stdout, " row: %d col: %d\n", p->row, p->col);
	last = p;
	if ((p = move(p)) == NULPTR)
	    break;
	if (last == p)
	    break;
	if (last->fptr != NULPTR)
	    if (last->fptr->fptr == last)
		last->fptr->fptr = NULPTR;
	/* now it can already ne NULL */
	if (last->fptr != NULPTR)
	    if (last->fptr->bptr == last)
		last->fptr->bptr = NULPTR;
	if (last->bptr != NULPTR)
	    if (last->bptr->fptr == last)
		last->bptr->fptr = NULPTR;
	if (last->bptr != NULPTR)
	    if (last->bptr->bptr == last)
		last->bptr->bptr = NULPTR;
	G_free(last);
    }				/* end of for i */
    if (p != NULPTR)
	G_free(p);

    return 0;
}



/* move - move to next point in line */
static struct COOR *move(struct COOR *point)
{
    if (direction == FORWARD) {
	if (point->fptr == NULPTR)	/* at open end of line */
	    return (NULPTR);
	if (point->fptr->fptr == point)	/* direction change coming up */
	    direction = BACKWARD;
	return (point->fptr);
    } else {
	if (point->bptr == NULPTR)
	    return (NULPTR);
	if (point->bptr->bptr == point)
	    direction = FORWARD;
	return (point->bptr);
    }

    return 0;
}

/* find_end - search for end of line, starting at a given point and */
/* moving in a given direction */
static struct COOR *find_end(struct COOR *seed, int dir, int *result,
			     int *n)
{
    struct COOR *start;

    start = seed;
    direction = dir;
    *result = *n = 0;
    while (!*result) {
	seed = move(seed);
	(*n)++;
	if (seed == start)
	    *result = LOOP;
	else {
	    if (seed == NULPTR)
		*result = OPEN;
	    else {
		if (at_end(seed))
		    *result = END;
	    }
	}
    }
    return (seed);
}

/* at_end - test whether a point is at the end of a line;  if so, give */
/* the direction in which to move to go away from that end */
static int at_end(struct COOR *ptr)
{
    if (ptr->fptr == ptr)
	return (BACKWARD);
    if (ptr->bptr == ptr)
	return (FORWARD);
    return (0);
}

/* re_map_areas - read back temporary area file and re-map the area */
/* information contained in it; write the results to the final area */
/* file */

int re_map_areas(void)
{
    if (!(which_outputs & AREAS))
	return (0);
    return 1;
}

/* write_area - make table of area equivalences and write attribute file */
int write_area( struct area_table *a_list,	/* list of areas */
	       struct equiv_table *e_list,	/* list of equivalences between areas */
	       int n_areas,	/* lengths of e_list, a_list */
	       int n_equiv)
{
    char type;
    int n, i;
    long count;
    struct area_table *p;
    char *temp_buf;
    struct Categories RastCats;
    struct Categories VectCats;
    struct Cell_stats stats;
    int cat;
    int catNum;
    char lbl[255];

    G_init_cell_stats(&stats);
    total_areas = 0;
    if (n_equiv < n_areas) {
	equivs = (int *) G_malloc(n_areas * sizeof(int));
	n = n_equiv;
    } else {
	equivs = (int *) G_malloc(n_equiv * sizeof(int));
	n = n_areas;
    }
    for (i = 0; i < n; i++) {
	if ((e_list + i)->mapped)
	    equivs[i] = (e_list + i)->where;
	else {
	    total_areas++;
	    equivs[i] = i;
	}
    }
    if (n < n_areas) {
	for (i = n; i < n_areas; i++) {
	    total_areas++;
	    equivs[i] = i;
	}
    }
    type = AREA;
    if (which_outputs & LABEL) {
        if (value_flag == CATLABEL) {
            G_init_cats((CELL) 0, "", &VectCats);
            catNum = 1;
        }
        for (i = 0, p = a_list; i < n_areas; i++, p++) {
            if (!(e_list + i)->mapped && p->width > 0 &&
                !G_is_c_null_value(&(p->cat))) {
                /* note: write_att does not outputs labels with 0 category value,
                   but anyway v.support does not build area with that category value...
                   we will wait for GRASS 5.1 to solve that problem */
                if(value_flag == CATLABEL) {
                    cat = catNum;
                    sprintf(lbl, "%d", p->cat);
                    G_set_cat(catNum, lbl, &VectCats);
                    catNum++;
                } else {
                    cat = p->cat;
                }

                G_update_cell_stats((CELL *) & (p->cat), 1, &stats);
                write_att(lab_digit, (char) dig_new_to_old_type(type),
                    cell_head.west + (p->col + (p->width / 2.0)) * cell_head.ew_res,
                    cell_head.north - (p->row + 0.5) * cell_head.ns_res,
                    cat);
	        }
	    }
        if (value_flag == CATNUM) {
            G_rewind_cell_stats(&stats);
            if (G_read_cats(cell_name, mapset, &RastCats))
                G_init_cats((CELL) 0, "", &RastCats);
            G_init_cats((CELL) 0, G_get_cats_title(&RastCats), &VectCats);
            while (G_next_cell_stat((CELL *) & cat, &count, &stats)) {
                temp_buf = G_get_cat(cat, &RastCats);
                if (strcmp(temp_buf, "") != 0)
                    G_set_cat(cat, temp_buf, &VectCats);
            }
            G_free_cats(&RastCats);
        }
        /* write out category labels and clean up */
        G_write_vector_cats(dig_name, &VectCats);
        G_free_cats(&VectCats);
        G_free_cell_stats(&stats);
    }

    return 0;
}

/* syntax - check syntax of command line and compile which_outputs to tell */
/* which output files the user wants generated; returns -1 on error, zero */
/* otherwise; default output files are binary digit and dlg label; anything */
/* other than default produces only the specific files requested */
int syntax(int argc, char *argv[], char **input, char **output)
{
    struct Flag *flag1;
    struct Flag *flag2;
    struct Option *opt1;
    struct Option *opt2;

    /* Define the different flags */

    flag1 = G_define_flag();
    flag1->key = 'l';
    flag1->description = "Smooth Corners";

    flag2 = G_define_flag();
    flag2->key = 'c';
    flag2->description = "Output values as category labels";

    /* Define the different options */
    opt1 = G_define_option();
    opt1->key = "input";
    opt1->type = TYPE_STRING;
    opt1->required = YES;
    opt1->multiple = NO;
    opt1->gisprompt = "old,cell,raster";
    opt1->description = "Name of an existing raster map";

    opt2 = G_define_option();
    opt2->key = "output";
    opt2->type = TYPE_STRING;
    opt2->required = YES;
    opt2->gisprompt = "new,dig,vector";
    opt2->description = "Name of a new vector map";

    *input = *output = NULL;

    if (G_parser(argc, argv))
	exit(-1);

    which_outputs = BINARY | LABEL;

    *input = opt1->answer;
    *output = opt2->answer;

    smooth_flag = (flag1->answer) ? SMOOTH : NO_SMOOTH;
    value_flag = (flag2->answer) ?  CATLABEL : CATNUM;

    error_prefix = argv[0];
    return (0);
}


int read_row(CELL * buf)
{
    if (last_read)
        return (0);
    if (first_read) {
        blank_line(buf);
        first_read = 0;
    } else {
        if (row_count >= n_rows) {
            last_read = 1;
            blank_line(buf);
        } else {
            G_get_c_raster_row(in_file_d, buf + 1, row_count++);
            G_set_c_null_value(buf, 1);
            G_set_c_null_value(buf + row_length + 1, 1);
        }
    }
    return (row_length + 2);
}

static int blank_line(CELL * buf)
{
    G_set_c_null_value(buf, row_length + 2);

    return 0;
}

int open_file(char *cell, char *digit)
{
    /* open cell file */
    if ((mapset = G_find_cell(cell, "")) == NULL) {
	fprintf(stderr, "%s:  open_file:  cell file %s not found\n",
		error_prefix, cell);
	exit(-1);
    }
    sscanf(cell, "%s", cell_name);
    if ((in_file_d = G_open_cell_old(cell_name, mapset)) < 0) {
	fprintf(stderr,
		"%s:  open_file:  could not open cell file %s in %s\n",
		error_prefix, cell_name, mapset);
	exit(-1);
    }
    if (G_get_cellhd(cell_name, mapset, &cell_head) == -1) {
	fprintf(stderr,
		"%s:  open_file:  could not read header for cell file %s in %s\n",
		error_prefix, cell_name, mapset);
	exit(-1);
    }
    G_get_window(&cell_head);
    strcpy(dig_name, digit);

    G__file_name(lab_name, "dig_att", digit, G_mapset());

    G__make_mapset_element("dig_att");
    G__make_mapset_element("dig");
    G__make_mapset_element("cats_dig");

    if (which_outputs & BINARY)
	Vect_open_new(&Map, digit);

    if (which_outputs & LABEL) {
	lab_digit = open_it(lab_name);
    }
    first_read = 1;
    last_read = 0;
    direction = FORWARD;
    row_length = cell_head.cols;
    n_rows = cell_head.rows;
    row_count = 0;
    alloc_bufs(row_length + 2);
    fill_head();

    return 0;
}

static FILE *open_it(char *name)
{
    FILE *file;

    if ((file = fopen(name, "w")) == NULL) {
	fprintf(stderr, "%s:  open_it:  could not open output file %s\n",
		error_prefix, name);
	exit(-1);
    }
    return (file);
}

int close_file(void)
{
    G_close_cell(in_file_d);
    if (which_outputs & LABEL)
	fclose(lab_digit);
    if (which_outputs & BINARY)
	Vect_close(&Map);

    return 0;
}

static int fill_head(void)
{
    /* put some junk into the digit file header */
    strcpy(head.organization, "organization");
    strcpy(head.date, "");
    strcpy(head.your_name, "name");
    strcpy(head.map_name, "mapname");
    strcpy(head.source_date, "");
    strcpy(head.line_3, "");
    head.orig_scale = 24000;
    head.plani_zone = cell_head.zone;
    head.W = cell_head.west;
    head.N = cell_head.north;
    head.E = cell_head.east;
    head.S = cell_head.south;
    head.digit_thresh = 0.04;
    head.map_thresh = 0.04;

    /* 4.0  copy head data to Map.head */
    if (which_outputs & BINARY)
	Vect_copy_head_data(&head, &(Map.head));

    return 0;
}

int show(struct COOR *point)
{
    if (point == NULPTR)
	fprintf(stdout, "pointer is NULL\n");
    else
	fprintf(stdout, "addr = %p fptr = %p bptr = %p (%d,%d,%d,%d,%d)\n",
		point, point->fptr, point->bptr, point->row,
		point->col, point->node, point->left, point->right);
    fflush(stdout);
    return 0;
}

#ifdef DEBUG
char *xmalloc(int size, char *label)
{
    char *addr;

    addr = G_malloc(size);
    fprintf(stdout, "MALLOC:   %8d   %7d          %s\n", addr, size,
	    label);
    return (addr);
}

int xfree(char *addr, char *label)
{
    fprintf(stdout, "FREE:     %8d                %s\n", addr, label);
    G_free(addr);
    return 0;
}

char *xrealloc(char *addr, int size, char *label)
{
    char *addr2, *G_realloc();

    addr2 = G_realloc(addr, size);
    fprintf(stdout, "REALLOC:  %8d   %7d  (%8d)   %s\n", addr2, size, addr,
	    label);
    return (addr2);
}
#endif
