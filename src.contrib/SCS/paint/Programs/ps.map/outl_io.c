
#include "gis.h"
#include "Vect.h"
#include "extr_areas.h"
#include <math.h>

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

static int blank_line ();

static struct Map_info Map;
static struct line_pnts *Points;

static struct Cell_head cell_head;
static int which_outputs;
static char *error_prefix;
static int direction;
static int first_read, last_read;
static char cell_name[256];
static char lab_name[256];
static FILE *area_digit, *lab_digit, *ascii_digit;
static FILE *tmp_digit;
static int in_file_d;
static int row_length, row_count, n_rows, total_areas;
static int *equivs;
static struct area_table *areas;
static struct dig_head head ;


o_io_init ()
{
    Points = Vect_new_line_struct ();
    error_prefix = "ps.map";
}

/* write_line - attempt to write a line to output */
/* just returns if line is not completed yet */

o_write_line(seed)
struct COOR *seed;
{
    struct COOR *point, *begin, *end, *find_end(), *move();
    int dir, line_type, n, n1;

    point = seed;
    if (dir = at_end(point))		/* already have one end of line */
    {
	begin = point;
	end = find_end(point,dir,&line_type,&n);
	if (line_type == OPEN)
	    return(-1);			/* unfinished line */
	direction = dir;
    }
    else	/* in middle of a line */
    {
	end = find_end(point,FORWARD,&line_type,&n);
	if (line_type == OPEN)		/* line not finished */
	    return(-1);
	if (line_type == END)		/* found one end at least */
	{					/* look for other one */
	    begin = find_end(point,BACKWARD,&line_type,&n1);
	    if (line_type == OPEN)		/* line not finished */
		return(-1);
	    if (line_type == LOOP)		/* this should NEVER be the case */
	    {
		fprintf(stderr,"%s:  o_write_line:  found half a loop!\n",error_prefix);
		return(-1);
	    }
	    direction = at_end(begin);	/* found both ends now; total length */
	    n += n1;				/*   is sum of distances to each end */
	}
	else	/* line_type = LOOP by default */
	{					/* already have correct length */
	    begin = end;			/* end and beginning are the same */
	    direction = FORWARD;		/* direction is arbitrary */
	}
    }
    write_ln(begin,end,n);
    return(0);
}


/* write_ln - actual writing part of write_line */
/* writes binary and ASCII digit files and supplemental file */

static int 
write_ln(begin,end,n)
struct COOR *begin, *end;		/* start and end point of line */
int n;				/* number of points to write */
{
    double x;
    double y;
    struct COOR *p, *last;
    int i, type;
    char *G_malloc();

    Vect_reset_line (Points);

    n++;					/* %% 6.4.88 */
    type = AREA;

    p = begin;
    y = cell_head.north - (double) p->row * cell_head.ns_res;
    x = cell_head.west  + (double) p->col * cell_head.ew_res;

    Vect_append_point (Points, x, y);

    for (i = 1; i < n; i++)
    {
	last = p;
	if ((p = move(p)) == NULPTR)	/* this should NEVER happen */
	{
	    fprintf(stderr,"%s:  o_write_line:  line terminated unexpectedly\n",
		error_prefix);
	    fprintf(stderr,"  previous (%d) point %x (%d,%d,%d) %x %x\n",
	    direction,last,last->row,last->col,last->node,
	    last->fptr,last->bptr);

	    exit(-1);
	}
	y = cell_head.north - p->row * cell_head.ns_res;
	x = cell_head.west  + p->col * cell_head.ew_res;

	Vect_append_point (Points, x, y);

	free(last);
    }
    free(p);

    Vect_write_line (&Map, type, Points);
}



/* move - move to next point in line */

static struct COOR *
move(point)
    struct COOR *point;
{
    if (direction == FORWARD)
    {
	if (point->fptr == NULPTR)		/* at open end of line */
	    return(NULPTR);
	if (point->fptr->fptr == point)	/* direction change coming up */
	    direction = BACKWARD;
	return(point->fptr);
    }
    else
    {
	if (point->bptr == NULPTR)
	    return(NULPTR);
	if (point->bptr->bptr == point)
	    direction = FORWARD;
	return(point->bptr);
    }
}

/* find_end - search for end of line, starting at a given point and */
/* moving in a given direction */

static struct COOR *
find_end (seed,dir,result,n)
struct COOR *seed;
int dir, *result, *n;
{
    struct COOR *start;

    start = seed;
    direction = dir;
    *result = *n = 0;
    while (!*result)
    {
	seed = move(seed);
	(*n)++;
	if (seed == start)
	    *result = LOOP;
	else
	{
	    if (seed == NULPTR)
		*result = OPEN;
	    else
	    {
		if (at_end(seed))
		    *result = END;
	    }
	}
    }
    return(seed);
}

/* at_end - test whether a point is at the end of a line;  if so, give */
/* the direction in which to move to go away from that end */

static int 
at_end (ptr)
struct COOR *ptr;
{
    if (ptr->fptr == ptr)
	return(BACKWARD);
    if (ptr->bptr == ptr)
	return(FORWARD);
    return(0);
}

/* re_map_areas - read back temporary area file and re-map the area */
/* information contained in it; write the results to the final area */
/* file */

o_re_map_areas()
{
    int n, a, width;
    CELL left, right, cat;
    double x1, y1, x2, y2;

    if (!(which_outputs & AREAS))
	return(0);
}

/* write_area - make table of area equivalences and write attribute file */

o_write_area(a_list,e_list,n_areas,n_equiv)
    struct area_table *a_list;		/* list of areas */
    struct equiv_table *e_list;		/* list of equivalences between areas */
    int n_equiv, n_areas;		/* lengths of e_list, a_list */
{
    char type;
    int n, i;
    struct area_table *p;
    char *G_malloc();

    total_areas = 0;
    if (n_equiv < n_areas)
    {
	equivs = (int *) G_malloc(n_areas * sizeof(int));
	n = n_equiv;
    }
    else
    {
	equivs = (int *) G_malloc(n_equiv * sizeof(int));
	n = n_areas;
    }
    for (i = 0; i < n; i++)
    {
	if ((e_list + i)->mapped)
	equivs[i] = (e_list + i)->where;
	else
	{
	    total_areas++;
	    equivs[i] = i;
	}
    }
    if (n < n_areas)
    {
	for (i = n; i < n_areas; i++)
	{
	    total_areas++;
	    equivs[i] = i;
	}
    }
    type = AREA;
}




o_read_row (buf)
    CELL *buf;
{
    int i;

    if (last_read)
	return(0);
    if (first_read)
    {
	blank_line(buf);
	first_read = 0;
    }
    else
    {
	if (row_count >= n_rows)
	{
	    last_read = 1;
	    blank_line(buf);
	}
	else
	{
	    G_get_map_row(in_file_d,buf + 1,row_count++);
	    *buf = *(buf + row_length + 1) = 0;
	}
    }
    return(row_length + 2);
}

static int 
blank_line (buf)
    CELL *buf;
{
    int i;

    for (i = 0; i < row_length + 2; i++)
	*(buf + i) = 0;
}

o_open_file (cell,digit)
char *cell, *digit;
{
    FILE *open_it();
    char *mapset, *p;

    which_outputs = BINARY;

    /* open cell file */
    if ((mapset = G_find_cell(cell,"")) == NULL)
    {
	fprintf(stderr,"%s:  o_open_file:  cell file %s not found\n",error_prefix,cell);
	exit(-1);
    }
    sscanf(cell,"%s",cell_name);
    if ((in_file_d = G_open_cell_old(cell_name,mapset)) < 0)
    {
	fprintf(stderr,"%s:  o_open_file:  could not open cell file %s in %s\n",error_prefix,cell_name,mapset);
	exit(-1);
    }
    if (G_get_cellhd(cell_name,mapset,&cell_head) == -1)
    {
	fprintf(stderr,"%s:  o_open_file:  could not read header for cell file %s in %s\n",error_prefix,cell_name,mapset);
	exit(-1);
    }
    G_set_window(&cell_head);

    G__file_name (lab_name, "dig_att", digit, G_mapset());

    G__make_mapset_element("dig_att");
    G__make_mapset_element("dig");

    Vect_open_new (&Map, digit);

    first_read = 1;
    last_read = 0;
    direction = FORWARD;
    row_length = cell_head.cols;
    n_rows = cell_head.rows;
    row_count = 0;
    o_alloc_bufs(row_length + 2);
    fill_head();
}

static FILE *
open_it(name)
char *name;
{
    FILE *file;

    if ((file = fopen(name,"w")) == NULL)
    {
	fprintf(stderr,"%s:  open_it:  could not open output file %s\n",error_prefix,name);
	exit(-1);
    }
    return(file);
}

o_close_file()
{
    G_close_cell(in_file_d);
    Vect_close (&Map);
}

static int 
fill_head()
{
    /* put some junk into the digit file header */
    strcpy(head.organization,"organization");
    strcpy(head.date,"");
    strcpy(head.your_name,"name");
    strcpy(head.map_name,"mapname");
    strcpy(head.source_date,"");
    strcpy(head.line_3,"");
    head.orig_scale = 24000;
    head.plani_zone = cell_head.zone;
    head.W = cell_head.west;
    head.N = cell_head.north;
    head.E = cell_head.east;
    head.S = cell_head.south;
    head.digit_thresh = 0.04;
    head.map_thresh = 0.04;

    /* 4.0  copy head data to Map.head*/
    Vect_copy_head_data (&head, &(Map.head));

}

show(point)
struct COOR *point;
{
    if (point == NULPTR)
	fprintf(stdout,"pointer is NULL\n");
    else
    fprintf (stdout,"addr = %x fptr = %x bptr = %x (%d,%d,%d,%d,%d)\n",
	point,point->fptr,point->bptr,point->row,
	point->col,point->node,point->left,point->right);
    fflush(stdout);
}

#ifdef DEBUG
char *
xmalloc(size,label)
int size;
char *label;
{
    char *addr, *G_malloc();

    addr = G_malloc(size);
    fprintf(stdout,"MALLOC:   %8d   %7d          %s\n",addr,size,label);
    return(addr);
}

xfree(addr,label)
char *addr, *label;
{
    fprintf(stdout,"FREE:     %8d                %s\n",addr,label);
    free(addr);
}

char *
xrealloc(addr,size,label)
char *addr, *label;
int size;
{
    char *addr2, *G_realloc();

    addr2 = G_realloc(addr,size);
    fprintf(stdout,"REALLOC:  %8d   %7d  (%8d)   %s\n",addr2,size,addr,label);
    return(addr2);
}
#endif
