/* Cell-file line extraction */
/*   Input/output and line tracing routines */

/* Mike Baba */
/* DBA Systems*/
/* Farfax, VA */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* March 1988 */

/* input is a GRASS cell file */
/* output is a binary or ascii digit file */

/* Global variables: */
/*    direction     indicates whether we should use fptr or bptr to */
/*                  move to the "next" point on the line */
/*    first_read    flag to indicate that we haven't read from input */
/*                  file yet */
/*    last_read     flag to indicate we have reached EOF on input */
/*    input_fd      input cell file descriptor */
/*    bin_digit     output digit file */
/*    ascii_digit   output ascii digit file */
/*    row_length    length of each row of the cell file (i.e., number of */
/*                  columns) */
/*    n_rows        number of rows in the cell file */
/*    row_count     number of the row just read in--used to prevent reading */
/*                  beyond end of the cell file */
/*    which_outputs which output files are to be generated */
/*    edge_type     LINE edge or AREA edge */
/*    error_prefix  our name as found from the argument list */

/* Entry points: */
/*    write_line    write a line out to the digit files */
/*    read_row      read another row of data--handles putting a "no data" */
/*                  boundary around the edges of the file */
/*    syntax        check syntax of command line and compile which_outputs */
/*    open_file     open input and output files */
/*    close_file    close input and output files */
/*    show          debugging routine to print out everything imaginable */
/*                  about a COOR structure */

#include <stdio.h>
#include <sys/wait.h>
#include "gis.h"
#include "head.h"
#include "mode.h"
#include "extr_lines.h"

#define BACKWARD 1
#define FORWARD 2
#define OPEN 1
#define END 2
#define LOOP 3
#define BINARY 1
#define ASCII 2
#define LINE_EDGE 1
#define AREA_EDGE 2

extern FILE *debug, *mem;

static struct Cell_head cell_head;
static int which_outputs;
static int edge_type;
static char *error_prefix;
static int direction;
static int first_read, last_read;
static char cell_name[256], ascii_name[256], bin_name[256];
static FILE *bin_digit, *ascii_digit;
static int input_fd;
static int row_length, row_count, n_rows;

/* write_line - attempt to write a line to output */
/* just returns if line is not completed yet */

write_line(seed)
struct COOR *seed;
{
  struct COOR *point, *begin, *end, *find_end(), *move();
  int dir, line_type, n, n1;

  fprintf(debug,"    write_line, seed:\n      ");
  show(seed);
  point = seed;
  if (dir = at_end(point))		/* already have one end of line */
  {
    begin = point;
    end = find_end(point,dir,&line_type,&n);
    if (line_type == OPEN)
    {
      fprintf(debug,"    found unfinished line, returning from write_line\n");
      return(-1);			/* unfinished line */
    }
    direction = dir;
  }
  else					/* in middle of a line */
  {
    end = find_end(point,FORWARD,&line_type,&n);
    if (line_type == OPEN)		/* line not finished */
    {
      fprintf(debug,"    found unfinished line, returning from write_line\n");
      return(-1);
    }
    if (line_type == END)		/* found one end at least */
    {					/* look for other one */
      begin = find_end(point,BACKWARD,&line_type,&n1);
      if (line_type == OPEN)		/* line not finished */
      {
        fprintf(debug,"    found unfinished line, returning from write_line\n");
        return(-1);
      }
      if (line_type == LOOP)		/* this should NEVER be the case */
      {
        fprintf(stderr,"%s:  write_line:  found half a loop!\n",error_prefix);
        return(-1);
      }
      direction = at_end(begin);	/* found both ends now; total length */
      n += n1;				/*   is sum of distances to each end */
    }
    else				/* line_type = LOOP by default */
    {					/* already have correct length */
      begin = end;			/* end and beginning are the same */
      direction = FORWARD;		/* direction is arbitrary */
    }
  }
  /* if (n > 2) */
  write_ln(begin,end,n);
  fprintf(debug,"    returning from write_line\n");
  return(0);
}

/* write_ln - actual writing part of write_line */
/* writes binary and ASCII digit files and supplemental file */

static int write_ln(begin,end,n)
struct COOR *begin, *end;		/* start and end point of line */
int n;					/* number of points to write */
{
  double x;
  double *yarray, *yp;
  struct COOR *p, *last;
  int i, type;
  char *xmalloc();

  fprintf(debug,"      write_ln, n = %d, begin:\n        ",n);
  show(begin);
  fprintf(debug,"      end:\n        ");
  show(end);
  if (edge_type == LINE_EDGE)
     type = LINE;
  if (edge_type == AREA_EDGE) 
     type = AREA;
  ++n;
  if (which_outputs & ASCII)
  { if (edge_type == LINE_EDGE)
       fprintf(ascii_digit,"L  %d\n",n);
     else  
       fprintf(ascii_digit,"A  %d\n",n);
  }
  if (which_outputs & BINARY)
  {
    /* fprintf (stderr,"TYPE = %d, 0-line 1-area \n",type); */
    fwrite(&type,sizeof(type),1,bin_digit);
    fwrite(&n,sizeof(n),1,bin_digit);
  }
  p = begin;
  yarray = yp = (double *) xmalloc(n * sizeof(double),"write_ln, yarray");
  fprintf(debug,"      allocated yarray from %x to %x\n",yarray,yarray + (n+1) );
  *yp = cell_head.north - ((double) p->row + 0.5) * cell_head.ns_res;
  x = cell_head.west + ((double) p->col + 0.5) * cell_head.ew_res;
  if (which_outputs & ASCII)
    fprintf(ascii_digit," %12.2lf %12.2lf\n",*yp,x);
  if (which_outputs & BINARY)
    fwrite(&x,sizeof(double),1,bin_digit);
  for (i = 1; i < n; i++)
  {
    last = p;
    if ((p = move(p)) == NULL)		/* this should NEVER happen */
    {
      fprintf(stderr,"%s:  write_line:  line terminated unexpectedly\n",error_prefix);
      fprintf(stderr,"  previous (%d) point %x (%d,%d,%d) %x %x\n",direction,last,last->row,last->col,last->node,last->fptr,last->bptr);
      exit(-1);
    }
    /*
    fprintf(debug,"        step = %d, direction = %d, ptr:\n          ",i,direction);
    show(p);
    */
    *++yp = cell_head.north - ((double) p->row + 0.5) * cell_head.ns_res;
    x = cell_head.west + ((double) p->col + 0.5) * cell_head.ew_res;
    if (which_outputs & ASCII)
      fprintf(ascii_digit," %12.2lf %12.2lf\n",*yp,x);
    if (which_outputs & BINARY)
      fwrite(&x,sizeof(double),1,bin_digit);
    xfree(last,"write_ln, last");
  }
  xfree(p,"write_ln, p");
  if (which_outputs & BINARY)
  {
    yp = yarray;
    for (i = 0; i < n; i++)
    {
      fwrite(yp++,sizeof(double),1,bin_digit);
    }
  }
  xfree(yarray,"write_ln, yarray");
  fprintf(debug,"      returning from write_ln\n");
}

/* move - move to next point in line */

static struct COOR *move(point)
struct COOR *point;
{
  fprintf(debug,"          move, direction = %d, input point:\n            ",direction);
  show(point);
  if (direction == FORWARD)
  {
    if (point->fptr == NULL)		/* at open end of line */
      return(NULL);
    if (point->fptr->fptr == point)	/* direction change coming up */
      direction = BACKWARD;
    fprintf(debug,"          returning point:\n            ");
    show(point->fptr);
    return(point->fptr);
  }
  else
  {
    if (point->bptr == NULL)
      return(NULL);
    if (point->bptr->bptr == point)
      direction = FORWARD;
    fprintf(debug,"          returning point:\n            ");
    show(point->bptr);
    return(point->bptr);
  }
}

/* find_end - search for end of line, starting at a given point and */
/* moving in a given direction */

static struct COOR *find_end(seed,dir,result,n)
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
      if (seed == NULL)
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

static int at_end(ptr)
struct COOR *ptr;
{
  if (ptr->fptr == ptr)
    return(BACKWARD);
  if (ptr->bptr == ptr)
    return(FORWARD);
  return(0);
}

/* syntax - check syntax of command line and compile which_outputs to tell */
/* which output files the user wants generated; returns -1 on error, zero */
/* otherwise; default output files are binary digit and dlg label; 
/* default line type is lines (not area edges), anything */
/* other than default produces only the specific files requested */

syntax(argc,argv,input,output)
int argc;
char *argv[];
char *input, *output;
{
    int i;
    char in[40], out[40], type[40], format[40];
    static cf_flag = 0;
    static vf_flag = 0;
    static fm_flag = 0;
    static lt_flag = 0;

    which_outputs = BINARY;
    edge_type = LINE_EDGE;

    /* must have input and output */
    if (argc < 3) usage(argv[0]);
 
    /*  argv[1] = input cell file */
	if (sscanf (argv[1], "%s", in) == 1)
	{
	    if (cf_flag++) usage(argv[0]);
            strcpy(input, in); 
	}
    /*  argv[2] = output vector file */
	if (sscanf (argv[2], "%s", out) == 1)
	{
	    if (vf_flag++) usage(argv[0]);
            strcpy(output, out); 
	}

    for (i = 3; i < argc; i++)
    {
	if (sscanf (argv[i], "format=%s", format) == 1)
	{
	    if (fm_flag++) usage(argv[0]);
            if (strncmp(format,"binary",6) == 0) which_outputs = BINARY;
            if (strncmp(format,"BINARY",6) == 0) which_outputs = BINARY;
            if (strncmp(format,"Binary",6) == 0) which_outputs = BINARY;
            else if (strncmp(format,"ascii",5) == 0)  which_outputs = ASCII;
            else usage (argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "attrib=%s", type) == 1)
	{
	    if (lt_flag++) usage(argv[0]);
            if (strncmp(type,"line",4) == 0) edge_type = LINE_EDGE;
            else if (strncmp(type,"LINE",4) == 0) edge_type = LINE_EDGE;
            else if (strncmp(type,"area",4) == 0)  edge_type = AREA_EDGE;
            else if (strncmp(type,"AREA",4) == 0)  edge_type = AREA_EDGE;
            else usage (argv[0]);
	    continue;
	}
	usage(argv[0]);
    }
    if (!((cf_flag == 1) && (vf_flag == 1) && (fm_flag <= 1)&&(lt_flag <= 1))) 
	usage(argv[0]);

    error_prefix = argv[0];
    return(0);
}

usage(me)
{
    fprintf (stderr, "%s cell_file vector_file [format=binary|ascii] [attrib=line|area] \n", me);
    exit(1);
}


read_row(buf)
CELL *buf;
{
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
      G_get_map_row(input_fd,buf + 1,row_count++);
      *buf = *(buf + row_length + 1) = 0;
    }
  }
  return(row_length + 2);
}

static int blank_line(buf)
CELL *buf;
{
  int i;

  for (i = 0; i < row_length + 2; i++)
    *(buf + i) = 0;
}

open_file(cell,digit)
char *cell, *digit;
{
  FILE *open_it();
  char *mapset, *p;

  /* open cell file */
  if ((mapset = G_find_cell(cell,"")) == NULL)
  {
    fprintf(stderr,"%s:  open_file:  cell file %s not found\n",error_prefix,cell);
    exit(-1);
  }
  sscanf(cell,"%s",cell_name);
  if ((input_fd = G_open_cell_old(cell_name,mapset)) < 0)
  {
    fprintf(stderr,"%s:  open_file:  could not open cell file %s in %s\n",error_prefix,cell_name,mapset);
    exit(-1);
  }
  if (G_get_cellhd(cell_name,mapset,&cell_head) == -1)
  {
    fprintf(stderr,"%s:  open_file:  could not read header for cell file %s in %s\n",error_prefix,cell_name,mapset);
    exit(-1);
  }
  G_set_window(&cell_head);
  /* open digit file */
  for (p = digit; *p != '\0' && *p != ' '; p++)
  { }
  *p = '\0';
  if (which_outputs & BINARY)
  {
    sprintf(bin_name,"%s/%s/%s/dig/%s",G_gisdbase(),G_location(),G_mapset(),digit);
    G__make_mapset_element("dig");
    bin_digit = open_it(bin_name);
  }
  if (which_outputs & ASCII)
  {
    sprintf(ascii_name,"%s/%s/%s/dig_ascii/%s",G_gisdbase(),G_location(),G_mapset(),digit);
    G__make_mapset_element("dig_ascii");
    ascii_digit = open_it(ascii_name);
  }
  first_read = 1;
  last_read = 0;
  direction = FORWARD;
  row_length = cell_head.cols;
  n_rows = cell_head.rows;
  row_count = 0;
  alloc_bufs(row_length + 2);
  fill_head();
}

static FILE *open_it(name)
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

close_file()
{
  G_close_cell(input_fd);
  if (which_outputs & ASCII)
    fclose(ascii_digit);
  if (which_outputs & BINARY)
    fclose(bin_digit);
}

static int fill_head()
{
  /* put some junk into the digit file header */
  strcpy(head.organization,"organization");
  strcpy(head.date,"1 JAN 1990");
  strcpy(head.your_name,"name");
  strcpy(head.map_name,"mapname");
  strcpy(head.source_date,"23 Nov 87");
  strcpy(head.line_3,"     ");
  head.orig_scale = 24000;
  head.plani_zone = cell_head.zone;
  head.W = cell_head.west;
  head.N = cell_head.north;
  head.E = cell_head.east;
  head.S = cell_head.south;
  head.digit_thresh = 0.05;
  head.map_thresh = 0.05;
  /* write digit file header into binary digit file */
  if (which_outputs & BINARY)
  {
    fseek(bin_digit,0L,0);
    fwrite(head.organization,sizeof(head.organization),1,bin_digit);
    fwrite(head.date,sizeof(head.date),1,bin_digit);
    fwrite(head.your_name,sizeof(head.your_name),1,bin_digit);
    fwrite(head.map_name,sizeof(head.map_name),1,bin_digit);
    fwrite(head.source_date,sizeof(head.source_date),1,bin_digit);
    fwrite(head.line_3,sizeof(head.line_3),1,bin_digit);
    fwrite(&head.orig_scale,sizeof(head.orig_scale),1,bin_digit);
    fwrite(&head.plani_zone,sizeof(head.plani_zone),1,bin_digit);
    fwrite(&head.W,sizeof(head.W),1,bin_digit);
    fwrite(&head.E,sizeof(head.E),1,bin_digit);
    fwrite(&head.S,sizeof(head.S),1,bin_digit);
    fwrite(&head.N,sizeof(head.N),1,bin_digit);
    fwrite(&head.map_thresh,sizeof(head.map_thresh),1,bin_digit);
  }
  /* write digit file header into ascii digit file */
  if (which_outputs & ASCII)
  {
    fprintf(ascii_digit,"ORGANIZATION: %s\n",head.organization);
    fprintf(ascii_digit,"DIGIT DATE:   %s\n",head.date);
    fprintf(ascii_digit,"DIGIT NAME:   %s\n",head.your_name);
    fprintf(ascii_digit,"MAP NAME:     %s\n",head.map_name);
    fprintf(ascii_digit,"MAP DATE:     %s\n",head.source_date);
    fprintf(ascii_digit,"MAP SCALE:    %d\n",head.orig_scale);
    fprintf(ascii_digit,"OTHER INFO:   %s\n",head.line_3);
    fprintf(ascii_digit,"UTM ZONE:     %d\n",head.plani_zone);
    fprintf(ascii_digit,"WEST EDGE:    %12.2lf\n",head.W);
    fprintf(ascii_digit,"EAST EDGE:    %12.2lf\n",head.E);
    fprintf(ascii_digit,"SOUTH EDGE:   %12.2lf\n",head.S);
    fprintf(ascii_digit,"NORTH EDGE:   %12.2lf\n",head.N);
    fprintf(ascii_digit,"MAP THRESH:   %12.2lf\n",head.map_thresh);
    fprintf(ascii_digit,"VERTI:\n") ;
  }
}

show(point)
struct COOR *point;
{
  if (point == NULL)
    fprintf(debug,"pointer is NULL\n");
  else
    fprintf(debug,"addr = %x fptr = %x bptr = %x (%d,%d,%d)\n",point,point->fptr,point->bptr,point->row,point->col,point->node);
}

char *xmalloc(size,label)
int size;
char *label;
{
  char *addr, *G_malloc();

  addr = G_malloc(size);
  fprintf(mem,"M:  %8x - %8x (%6d)                 %s\n",addr,addr+size-1,size,label);
  return(addr);
}

xfree(addr,label)
char *addr, *label;
{
  fprintf(mem,"F:  %8x                                     %s\n",addr,label);
  free(addr);
}

char *xrealloc(addr,size,label)
char *addr, *label;
int size;
{
  char *addr2, *G_realloc();

  addr2 = G_realloc(addr,size);
  fprintf(mem,"R:  %8x - %8x (%6d) (from %8x) %s\n",addr2,addr2+size-1,size,addr,label);
  return(addr2);
}






