/* Cell-file area extraction */
/*   Input/output and line tracing routines */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* November 1987 - January 1988 */

/* input is a cell file found in the normal GRASS way */
/* outputs are ASCII and binary digit files and a supplemental area file */
/* to be used to improve the dlg labelling process */

/* Global variables: */
/*    direction     indicates whether we should use fptr or bptr to */
/*                  move to the "next" point on the line */
/*    first_read    flag to indicate that we haven't read from input */
/*                  file yet */
/*    last_read     flag to indicate we have reached EOF on input */
/*    in_file_d     input cell file descriptor */
/*    bin_digit     output digit file */
/*    ascii_digit   output ascii digit file */
/*    area_digit    output supplemental file which gives categories to */
/*                  the left and right of each line;  this info is written */
/*                  in the same order as the lines in the digit files */
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
/*    write_area    make table of area mappings and write dlg label file */
/*    read_row      read another row of data--handles putting a "no data" */
/*                  boundary around the edges of the file */
/*    syntax        check syntax of command line and compile which_outputs */
/*    open_file     open input and output files */
/*    close_file    close input and output files */
/*    show          debugging routine to print out everything imaginable */
/*                  about a COOR structure */

#include "gis.h"
#include "dig_head.h"
#include "mode.h"
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

static struct Cell_head cell_head;
static int which_outputs;
static char *error_prefix;
static int direction;
static int first_read, last_read;
static char cell_name[256], area_name[256], ascii_name[256], tmp_name[256];
static char lab_name[256], bin_name[256];
static FILE *area_digit, *lab_digit, *bin_digit, *ascii_digit;
static FILE *tmp_digit;
static int in_file_d;
static int row_length, row_count, n_rows, total_areas;
static int *equivs;
static struct area_table *areas;
static int smooth_flag;

/* write_line - attempt to write a line to output */
/* just returns if line is not completed yet */

write_line(seed)
    struct COOR *seed;
{
  struct COOR *point, *begin, *end, *find_end(), *move();
  struct COOR *last;
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
  else					/* in middle of a line */
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
  if (smooth_flag == SMOOTH)
    write_smooth_ln(begin, end, n);
  else
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
  double *yarray, *yp;
  struct COOR *p, *last;
  int i, type;
  char *G_malloc();

  n++;					/* %% 6.4.88 */
  type = AREA;
#ifdef ASCII
  if (which_outputs & ASCII)
    fprintf(ascii_digit,"A  %d\n",n);	/* %% 6.4.88 */
#endif
  if (which_outputs & BINARY)
  {
    fwrite(&type,sizeof(type),1,bin_digit);
    fwrite(&n,sizeof(n),1,bin_digit);
  }
  p = begin;
  yarray = yp = (double *) G_malloc(n * sizeof(double));
  *yp = cell_head.north - (double) p->row * cell_head.ns_res;
  x = cell_head.west + (double) p->col * cell_head.ew_res;
#ifdef ASCII
  if (which_outputs & ASCII)
    fprintf(ascii_digit," %12.2lf %12.2lf\n",*yp,x);
#endif
#ifdef WRITE_AREAS
  if (which_outputs & AREAS)
    fprintf(tmp_digit,"%3d  %12.2lf  %12.2lf",n,*yp,x);
#endif
  if (which_outputs & BINARY)
    fwrite(&x,sizeof(double),1,bin_digit);
  for (i = 1; i < n; i++)
  {
    last = p;
    if ((p = move(p)) == NULPTR)	/* this should NEVER happen */
    {
      fprintf(stderr,"%s:  write_line:  line terminated unexpectedly\n",error_prefix);
      fprintf(stderr,"  previous (%d) point %x (%d,%d,%d) %x %x\n",direction,last,last->row,last->col,last->node,last->fptr,last->bptr);
      exit(-1);
    }
    *++yp = cell_head.north - p->row * cell_head.ns_res;
    x = cell_head.west + p->col * cell_head.ew_res;
#ifdef ASCII
    if (which_outputs & ASCII)
      fprintf(ascii_digit," %12.2lf %12.2lf\n",*yp,x);
#endif
    if (which_outputs & BINARY)
      fwrite(&x,sizeof(double),1,bin_digit);
    free(last);
  }
#ifdef WRITE_AREAS
  if (which_outputs & AREAS)
  {
    if (direction == FORWARD)
      fprintf(tmp_digit,"  %12.2lf  %12.2lf  %3d  %3d\n",*yp,x,p->left,p->right);
    else
      fprintf(tmp_digit,"  %12.2lf  %12.2lf  %3d  %3d\n",*yp,x,p->right,p->left);
  }
#endif
  free(p);
  if (which_outputs & BINARY)
  {
    yp = yarray;
    for (i = 0; i < n; i++)
    {
      fwrite(yp++,sizeof(double),1,bin_digit);
    }
  }
  free(yarray);
}


/* write_smooth_ln - actual writing part of write_line for smoothed lines */
/* writes binary and ASCII digit files and supplemental file */
#define SNAP_THRESH 0.00001

static int 
write_smooth_ln(begin,end,n)
    struct COOR *begin, *end;		/* start and end point of line */
    int n;				/* number of points to write */
{
  double dx, dy;
  int idx, idy;
  double *yarray, *yp;
  double *xarray, *xp;
  struct COOR *p, *last;
  int i, total, type;
  char *G_malloc();

  n++;					/* %% 6.4.88 */
  type = AREA;
  p = begin;
  /* allocate the arrays and get the first point */
  yarray = yp = (double *) G_malloc(2*n * sizeof(double));
  *yp = cell_head.north - (double) p->row * cell_head.ns_res;
  xarray = xp = (double *) G_malloc(2*n * sizeof(double));
  *xp = cell_head.west + (double) p->col * cell_head.ew_res;

  /* generate the list of smoothed points, may be duplicate points */
  total = 1;
  for (i = 1; i < n; i++) {
    last = p;
    if ((p = move(p)) == NULPTR)	/* this should NEVER happen */
      {
	fprintf(stderr,"%s:  write_line:  line terminated unexpectedly\n",error_prefix);
	fprintf(stderr,"  previous (%d) point %x (%d,%d,%d) %x %x\n",direction,last,last->row,last->col,last->node,last->fptr,last->bptr);
	exit(-1);
      }
    
    idy = (p->row - last->row);
    idx = (p->col - last->col);
    dy = (idy>0) ? 0.5 : ((idy<0) ? -0.5 : 0.0); /* dy = 0.0, 0.5, or -0.5 */
    dx = (idx>0) ? 0.5 : ((idx<0) ? -0.5 : 0.0); /* dx = 0.0, 0.5, or -0.5 */
    *++yp = cell_head.north - (last->row + dy) * cell_head.ns_res;
    *++xp = cell_head.west + (last->col + dx) * cell_head.ew_res;
    total++;
    *++yp = cell_head.north - (p->row - dy) * cell_head.ns_res;
    *++xp = cell_head.west + (p->col - dx) * cell_head.ew_res;
    total++;

    free(last);
  } /* end of for i */
  *++yp = cell_head.north - (double) p->row * cell_head.ns_res;
  *++xp = cell_head.west + (double) p->col * cell_head.ew_res;
  total++;

  /* strip out the duplicate points from the list */
  n = 1;
  xp = xarray+1; yp = yarray+1;
  for (i=1; i<total; i++) {
    if ((fabs(*xp - *(xp-1)) < SNAP_THRESH) &&
	(fabs(*yp - *(yp-1)) < SNAP_THRESH)) {
      xp++;
      yp++;
    }
    else {
      xarray[n] = *xp++;
      yarray[n] = *yp++;
      n++;
    }
  }

  /* write files */
#ifdef ASCII
  if (which_outputs & ASCII)
    fprintf(ascii_digit,"A  %d\n",n);	/* %% 6.4.88 */
#endif
  if (which_outputs & BINARY)
  {
    fwrite(&type,sizeof(type),1,bin_digit);
    fwrite(&n,sizeof(n),1,bin_digit);
  }

  yp=yarray; xp=xarray;
#ifdef ASCII
  if (which_outputs & ASCII)
    fprintf(ascii_digit," %12.2lf %12.2lf\n",*yp,*xp);
#endif
#ifdef WRITE_AREAS
  if (which_outputs & AREAS)
    fprintf(tmp_digit,"%3d  %12.2lf  %12.2lf",n,*yp,*xp);
#endif


  /* write x array and for ASCII both x and y */
  for (i=0; i < n; xp++, yp++, i++) {
    if (which_outputs & BINARY)
      fwrite(xp,sizeof(double),1,bin_digit);

#ifdef ASCII
    if (which_outputs & ASCII)
      fprintf(ascii_digit," %12.2lf %12.2lf\n",*yp,*xp);
#endif
  }

#ifdef WRITE_AREAS
  xp--; yp--; /* back up one to the last coordinate */
  if (which_outputs & AREAS) {
    if (direction == FORWARD)
      fprintf(tmp_digit,"  %12.2lf  %12.2lf  %3d  %3d\n",*yp,*xp,p->left,p->right);
    else
      fprintf(tmp_digit,"  %12.2lf  %12.2lf  %3d  %3d\n",*yp,*xp,p->right,p->left);
  }
#endif
  free(xarray);
  free(p);

  /* write y array */
  if (which_outputs & BINARY) {
    yp = yarray;
    for (i = 0; i < n; i++) {
      fwrite(yp++,sizeof(double),1,bin_digit);
    }
  }
  free(yarray);
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

re_map_areas()
{
  int n, a, width;
  CELL left, right, cat;
  double x1, y1, x2, y2;

  if (!(which_outputs & AREAS))
    return(0);
#ifdef WRITE_AREAS
  fclose(tmp_digit);
  if ((tmp_digit = fopen(tmp_name,"r")) == NULL)
  {
    fprintf(stderr,"%s:  Could not re-open temporary area file %s\n",error_prefix,tmp_name);
    exit(-1);
  }
  while (fscanf(tmp_digit,"%d %lf %lf %lf %lf %d %d",&n,&y1,&x1,&y2,&x2,&left,&right) != EOF)
    fprintf(area_digit,"%3d  %12.2lf  %12.2lf  %12.2lf  %12.2lf  %3d  %3d\n",n,y1,x1,y2,x2,equivs[left],equivs[right]);
#endif
}

/* write_area - make table of area equivalences and write dlg label file */

write_area(a_list,e_list,n_areas,n_equiv)
    struct area_table *a_list;		/* list of areas */
    struct equiv_table *e_list;		/* list of equivalences between areas */
    int n_equiv, n_areas;		/* lengths of e_list, a_list */
{
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
  if (which_outputs & LABEL)
  {
    for (i = 0, p = a_list; i < n_areas; i++, p++)
    {
      if (!(e_list + i)->mapped && p->width > 0)
      {
        fprintf(lab_digit,"A    %7.2lf  %7.2lf     %3d \n",
	    cell_head.west + (p->col + (p->width/2.0)) * cell_head.ew_res, 
	    cell_head.north - (p->row + 0.5) * cell_head.ns_res,
	    p->cat);
      }
    }
  }
}

/* syntax - check syntax of command line and compile which_outputs to tell */
/* which output files the user wants generated; returns -1 on error, zero */
/* otherwise; default output files are binary digit and dlg label; anything */
/* other than default produces only the specific files requested */

syntax(argc,argv,input,output)
    int argc;
    char *argv[];
    char **input, **output;
{
  int i;
  char *p;

#ifdef OPTS
  if (argc < 2)
#else
  if (argc != 3)	/* no options in current version */
#endif
    return(-1);
  *input = *output = NULL;
  if (argc == 3)			/* must be file names only */
  {
    if (*argv[1] == '-' || *argv[2] == '-')
      return(-1);
    which_outputs = BINARY | LABEL;
    *input = argv[1];
    *output = argv[2];
    smooth_flag = SMOOTH;
  }
  else					/* file names and options */
  {
    for (i = 1; i < argc; i++)		/* process each arg */
    {
      if (*argv[i] == '-')		/* option */
      {
        for (p = argv[i] + 1; *p; p++)	/* process each char in option string */
        {
          switch (*p)
          {
            case 'a':
              which_outputs |= ASCII;
              break;
            case 'A':
              which_outputs |= AREAS;
              break;
            case 'b':
              which_outputs |= BINARY;
              break;
            case 'l':
              which_outputs |= LABEL;
              break;
            default:
              return(-1);
          }				/* switch */
        }				/* for on p */
      }
      else				/* not an option */
      {
        if (*input != NULL)		/* already have input file */
        {
          if (*output != NULL)		/* already have output file, too */
            return(-1);
          else
            *output = argv[i];
        }
        else
          *input = argv[i];
      }
    }					/* for on arg list */
    if (*output == NULL)		/* no output file found */
      return(-1);
  }
  error_prefix = argv[0];
  return(0);
}

read_row(buf)
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
  if ((in_file_d = G_open_cell_old(cell_name,mapset)) < 0)
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
  /* open digit, supplemental area and dlg label files */
  for (p = digit; *p != '\0' && *p != ' '; p++)
  { }
  *p = '\0';
  sprintf(bin_name,"%s/%s/%s",G_gisdbase(),G_location(),G_mapset());
  strcpy(lab_name,bin_name);		/* name for dlg label file */
  strcpy(tmp_name,bin_name);		/* name for temporary area file */
  strcpy(area_name,bin_name);		/* name for supplementary file */
  strcpy(ascii_name,bin_name);		/* name for ascii digit file */
  strcat(bin_name,"/dig/");
  strcat(lab_name,"/dig_att/");
  strcat(tmp_name,"/dig/");
  strcat(area_name,"/dig/");
  strcat(ascii_name,"/dig/");
  strcat(bin_name,digit);
  strcat(lab_name,digit);
  strcat(tmp_name,digit);
  strcat(area_name,digit);
  strcat(ascii_name,digit);
  strcat(tmp_name,".tmp");
  strcat(area_name,".area");
  strcat(ascii_name,".ascii");
  G__make_mapset_element("dig_att");
  G__make_mapset_element("dig");
  if (which_outputs & BINARY)
    bin_digit = open_it(bin_name);
#ifdef ASCII
  if (which_outputs & ASCII)
    ascii_digit = open_it(ascii_name);
#endif
  if (which_outputs & LABEL)
    lab_digit = open_it(lab_name);
#ifdef WRITE_AREAS
  if (which_outputs & AREAS)
  {
    tmp_digit = open_it(tmp_name);
    area_digit = open_it(area_name);
  }
#endif
  first_read = 1;
  last_read = 0;
  direction = FORWARD;
  row_length = cell_head.cols;
  n_rows = cell_head.rows;
  row_count = 0;
  alloc_bufs(row_length + 2);
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

close_file()
{
  G_close_cell(in_file_d);
#ifdef WRITE_AREAS
  if (which_outputs & AREAS)
  {
    fclose(area_digit);
    fclose(tmp_digit);
    unlink(tmp_name);
  }
#endif
  if (which_outputs & LABEL)
    fclose(lab_digit);
#ifdef ASCII
  if (which_outputs & ASCII)
    fclose(ascii_digit);
#endif
  if (which_outputs & BINARY)
    fclose(bin_digit);
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
  /* write digit file header into binary digit file */
  if (which_outputs & BINARY)
  {
    dig_write_head_binary (bin_digit, &head);
  }
#ifdef ASCII
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
#endif ASCII
}

show(point)
struct COOR *point;
{
  if (point == NULPTR)
    fprintf(stdout,"pointer is NULL\n");
  else
    fprintf(stdout,"addr = %x fptr = %x bptr = %x (%d,%d,%d,%d,%d)\n",point,point->fptr,point->bptr,point->row,point->col,point->node,point->left,point->right);
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
