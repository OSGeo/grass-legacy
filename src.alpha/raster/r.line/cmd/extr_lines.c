/*
 * Cell-file line extraction
 *   Line-tracing algorithm
 *
 * Mike Baba 
 * DBA Systems
 * Farfax, VA 
 * Jan 1990 
 *
 * Jean Ezell
 * US Army Corps of Engineers
 * Construction Engineering Research Lab
 * Modeling and Simulation Team
 * Champaign, IL  61820
 * March 1988
 */

#include <stdio.h>
#include "gis.h"
#include "extr_lines.h"

static struct line_hdr *v_list;
static struct COOR *h_ptr;
static CELL *top, *middle, *bottom;
static CELL tl, tc, tr, ml, mc, mr, bl, bc, br;
static int row, col, n_cols;
char buf[80];

extract_lines()
{
  row = -3;
  read_next();
  read_next();
  while (read_next())
  {
    for (col = 1; col < n_cols - 1; col++)
    {
      if (mc = middle[col])
      {
        tl = top[col - 1];
        tc = top[col];
        tr = top[col + 1];
        ml = middle[col - 1];
        mr = middle[col + 1];
        bl = bottom[col - 1];
        bc = bottom[col];
        br = bottom[col + 1];
        update_list(nabors());
      }
    }
  }
}

static nabors()
{
  int count;

  count = 0;
  if (tl)
    count++;
  if (tc)
    count++;
  if (tr)
    count++;
  if (mr)
    count++;
  if (br)
    count++;
  if (bc)
    count++;
  if (bl)
    count++;
  if (ml)
    count++;
  return(count);
}

static update_list(count)
int count;
{
  struct COOR *new_ptr1, *new_ptr2, *new_ptr3;
  struct COOR *get_ptr(), *start_line(), *end_line();

  switch(count)
  {
    case 0:
      fprintf(stderr,"update_list:  isolated cell (%d,%d)\n",row,col);
      break;
    case 1:				/* begin or end line */
      if (ml)
        h_ptr = end_line(h_ptr,0);
      if (tl)
        v_list[col].left = end_line(v_list[col].left,0);
      if (tc)
        v_list[col].center = end_line(v_list[col].center,0);
      if (tr)
        v_list[col].right = end_line(v_list[col].right,0);
      if (mr)
        h_ptr = start_line(0);
      if (br)
        v_list[col + 1].left = start_line(0);
      if (bc)
        v_list[col].center = start_line(0);
      if (bl)
        v_list[col - 1].right = start_line(0);
      break;
    case 2:				/* straight or bent line */
      if (tl != 0 && br != 0)		/* slanted line (\) */
      {
        v_list[col + 1].left = v_list[col].left;
        v_list[col].left = NULL;
      }
      else if (tr != 0 && bl != 0)	/* slanted line (/) */
      {
        v_list[col - 1].right = v_list[col].right;
        v_list[col].right = NULL;
      }
      else if (ml != 0 && tc != 0)	/* bend (_|) */
      {
        join_lines(h_ptr,v_list[col].center);
        h_ptr = v_list[col].center = NULL;
      }
      else if (ml != 0 && tr != 0)	/* bend (_/) */
      {
        join_lines(h_ptr,v_list[col].right);
        h_ptr = v_list[col].left = NULL;
      }
      else if (tl != 0 && tr != 0)	/* bend (\/) */
      {
        join_lines(v_list[col].left,v_list[col].right);
        v_list[col].left = v_list[col].right = NULL;
      }
      else if (!((tc != 0 && bc != 0) || (ml != 0 && mr != 0)))
      {
        if (ml || tl || tc || tr)	/* old line bends toward */
        {				/*   new area */
          new_ptr1 = get_ptr();
          if (ml)			/* join new to where came from */
          {
            extend_line(h_ptr,new_ptr1);
            h_ptr = NULL;
          }
          else if (tl)
          {
            extend_line(v_list[col].left,new_ptr1);
            v_list[col].left = NULL;
          }
          else if (tc)
          {
            extend_line(v_list[col].center,new_ptr1);
            v_list[col].center = NULL;
          }
          else
          {
            extend_line(v_list[col].right,new_ptr1);
            v_list[col].right = NULL;
          }
          if (mr)			/* find out where going */
            h_ptr = new_ptr1;
          else if (br)
            v_list[col + 1].left = new_ptr1;
          else if (bc)
            v_list[col].center = new_ptr1;
          else
            v_list[col - 1].right = new_ptr1;
        }
        else
        {				/* starting in middle of line */
          new_ptr1 = get_ptr();
          new_ptr2 = get_ptr();
          new_ptr3 = get_ptr();
          new_ptr1->fptr = new_ptr2;
          new_ptr1->bptr = new_ptr3;
          new_ptr3->bptr = new_ptr2->bptr = new_ptr1;
          if (mr)
            h_ptr = new_ptr2;
          else
            v_list[col + 1].left = new_ptr2;
          if (bc)
            v_list[col].center = new_ptr3;
          else
            v_list[col - 1].right = new_ptr3;
        }
      }
      break;
    case 3:	
        if (ml || tl || tc || (tr && !mr))
        {
          if (ml)                         /* stop horz. and vert. lines */
               h_ptr = end_line(h_ptr,1);
          if (tc)
            v_list[col].center = end_line(v_list[col].center,1);          

                                       /* stop diag lines if no horz,vert */
          if ((tl) && (!ml) && (!tc))
             v_list[col].left = end_line(v_list[col].left,1);
          if ((tr) && (!mr) && (!tc))
             v_list[col].right = end_line(v_list[col].right,1); 
        } 
        if (mr)                       /* start horz. and vert */
           h_ptr = start_line(1);
        if (bc)
          v_list[col].center = start_line(1);

                                      /* start diag if no horz,vert */
        if ((br) && (!mr) && (!bc))
           v_list[col + 1].left = start_line(1);
        if ((bl) && (!ml) && (!bc))
           v_list[col - 1].right = start_line(1);
       break;
    case 4:
        if (ml)                         /* end horz. and vert lines */
          h_ptr = end_line(h_ptr,1);
        if (tc)
          v_list[col].center = end_line(v_list[col].center,1);
                           
                                       /* end diag lines only if no horz,vert*/
        if ((tl) && (!ml) && (!tc))
          v_list[col].left = end_line(v_list[col].left,1);
        if ((tr) && (!mr) && (!tc))
          v_list[col].right = end_line(v_list[col].right,1);

        if (mr)                       /* start horz. and vert */
          h_ptr = start_line(1);
        if (bc)
          v_list[col].center = start_line(1);
                                      /* start diag if no horz,vert */
        if ((br) && (!mr) && (!bc))
          v_list[col + 1].left = start_line(1);
        if ((bl) && (!ml) && (!bc))
          v_list[col - 1].right = start_line(1);
      break;
    case 5:
        fprintf(stderr,"crowded cell %xH (%d,%d) -continuing\n",count,row,col);
        if (ml)                         /* end horz. and vert lines */
          h_ptr = end_line(h_ptr,1);
        if (tc)
          v_list[col].center = end_line(v_list[col].center,1);
                           
                                       /* end diag lines only if no horz,vert*/
        if ((tl) && (!ml) && (!tc))
          v_list[col].left = end_line(v_list[col].left,1);
        if ((tr) && (!mr) && (!tc))
          v_list[col].right = end_line(v_list[col].right,1);

        if (mr)                       /* start horz. and vert */
          h_ptr = start_line(1);
        if (bc)
          v_list[col].center = start_line(1);
                                      /* start diag if no horz,vert */
        if ((br) && (!mr) && (!bc))
          v_list[col + 1].left = start_line(1);
        if ((bl) && (!ml) && (!bc))
          v_list[col - 1].right = start_line(1);
      break;
    default:
      fprintf(stderr,"update_list:  crowded cell %xH (%d,%d)\n",count,row,col);
      sprintf (buf,"cell file is not thinned properly. \nPlease run r.thin\n");
      G_fatal_error (buf);
      exit (-1);

  }					/* switch count */
}

static struct COOR *end_line(ptr,node)
struct COOR *ptr;
int node;
{
  ptr->row = row;
  ptr->col = col - 1;
  ptr->node = node;
  ptr->fptr = ptr;
  write_line(ptr);
  return(NULL);
}

static struct COOR *start_line(node)
int node;
{
  struct COOR *new_ptr1, *new_ptr2, *get_ptr();

  new_ptr1 = get_ptr();
  new_ptr2 = get_ptr();
  new_ptr1->bptr = new_ptr1;
  new_ptr1->fptr = new_ptr2;
  new_ptr1->node = node;
  new_ptr2->bptr = new_ptr1;
  return(new_ptr2);
}

join_lines(p,q)
struct COOR *p, *q;
{
  p->row = row;
  p->col = col - 1;
  if (p->fptr != NULL)
  {
    exit(-1);
  }
  p->fptr = q->bptr;
  if (q->fptr != NULL)
  {
    exit(-1);
  }
  if (q->bptr->fptr == q)
    q->bptr->fptr = p;
  else
    q->bptr->bptr = p;
  xfree(q,"join_lines, q");
  write_line(p);
}

extend_line(p,q)
struct COOR *p, *q;
{
  p->row = row;
  p->col = col - 1;
  if (p->fptr != NULL)
  {
    exit(-1);
  }
  p->fptr = q;
  if (q->bptr != NULL)
  {
    exit(-1);
  }
  q->bptr = p;
}

stop_line(p,q)
struct COOR *p, *q;
{
  p->row = row;
  p->col = col - 1;
  if (p->fptr != NULL)
  {
    exit(-1);
  }
  p->fptr = q;
  if (q->bptr != NULL)
  {
    exit(-1);
  }
  q->bptr = p;
  q->fptr = q;
}

struct COOR *get_ptr()
{
  char *xmalloc();
  struct COOR *p;

  p = (struct COOR *) xmalloc(sizeof(struct COOR),"get_ptr, p");
  p->row = row;
  p->col = col - 1;
  p->node = 0;
  p->bptr = p->fptr = NULL;
  return(p);
}

alloc_bufs(size)
int size;
{
  int i;
  char *xmalloc();

  top = (CELL *) xmalloc(size * sizeof(CELL),"alloc_bufs, top");
  middle = (CELL *) xmalloc(size * sizeof(CELL),"alloc_bufs, middle");
  bottom = (CELL *) xmalloc(size * sizeof(CELL),"alloc_bufs, bottom");
  v_list = (struct line_hdr *) xmalloc(size * sizeof(struct line_hdr),"alloc_bufs, v_list");
  for (i = 0; i < size; i++)
    v_list[i].left = v_list[i].center = v_list[i].right = NULL;
  n_cols = size;
}

static read_next()
{
  CELL *p;

  row++;
  p = top;
  top = middle;
  middle = bottom;
  bottom = p;
  return(read_row(bottom));
}



