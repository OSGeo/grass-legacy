/* -*-c-basic-offset: 4-*-
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
 *
 * Algorithm was modified by Olga Waupotitsch
 * USA CERL on nov, 1993
 * because the previous implementation was incosistent
 * stopped in the middle of map, because it tried to continue
 * a line wich was presumed to have been started earlier
 * but in fact was not started.
 * also the write_line() complained that the lines end unexpectedly
 *
 * After modification all these problems are gone
 *
 * Modified for the new Grass 5.0 floating point and
 * null values raster file format.
 * Pierre de Mouveaux - 20 april 2000.
 */
#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "extr_lines.h"

static struct line_hdr *v_list;
static struct COOR *h_ptr;
static void *top, *middle, *bottom;
static int tl, tc, tr, ml, mc, mr, bl, bc, br;
static int row, col, n_cols;
char buf[80];
static int read_next(void);
static int nabors(void);
static int update_list(int);
static struct COOR *end_line(struct COOR *,int);
static struct COOR *start_line(int);
struct COOR *get_ptr(void);

extern int data_type;
extern int data_size;

int extract_lines (void)
{
	row = -3;
	read_next();
	read_next();

	switch (data_type) {
		case CELL_TYPE:
		{
			while (read_next()) {
				CELL* m = &((CELL*)middle)[1];
				CELL* t = &((CELL*)top)[1];
				CELL* b = &((CELL*)bottom)[1];
				for (col = 1; col < n_cols - 1; col++,t++,m++,b++) {
					m = &((CELL*)middle)[col];
					t = &((CELL*)top)[col];
					b = &((CELL*)bottom)[col];

					if (mc = !G_is_c_null_value(m)) {
						tl = !G_is_c_null_value(t - 1);
						tc = !G_is_c_null_value(t);
						tr = !G_is_c_null_value(t + 1);
						ml = !G_is_c_null_value(m - 1);
						mr = !G_is_c_null_value(m + 1);
						bl = !G_is_c_null_value(b-1);
						bc = !G_is_c_null_value(b);
						br = !G_is_c_null_value(b+1);
						update_list(nabors());
					}
				}
			}
			break;
		}
		case FCELL_TYPE:
		{
			while (read_next()) {
				FCELL* m = &((FCELL*)middle)[1];
				FCELL* t = &((FCELL*)top)[1];
				FCELL* b = &((FCELL*)bottom)[1];

				for (col = 1; col < n_cols - 1; col++,t++,m++,b++) {
					m = &((FCELL*)middle)[col];
					t = &((FCELL*)top)[col];
					b = &((FCELL*)bottom)[col];

					if (mc = !G_is_f_null_value(m)) {
						tl = !G_is_f_null_value(t - 1);
						tc = !G_is_f_null_value(t);
						tr = !G_is_f_null_value(t + 1);
						ml = !G_is_f_null_value(m - 1);
						mr = !G_is_f_null_value(m + 1);
						bl = !G_is_f_null_value(b-1);
						bc = !G_is_f_null_value(b);
						br = !G_is_f_null_value(b+1);
						update_list(nabors());
					}
				}
			}
			break;
		}
		case DCELL_TYPE:
		{
			while (read_next()) {
				DCELL* m = &((DCELL*)middle)[1];
				DCELL* t = &((DCELL*)top)[1];
				DCELL* b = &((DCELL*)bottom)[1];
				for (col = 1; col < n_cols - 1; col++,t++,m++,b++) {
					m = &((DCELL*)middle)[col];
					t = &((DCELL*)top)[col];
					b = &((DCELL*)bottom)[col];
					if (mc = !G_is_d_null_value(m)) {
						tl = !G_is_d_null_value(t - 1);
						tc = !G_is_d_null_value(t);
						tr = !G_is_d_null_value(t + 1);
						ml = !G_is_d_null_value(m - 1);
						mr = !G_is_d_null_value(m + 1);
						bl = !G_is_d_null_value(b-1);
						bc = !G_is_d_null_value(b);
						br = !G_is_d_null_value(b+1);
						update_list(nabors());
					}
				}
			}			
			break;
		}
	}
	return 0;
}

static int nabors (void)
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

static int update_list(int count)
{
  struct COOR *new_ptr1, *new_ptr2, *new_ptr3;

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

      /* first take care of the cases where both non-zero
	 neighbours are in a upper-left corner (cw from ml to tr) */
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
      else if (tl != 0 && tc != 0)	/* bend (\|) */
	 v_list[col].center = end_line(v_list[col].center,1);
      else if (tr != 0 && tc != 0)   /* bend |/ */
	 v_list[col].center = end_line(v_list[col].center,1);
      else if (tl != 0 && ml != 0)   
	 h_ptr = end_line(h_ptr, 1);

      /* now take care of the cases when non-zero neighbours
      are next to nonzero neighbours in a top-left corner */
      else if (bl != 0 && ml != 0)   
	 v_list[col].center = start_line(1);
      else if (tr != 0 && mr != 0)   
	 h_ptr = start_line(1);

      else if (!((tc != 0 && bc != 0) || (ml != 0 && mr != 0)))
      /* if not horiz or vertical line */
      {
	/* one of the non zero neighbours is in the top left corner,
	   and the other one is one of bl - mr, not next to the first one */
        if (ml || tl || tc || tr)	/* old line bends toward */
        {				/*   new area */
          new_ptr1 = get_ptr();
          if (ml)			/* join new to where came from */
          {
	    if(h_ptr==NULL) fprintf(stderr, "Warning! h_ptr is NULL! ");
	   /* this should never happen by the logic of algorithm */
            extend_line(h_ptr,new_ptr1);
            h_ptr = NULL;
          }
          else if (tl)
          {
	    if(v_list[col].left==NULL) 
		   fprintf(stderr, "Warning! v_list[col].left is NULL!");
	   /* this should never happen by the logic of algorithm */
            extend_line(v_list[col].left,new_ptr1);
            v_list[col].left = NULL;
          }
          else if (tc)
          {
	    if(v_list[col].center==NULL) 
		   fprintf(stderr, "Warning! v_list[col].center is NULL!");
	   /* this should never happen by the logic of algorithm */
            extend_line(v_list[col].center,new_ptr1);
            v_list[col].center = NULL;
          }
          else/* tr */
          {
	    if(v_list[col].right==NULL) 
		   fprintf(stderr, "Warning! v_list[col].right is NULL!");
	   /* this should never happen by the logic of algorithm */
            extend_line(v_list[col].right,new_ptr1);
            v_list[col].right = NULL;
          }
          if (mr)			/* find out where going */
	  /* tr is 0 here */
            h_ptr = new_ptr1;
          else if (br)
            v_list[col + 1].left = new_ptr1;
          else if (bc)
            v_list[col].center = new_ptr1;
          else /* bl, ml is 0 here */
            v_list[col - 1].right = new_ptr1;
        }
        else
        {/* lower-left */	
	  /* if the non-zero neigbours are adjacent */
	  if (mr && br) 
		h_ptr = start_line(1);
	  else if ((br && bc) || (bl && bc)) 
		v_list[col].center = start_line(1);
	  else
	  /* the non-zero neigbours are not adjacent */
          {/* starting in middle of line */
             new_ptr1 = get_ptr();
             new_ptr2 = get_ptr();
             new_ptr3 = get_ptr();
             new_ptr1->fptr = new_ptr2;
             new_ptr1->bptr = new_ptr3;
             new_ptr3->bptr = new_ptr2->bptr = new_ptr1;
	     if (mr && bc) 
	     {
               h_ptr = new_ptr2;
	       v_list[col].center = new_ptr3;
	     }
	     else if(mr && bl)
	     {
               h_ptr = new_ptr2;
	       v_list[col-1].right = new_ptr3;
	     }
	     else if(bl && br)
	     {
	       v_list[col-1].right = new_ptr3;
	       v_list[col+1].left = new_ptr2;
	     }
	   }/* starting in the middle of the line */

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
      if(bl)
          v_list[col - 1].right = start_line(1);
      break;
    case 5:
       /* fprintf(stderr,"crowded cell %xH (%d,%d) -continuing\n",count,row,col);*/
       /* I think 5 neighbours is nor crowded, so we shouldn't worry the user
	   Olga */
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
    case 6:
	/* the same as case 5 */
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

  return 0;
}

static struct COOR *end_line(struct COOR *ptr,int node)
{
  ptr->row = row;
  ptr->col = col - 1;
  ptr->node = node;
  ptr->fptr = ptr;
  write_line(ptr);
  return(NULL);
}

static struct COOR *start_line(int node)
{
  struct COOR *new_ptr1, *new_ptr2;

  new_ptr1 = get_ptr();
  new_ptr2 = get_ptr();
  new_ptr1->bptr = new_ptr1;
  new_ptr1->fptr = new_ptr2;
  new_ptr1->node = node;
  new_ptr2->bptr = new_ptr1;
  return(new_ptr2);
}

int join_lines (struct COOR *p, struct COOR *q)
{
  p->row = row;
  p->col = col - 1;
  if (p->fptr != NULL)
  {
    G_warning("join_lines: p front pointer not NULL!");
    /* exit(-1);*/
  }
  p->fptr = q->bptr;
  if (q->fptr != NULL)
  {
    G_warning("join_lines: q front pointer not NULL!");
    /* exit(-1);*/
  }
  if (q->bptr->fptr == q)
    q->bptr->fptr = p;
  else
    q->bptr->bptr = p;
  xfree(q,"join_lines, q");
  write_line(p);

  return 0;
}

int extend_line (struct COOR *p, struct COOR *q)
{
  while (p==NULL)
  {
     fprintf(stderr,"WARING! extend line:  p is NULL\n");
     /* should never happen by the logic of algorithm */
     fflush(stderr);
      p = start_line(1);
  }
  p->row = row;
  p->col = col - 1;
  if (p->fptr != NULL)
  {
    G_warning("extend_lines: p front pointer not NULL!");
    /* should never happen by the logic of algorithm */
    /* exit(-1);*/
  }
  p->fptr = q;
  if (q->bptr != NULL)
  {
    G_warning("extend_lines: q back pointer not NULL!");
    /* should never happen by the logic of algorithm */
    /* exit(-1);*/
  }
  q->bptr = p;

  return 0;
}

int stop_line (struct COOR *p, struct COOR *q)
{
  p->row = row;
  p->col = col - 1;
  if (p->fptr != NULL)
  {
    G_warning("stop_line: p front pointer not NULL!");
    /* should never happen by the logic of algorithm */
    /* exit(-1);*/
  }
  p->fptr = q;
  if (q->bptr != NULL)
  {
    G_warning("stop_line: q back pointer not NULL!");
    /* should never happen by the logic of algorithm */
    /* exit(-1);*/
  }
  q->bptr = p;
  q->fptr = q;

  return 0;
}

struct COOR *get_ptr (void)
{
  struct COOR *p;

  p = (struct COOR *) xmalloc(sizeof(struct COOR),"get_ptr, p");
  p->row = row;
  p->col = col - 1;
  p->node = 0;
  p->bptr = p->fptr = NULL;
  return(p);
}

int alloc_bufs(int size)
{
  int i;

  top = (void *) xmalloc(size * data_size,"alloc_bufs, top");
  middle = (void *) xmalloc(size * data_size,"alloc_bufs, middle");
  bottom = (void *) xmalloc(size * data_size,"alloc_bufs, bottom");
  v_list = (struct line_hdr *) xmalloc(size * sizeof(struct line_hdr),"alloc_bufs, v_list");
  for (i = 0; i < size; i++)
    v_list[i].left = v_list[i].center = v_list[i].right = NULL;
  n_cols = size;

  return 0;
}

static int read_next (void)
{
  void *p;

  row++;
  p = top;
  top = middle;
  middle = bottom;
  bottom = p;
  return(read_row(bottom));
}
