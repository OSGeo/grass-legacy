 
/* ExtractProfile.c 
 *
 * function defined:
 *
 * ExtractProfile(profile,name,mapset)
 *
 *  struct Profile *profile;	- pointer in initialized profile struct 
 *  char   *name,               - cell-file map name
 *         *mapset;             - mapset name
 *
 * PURPOSE: To extract a profile from a cell-file.  A profile is a list
 * of all the cell-values along a given line drawn across a cell-file.
 * The number of cell-values is determined by the end-points of the line
 * and the current window definition.
 *
 * NOTES:
 *
 * 1) Assumes that profile is a pointer to a profile structure that has
 *    been initialized through a call to the routine InitProfile.  The 
 *    profile structure is defined in profile.h.
 *
 * 2) ExtractProfile returns 0 upon successful completion, or a negative
 *    value if an error is encountered:
 *
 *       -1   profile end-point outside the current window
 *       -2   unable to open cell-file
 *       -3   error reading from cell-file
 *       -4   profile's window does not match current window
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include <limits.h>
#include "profile.h"

int 
ExtractProfile (struct Profile *profile, char *name, char *mapset)
{
int    fd;               /* cell-file desciptor */
struct Cell_head window; /* current GIS window */
CELL   *buf;             /* storage for one cell-file row */
CELL   theCell;          /* storage of the current cell of interest - for NULL fix */
struct ProfileNode *ptr = NULL; 
int    stop;
int    row1, col1;    
int    row2, col2;    
int    row, col;         /* keep track of profile line position */ 
float  slope;            /* for profile line equation */
int    incr;

G_get_set_window(&window);

/* make sure profile`s window matches currently set window */
if (profile->window.north != window.north  ||
   profile->window.south  != window.south  ||
   profile->window.east   != window.east   ||
   profile->window.west   != window.west   ||
   profile->window.ew_res != window.ew_res ||
   profile->window.ns_res != window.ns_res)
   return(-4); 

/* make sure profile's end-points are not outside the current window */
if (profile->n1 > window.north || profile->n1 < window.south ||
   profile->e1 > window.east  || profile->e1 < window.west  ||
   profile->n2 > window.north || profile->n2 < window.south ||
   profile->e2 > window.east  || profile->e2 < window.west)
   return(-1);

/* figure row and column coords. of end-points */
row1 = (profile->window.north - profile->n1) / profile->window.ns_res ;
col1 = (profile->e1 - profile->window.west) / profile->window.ew_res;
row2 = (profile->window.north - profile->n2) / profile->window.ns_res ;
col2 = (profile->e2 - profile->window.west) / profile->window.ew_res;

#ifdef DEBUG
fprintf (stdout,"ExtractProfile: from (%d,%d) to ",row1,col1);
fprintf (stdout,"(%d,%d)\n"                       ,row2,col2);
#endif
 
/* open the cel file, return -2 if problem */
fd = G_open_cell_old(name,mapset);
if (fd < 0)
   {
   fprintf(stderr,"warning: unable to open [%s] in [%s]\n",name,mapset);
   return(-2);
   }
buf = G_allocate_cell_buf(); 

/*
 * this section loops through a line between (row1,col1) and (row2,col2)
 * adding category values along line to linked-list that represents
 * the profile
 *
 * two cases:
 * 1) line crosses more rows than columns.
 * 2) line crosses more columns than rows.
 *
 */

/* CASE 1: line crosses more rows than columns. */
if ( (row1 != row2) && (abs(row1-row2) > abs(col1-col2)) )
   {
   if (row2 < row1)
      incr = -1;      /* line goes "up" */
   else
      incr = 1;       /* line goes "down" */

   if (row1==row2)
      slope= (float)0.0;      /* line is horizontal */
   else
      slope  = (float)((float)(col2-col1)/(float)(row2-row1));

#  ifdef DEBUG
   fprintf (stdout,"CASE 1: slope=%f\n",slope);
#  endif

   /* LOOP through profile line by rows */
   stop = row2+incr;
   for (row=row1; row!=stop; row+=incr)
      {
      profile->count++;
      col = (int)(slope*(float)(row-row1) + col1);

      if (G_get_map_row(fd,buf,row) < 0)
         return(-3);

#     ifdef DEBUG
      fprintf (stdout,"[(%d,%d):%d] ",row,col,buf[col]);
#     endif
      /* Test if NULL, if so set to the minimum integer value */
      if (G_is_c_null_value(&(buf[col])))
	      theCell = INT_MIN;
      else
	      theCell = buf[col];
		      
      /* set mins and maxes */
      if (row==row1)
         {
         profile->MaxCat = theCell;
         profile->MinCat = (theCell == INT_MIN) ? INT_MAX : theCell ;
         }
      else
         {
         if (theCell > profile->MaxCat)
            profile->MaxCat = theCell;
         if (theCell < profile->MinCat && theCell != INT_MIN)
            profile->MinCat = theCell;
         }

      /* add to linked list */
      if (profile->ptr == NULL) /* add to linked list */
         {
         /* first in list */
         profile->ptr = (struct ProfileNode *)
                        G_malloc(sizeof(struct ProfileNode));
         profile->ptr->cat = theCell;
         profile->ptr->next = NULL;
         ptr = profile->ptr;
         }
      else
         {
         /* not first in list */
         ptr->next = (struct ProfileNode *)
                     G_malloc(sizeof(struct ProfileNode));
         ptr->next->cat = theCell;
         ptr->next->next = NULL;
         ptr = ptr->next; 
         } 
      }
   }

/* CASE 2: line crosses more columns than rows */
else 
   {
   if (col2 < col1)
      incr = -1;      /* line goes "left" */
   else
      incr = 1;       /* line goes "right" */

   if (col1==col2)
      slope= (float)0.0;      /* line is vertical */
   else
      slope  = (float)((float)(row2-row1)/(float)(col2-col1));

#  ifdef DEBUG
   fprintf (stdout,"CASE 2: slope=%f\n",slope);
#  endif

   /* LOOP through profile line by columns */
   stop = col2+incr;
   for (col=col1; col!=stop; col+=incr)
      {
      profile->count++;
      row = (int)(slope*(float)(col-col1)+row1);

      if (G_get_map_row(fd,buf,row) < 0)
         return(-3);

#     ifdef DEBUG
      fprintf (stdout,"[(%d,%d):%d] ",row,col,buf[col]);
#     endif
      /* Test if NULL, if so set to the minimum integer value */
      if (G_is_c_null_value(&(buf[col])))
	      theCell = INT_MIN;
      else
	      theCell = buf[col];
	
      /* set mins and maxes */
      if (col==col1)
         {
         profile->MaxCat = theCell;
         profile->MinCat = (theCell == INT_MIN) ? INT_MAX : theCell;
         }
      else
         {
         if (theCell > profile->MaxCat)
            profile->MaxCat = theCell;
         if (theCell < profile->MinCat && theCell != INT_MIN)
            profile->MinCat = theCell;
         }

      /* add to linked list */
      if (profile->ptr == NULL)
         {
         /* first in list */
         profile->ptr = (struct ProfileNode *)
                        G_malloc(sizeof(struct ProfileNode));
         profile->ptr->cat = theCell;
         profile->ptr->next = NULL;
         ptr = profile->ptr;
         }
      else
         {
         /* not first in list */
         ptr->next = (struct ProfileNode *)
                     G_malloc(sizeof(struct ProfileNode));
         ptr->next->cat = theCell;
         ptr->next->next = NULL;
         ptr = ptr->next; 
         } 
      }
   }
#ifdef DEBUG
fprintf (stdout,"\n");
#endif

G_unopen_cell(fd);
return(0);
}
