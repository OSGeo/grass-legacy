				/********************************/
				/*	r.le.dist/trace.c	*/
				/*				*/
				/*		2.1		*/
				/*				*/
				/*       07/15/94 version	*/
				/*				*/
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/

#include "r.le.dist.h"

extern struct CHOICE 	*choice;
extern struct REGLIST	*reglist;
extern int   	     	finput;
int    			total_patches=0;
PATCH 			*patch_list = NULLPTR;

				/* DRIVER FOR CELL CLIPPING, TRACING,
				   AND CALCULATIONS */


void  cell_clip_drv(col0, row0, ncols, nrows, value, index)
int   row0, col0, nrows, ncols, index;
int  **value;
{
  CELL		**buf, **pat, *row_buf;
  int 		i, j, fd, p;        
  PATCH  	*list_head;

  total_patches=0;

				/* dynamically allocate storage for the
				   buffer that will hold the contents of
				   the window */

  buf = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
  for(i = 0; i < nrows + 3; i++)
      buf[i] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));

				/* dynamically allocate storage for the
				   buffer that will hold the map of the
				   patch numbers */

  if (choice->patchmap) {
     pat = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
     for(i = 0; i < nrows + 3; i++)
         pat[i] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));
  }

				/* clip out the sampling area */

  cell_clip(buf, row0, col0, nrows, ncols, index);

				/* trace the patches in the sampling area */

  trace(nrows, ncols, buf, pat);

				/* if a map of patches or a map of core
				   areas was requested */

  if (choice->patchmap) {
     fd = G_open_cell_new("num");
     for (i = 1; i < nrows + 1; i++) {
	row_buf = G_allocate_cell_buf();
	G_zero_cell_buf(row_buf);
        for (j = 1; j < ncols + 1; j++)
	   *(row_buf + j - 1) = *(*(pat + i) + j);
        G_put_map_row(fd,row_buf);
     }
  }


/*  printf("\nPatch list after tracing\n");
  list_head = patch_list;
  while(list_head) {
     printf("num=%d att=%d pts=%f crow=%3.0f ccol=%3.0f n=%d s=%d e=%d w=%d\n",list_head->num, list_head->att,
             list_head->npts, list_head->c_row, list_head->c_col, list_head->n,list_head->s,list_head->e,list_head->w);
     for (i=0; i<list_head->npts; i++) {
        printf("   row[%d]=%d col[%d]=%d\n",i,*(list_head->row + i),i,*(list_head->col + i));
     }

     list_head = list_head->next;
  }
*/
				/* if the moving window has been selected
				   call the moving window driver program */

  if(choice->wrum == 'm') {
     if (is_not_empty_buffer(buf,nrows+3,ncols+3)) {
	if(center_is_not_zero(buf,nrows,ncols)) {
           mv_dist(patch_list, value, index);
	}
        else {
	   for (p = 0; p < 6; p++)
	      *(*(value + index) + p) = 0;
	}
     }
     else {
	for (p = 0; p < 6; p++)
	   *(*(value + index) + p) = 0;
     }
  }

				/* otherwise call the other driver program */

  else   
     if (is_not_empty_buffer(buf,nrows+3,ncols+3))
        df_dist(patch_list);

				/* free memory allocated for window buffer */
 
  for(i = 0; i < nrows + 3; i++) 
     free(buf[i]);
  free(buf);

				/* free memory allocated for num map */

  if (choice->patchmap) {
     for(i=0; i<nrows+3; i++) {
        free(pat[i]);
     }
     free(pat);
  }

				/* free memory allocated for patch list */
  
  while(patch_list) {
     list_head = patch_list;
     patch_list = patch_list->next;
     free(list_head->col);
     free(list_head->row);
     free(list_head);
  }

				/* close the num file and release the
				   memory allocated for it */

  if (choice->patchmap) {
     G_close_cell(fd);
     free(row_buf);
  }

}




				/* CHECK BUFFER; RETURN 1 IF BUFFER
				   IS NOT EMPTY, 0 IF EMPTY */

int is_not_empty_buffer(buf,rows,cols)

int **buf, rows, cols ;

{
 
  register int i, j ;

  for (i = 0; i < rows; i++)
      for (j = 0; j < cols; j++) 
          if(buf[i][j]) return (1) ;
  return(0) ;

}




				/* CHECK TO SEE IF THE CENTER PIXEL
				   IN THE BUFFER IS ZERO.  RETURN 1
				   IF IT IS, 0 IF NOT */

int center_is_not_zero(buf,rows,cols)

int **buf, rows, cols ;

{
 
  register int i, j ;

  if(buf[(rows/2) + 1][(cols/2) + 1]) 
     return (1) ;
  return (0) ;

}






				/* OPEN THE RASTER FILE TO BE CLIPPED,
				   AND DO THE CLIPPING */

void  cell_clip(buf, row0, col0, nrows, ncols, index)
int   **buf, row0, col0, nrows, ncols, index;
{
  CELL          *tmp, *tmp1;
  int           fr, ftmp;
  register int  i, j;

  if(choice->wrum == 'r'){
     if(0 > (fr = G_open_cell_old(choice->reg, G_mapset()))) {
        printf("\n");
        printf("   *******************************************************\n");
        printf("    You specified sam=r to request sampling by region,    \n");
        printf("    but the region map specified with the 'reg=' parameter\n");
        printf("    cannot be found in the current mapset.                \n");
        printf("   *******************************************************\n");
        exit(1);
     } 
     tmp1 =  G_allocate_cell_buf();
     G_zero_cell_buf(tmp1);
     printf("Analyzing region number %d...\n", index);
  }

  tmp =  G_allocate_cell_buf();

				/* for each row */

  for(i = row0; i < row0 + nrows; i++){

  				/* if region, read in the corresponding
				   map row in the region file */

    if(choice->wrum == 'r')
        G_get_map_row_nomask(fr, tmp1, i);

				/* allocate memory for 1 row, then 
				   initialize each element of the 
				   row to 0 */
 
    G_zero_cell_buf(tmp);

				/* read row i of the map into tmp */

    G_get_map_row(finput, tmp, i); 

				/* for all the columns one by one */

    for(j = col0; j < col0 + ncols; j++) {

				/* if the choice is not "by region" or
				   if this column is in region "att" */

	if(choice->wrum != 'r' || *(tmp1+j) == index) {

				/* copy the contents of tmp into the
				   appropriate cell in buf */

	  *(*(buf+i+1-row0)+j+1-col0) = *(tmp+j);

	}

    }
  }

  free(tmp);
  if(choice->wrum == 'r'){
     free(tmp1);
     G_close_cell(fr);
  }
}






				/* DRIVER TO LOOK FOR NEW PATCHES, CALL
				   THE TRACING ROUTINE, AND ADD NEW PATCHES
				   TO THE PATCH LIST */

void  trace(nrows, ncols, buf, pat)
int   nrows, ncols, **buf, **pat;
{
  int           class, x, y; 
  double        r, rc;
  register int  i, j;
  PATCH     	*tmp, *find_any, *list_head;


				/* go thru buf, which contains the entries 
				   within the clipped window, column by
				   column and row by row; the contents of 
				   buf[i][j] are changed to a negative value
				   by the save_pt routine after a pt has
				   been traced */
  
  i=0;
  while(i++ < nrows) {							/*1*/
     j=0;
     while(j++ < ncols) {						/*2*/
 
 				/* if this pt contains a positive value, it 
				   may be the start of an untraced patch */

       if((class = *(*(buf+i)+j)) > 0 ) { 				/*3*/

				/* trace the patch from the current pt */

          list_head = patch_list ; 
	  if(find_any = get_bd(i, j, nrows, ncols, class, buf, list_head,
	         pat)){							/*4*/ 

 				/* if the first patch, make tmp point to
				   the patch list and add the first patch
				   to the list */

 	     if(total_patches == 0) {                
                patch_list = find_any;
		tmp = patch_list;
		tmp->next = NULLPTR;
	     }
				/* add the next patch to the patch list */

             else {
                tmp->next = find_any; 	     	 
	        tmp = tmp->next;
		tmp->next = NULLPTR;
	     }

				/* increment the count of total patches */

	     total_patches ++;
       	      
	  }								/*4*/

				/* if i and j are now at or outside the
				   limits of the window, then quit */

    	  if(i >= nrows && j >= ncols) {
		return;
	  }
       }								/*3*/

  				/* if this pt is the boundary point of an 
				   already traced patch or is outside the
				   window, do not start tracing; skip to 
				   next pt */

       else if((class = *(*(buf+i)+j)) <= 0) { 				/*5*/    
				/* if i and j are now at or outside the
				   limits of the window, then quit */

    	  if(i >= nrows && j >= ncols)
             return;
       }								/*5*/
    }									/*2*/
  } 									/*1*/
}






				/* TRACE THE BOUNDARY OF A PATCH, AND
				   SAVE THE PATCH CHARACTERISTICS IN
				   THE PATCH STRUCTURE */				   

PATCH *get_bd(row0, col0, nrows, ncols, class, buf, p_list, pat)
int   row0, col0, class, nrows, ncols;
CELL  **buf, **pat;
PATCH *p_list; 
{  
  int           i = row0, j = col0, pts=0, di=0, dj=-1, 
                not_done, m, roww=0, rowe=0, row1, col1,
		di2, dj2, p, q; 
  PATCH         *patch;
  CELL		**patchmap;
  PT		*ptrfirst, *ptrthis, *ptrnew, *ptrfree;

				/* allocate memory for 1 patch to be
				   saved in the patch data structure */

  patch = (PATCH *)G_calloc(1, sizeof(PATCH));

				/* allocate memory for patchmap, which
				   will hold the boundaries found in
				   the buf array */

  patchmap = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
  for (m = 0; m < nrows + 3; m++)
     patchmap[m] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));

				/* print on the screen a message indicating
				   that tracing has reached a certain patch */

  if (choice->wrum != 'm') {
     fprintf(stdout,"Tracing patch %7d\r",total_patches+1);
     fflush(stdout);
  }

				/* this loop goes until the patch has been 
				   traced */
  for(;;){								/*1*/


				/* STEP 1: RECORD ATTRIBUTE AND PATCH NUMBER,
				   THEN TRACE THE PTS IN THE BOUNDARY, 
				   RECORDING THE ROW AND COL OF EACH PT, AND
				   FINDING THE PATCH BOUNDING BOX */

				/* initialize variables */

     not_done = 1;
     patch->s = 0;
     patch->e = 0;
     patch->w = BIG;
     patch->n = BIG;

				/* while tracing is not done */

     while (not_done) {							/*2*/

/*printf("class=%d buf[%d][%d]=%d row0=%d col0=%d pts=%d\n",class,i,j,*(*(buf + i) + j),row0,col0,pts);
*/
				/* if this is the first pt in the patch,
				   fill the PATCH structure with the 
				   attribute and number of the patch,
				   and set the first pt to NULL */

        if (pts == 0) {
	   patch->att = class;
	   patch->num = total_patches + 1;
	   ptrfirst = (PT *) NULL;
	}

				/* if this pt has a positive value, then
				   it hasn't been traced.  So:
				     (1) change its value to negative,
				     (2) put a 1 in patchmap at the location,
				     (3) save the row & col in PATCH structure,
				     (4) see if the pt expands the current
					 bounding box
				     (5) increment the pts count */

        if(*(*(buf + i) + j) > 0) {  
           *(*(buf + i) + j) = -*(*(buf + i)+ j);
	   *(*(patchmap + i) + j) = 1;
	   ptrnew = (PT *)G_calloc(1, sizeof(PT));
	   if (ptrfirst == (PT *) NULL) {
	      ptrfirst = ptrthis = ptrnew;
	   }
	   else { 
	      ptrthis = ptrfirst;
	      while (ptrthis->next != (PT *) NULL)
		 ptrthis = ptrthis->next;
	      ptrthis->next = ptrnew;
	      ptrthis = ptrnew;
	   }
           ptrthis->row = i;
           ptrthis->col = j;
	   if (i > patch->s) patch->s = i;
	   if (i < patch->n) patch->n = i;
	   if (j > patch->e) patch->e = j;
	   if (j < patch->w) patch->w = j;
	   pts++;
        }

				/* if there is a neighboring pixel, with the
				   same class, moving clockwise around the 
				   patch, then reset i and j to this
				   location, then reset di and dj */

	if (yes_nb (&di, &dj, buf, class, i, j)) {
	   i = i + di;
	   j = j + dj;
	   di = -di;
	   dj = -dj;
	   clockwise (&di,&dj);

				/* if tracing has returned to the starting
				   pt, then stop; in a special case with
				   diagonal tracing, don't stop if there is
				   a traceable pixel below and to the left */

	   if (i == row0 && j == col0) {
	      not_done = 0;
	      if (choice->trace && class == *(*(buf + i + 1) + j - 1))
	         not_done = 1;
	   }
	}

				/* if there is no neighboring pixel with the
				   same class, then stop tracing */

	else
	   not_done = 0;

     }									/*2*/


				/* STEP 2: CLEAN AND FILL THE PATCH WITHIN
				   ITS BOUNDARIES. THE MAP IS CLEANED AND 
				   FILLED INTO "PATCHMAP" WHICH THEN CONTAINS
				   THE FOLLOWING VALUES:
					1 = BOUNDARY PT
				     -999 = INTERIOR (NON BOUNDARY) PT */

     for (i=patch->n; i < patch->s + 1; i++) { 				/*3*/

				/* find the westernmost and easternmost boundary
				   points in row i */

        roww = patch->w;
        rowe = patch->e;
	while (*(*(patchmap + i) + roww) == 0 && roww < patch->e) roww++;
	while (*(*(patchmap + i) + rowe) == 0 && rowe > patch->w) rowe--;

				/* if the westernmost and easternmost boundary
				   pts in row i are not the same or are not
				   next to each other, then we need to scan
				   across row i */

	if (roww != rowe && roww + 1 != rowe) {				/*4*/
	   for (j = roww; j < rowe; j++) {				/*5*/

				/* if this pixel is a boundary or interior pt
				   and the next pixel is not, */

	      if (*(*(patchmap + i) + j) != 0 && 
		  *(*(patchmap + i) + j + 1) == 0) {			/*6*/

				/* but it has the same class, then make it
				   a -999 in patchmap and make buf negative */

		 if (*(*(buf + i) + j + 1) == class) {
		    *(*(patchmap + i) + j + 1) = -999;
		    *(*(buf + i) + j + 1) = -abs(class);
		 }

				/* and it doesn't have the same class, then
				   it marks the edge of an interior boundary
				   for the patch.  Trace this boundary only
				   if it has not already been traced */

	         else if (*(*(buf + i) + j + 1) != class &&
		    (*(*(patchmap + i) + j) != 1 || 
		     *(*(patchmap + i) + j + 1) == 0)) {		/*7*/
		    not_done = 1;
		    row1 = p = i;
		    col1 = q = j;
		    di2 = 0;
		    dj2 = 1;
		    while (not_done) {					/*8*/
		       if (*(*(patchmap + p) + q) == -999)
			   *(*(patchmap + p) + q) = 4;
		       if (*(*(patchmap + p) + q) == 4) {
	   		  ptrnew = (PT *)G_calloc(1, sizeof(PT));
		      	  ptrthis = ptrfirst;
	      		  while (ptrthis->next != (PT *) NULL)
		 	     ptrthis = ptrthis->next;
	      		  ptrthis->next = ptrnew;
	      		  ptrthis = ptrnew;
           		  ptrthis->row = p;
           		  ptrthis->col = q;
			  *(*(patchmap + p) + q) = 1;
			  *(*(buf + p) + q) = -abs(class);
			  pts++;
		       }
		       if (yes_nb(&di2, &dj2, buf, class, p, q)) {
			  p = p + di2;
			  q = q + dj2;
			  if (*(*(patchmap + p) + q) != 1)
			      *(*(patchmap + p) + q) = 4;
			  di2 = -di2;
			  dj2 = -dj2;
			  clockwise(&di2, &dj2);
			  if (p == row1 && q == col1) {
			     not_done = 0;
			  }
		       }
		       else not_done = 0;
		    }							/*8*/
		 }							/*7*/
	      }								/*6*/
	   }								/*5*/
	}								/*4*/
     }									/*3*/


				/* STEP 3: IF A PATCHMAP WAS REQUESTED,
				   THEN COPY THE PATCH NUMBERS INTO THE
				   PAT ARRAY */ 

     for (i = patch->n; i < patch->s + 1; i++) {
	for (j = patch->w; j < patch->e + 1; j++) {
	   if(*(*(patchmap + i) + j) ||
	      *(*(patchmap + i) + j) == -999) {

				/* if a num map was requested with the -n flag,
				   then copy the patch numbers into pat array */

	      if (choice->patchmap)
	         *(*(pat + i) + j) = patch->num;
	   }
	}
     }



				/* STEP 4: GO THROUGH THE RESULTING LIST OF PTS,
				   RECORD THE ROW AND COL IN THE PATCH 
				   STRUCTURE, AND FIND THE LONG AXIS AND
				   CENTER OF THE PATCH */

     patch->npts = pts;

				/* allocate enough memory to store the list of
				   pts in the PATCH structure */

     patch->col = (int *)G_calloc(pts, sizeof(int));
     patch->row = (int *)G_calloc(pts, sizeof(int));

				/* go through the list of pts */

     i = 0;
     ptrthis = ptrfirst;
     while (ptrthis) {
        ptrfree = ptrthis;

				/* save the pt locat. in the PATCH structure */
	
	*(patch->row + i) = ptrthis->row;
	*(patch->col + i) = ptrthis->col;

				/* patch center step 1: sum up the boundary
				   coordinates */

        if (i < pts){
           patch->c_row += *(patch->row + i);
           patch->c_col += *(patch->col + i);
        }

        ptrthis = ptrthis->next;
	free(ptrfree);
	i++;

     }

				/* patch center step 2: complete
				   the calculations */

     patch->c_col = (int) (patch->c_col/pts + 0.5);
     patch->c_row = (int) (patch->c_row/pts + 0.5);


				/* STEP 3: MAKE NEXT PATCH NULL, FREE MEMORY,
				   AND RETURN THE PATCH STRUCTURE */

     patch->next = NULLPTR;

				/* free the memory allocated for patchmap
				   and the pt list */

     for (i=0; i<nrows+3; i++)
        free(patchmap[i]);
     free(patchmap);

				/* send the patch info back to trace */

     return (patch);

  }									/*1*/  
}






				/* SEARCH THE 8 NEIGHBORS OF A PIXEL IN
				   THE BUFFER IN A CLOCKWISE DIRECTION
				   LOOKING FOR A PIXEL WITH THE SAME
				   CLASS AND RETURN A 1 AND DI, DJ FOR
				   THE FIRST PIXEL FOUND; OTHERWISE RETURN
				   A ZERO */

int  yes_nb(di, dj, buf, class, i, j)
int  *di, *dj, class, i, j;
CELL **buf;
{

				
/* di=0 to start; di is the value to be added to i to get to the 
	pixel with the same value
   dj=-1 to start; dj is the value to be added to j to get to the 
	pixel with the same value  
   class = the attribute of the center pixel
*/

   register int k;

				/* if tracing is to include crossing to 
				   diagonal pixels */

   if(choice->trace) { 							/*1*/  

				/* search through the 8 neighbor pixels */

      for(k=0; k<8; k++) { 

				/* if the neighbor pixel has the same 
				   attribute as that of the current pixel,
				   then maybe it is part of the same patch */

         if(class == abs(*(*(buf+i+*di)+j+*dj)))
	    return 1;
         else 
            clockwise(di, dj);
   
      }	

				/* if no neighbor with the same class is found,
				   then we are done tracing the patch */

      return 0;
   }									/*1*/

  				/* if tracing is not to include crossing to
				   diagonal pixels */
   else {
				/* search through the 8 neighbor pixels */

      for(k=0; k<8; k++){

				/* if the neighbor pixel has the same 
				   attribute as that of the current pixel,
				   then maybe it is part of the same patch */

         if(class == abs(*(*(buf+i+*di)+j+*dj))) {

				/* if the neighbor pixel is directly above,
				   below, to the right or left of the current
				   pixel then tracing can continue */

	    if (*di == 0 || *dj == 0) return 1;

				/* next check the diagonal neighbors 
				   that have a bishops pattern and if
				   they have an adjacent pixel with the same
				   class then continue tracing, as they are
				   not isolated diagonal pixels */

				/* lower left bishops pattern */

	    if(*di == 1 && *dj == -1) 
	       if((class == abs(*(*(buf+i+*di)+j))) ||
	          (class == abs(*(*(buf+i)+j+*dj)))) return 1;

				/* upper left bishops pattern */

	    if(*di == -1 && *dj == -1) 
	       if((class == abs(*(*(buf+i+*di)+j))) ||
	          (class == abs(*(*(buf+i)+j+*dj)))) return 1;

				/* upper right bishops pattern */

 	    if(*di == -1 && *dj == 1) 
	       if((class == abs(*(*(buf+i+*di)+j))) ||
	          (class == abs(*(*(buf+i)+j+*dj)))) return 1;

				/* lower right bishops pattern */

 	    if(*di == 1 && *dj == 1) 
	       if((class == abs(*(*(buf+i+*di)+j))) ||
	          (class == abs(*(*(buf+i)+j+*dj)))) return 1;

         }

				/* if the neighbor pixel has a different
				   class or it is not in the
				   same row or col and is an isolated
				   bishops pattern pixel, then don't
				   trace it, but go to the next one
				   of the 8 neighbors */
        
         clockwise(di, dj);
   
      }

				/* if all the neighbors are isolated
				   bishops pattern pixels or no 
				   neighbor with the same class is found
				   then we are done tracing the patch */

      return 0;


   }
}









				/* CIRCLE CLOCKWISE AROUND THE CURRENT PT */

void  clockwise(i, j)    
int    *i, *j;

{
  if(*i != 0 && *j != -*i)
     *j -= *i;
  else 
     *i += *j;
}  

 



				/* CIRCLE COUNTERCLOCKWISE AROUND THE CURRENT
				   PT */

void counterclockwise(i, j)
int	*i, *j;

{
   if (*i==1 && *j==0) *j=1;
   else if (*i==1 && *j==1) *i=0;
   else if (*i==0 && *j==1) *i=-1;
   else if (*i==-1 && *j==1) *j=0;
   else if (*i==-1 && *j==0) *j=-1;
   else if (*i==-1 && *j==-1) *i=0;
   else if (*i==0 && *j==-1) *i=1;
   else if (*i==1 && *j==-1) *j=0;
}



