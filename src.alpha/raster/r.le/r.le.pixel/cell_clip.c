                 		/********************************/
		                /* 	r.le.pixel/cell_clip.c  */
                                /*                              */
				/*		2.1		*/
				/*				*/
                                /*        07/05/94 version      */
                                /*                              */
				/*        Programmer: Baker	*/
				/*	  Univ. of Wyoming	*/
		                /********************************/

#include "r.le.pixel.h"

extern struct CHOICE *choice;
extern int	     finput;





					/* CELL CLIP DRIVER */

void  cell_clip_drv(col0, row0, ncols, nrows, value, index, cntwhole)
int   row0, col0, nrows, ncols, **value, index, cntwhole;
{
  CELL         **buf;
  register int i,j;
  int	       *richtmp, *rich, cnt=0;	 

				/* dynamically allocate storage for the
				   buffer that will hold the contents of
				   the window */

  buf = (CELL **)G_calloc(nrows+3, sizeof(CELL *));
  for(i=0; i<nrows+3; i++) {
     buf[i] = (CELL *)G_calloc(ncols+3, sizeof(CELL));
  } 

					/* call the cell_clip routine */

  cell_clip(buf, row0, col0, nrows, ncols, index);

					/* dynamically allocate memory for
					   the richness array */

  richtmp = (int *)G_calloc(MAX, sizeof(int));

 					/* go through the sampling area
					   pixel by pixel */

  for(i = 1; i < nrows + 1; i++) {
     for(j = 1; j < ncols + 1; j++) {
        if (buf[i][j]) 

					/* call get_rich to tally up the
					   number of different attributes
					   in the sampling area and fill
					   the richness array with those
					   attributes */

	   get_rich(buf[i][j], richtmp, &cnt);
     }
  }

  if (cnt) {

     rich = (int *)G_calloc(cnt, sizeof(int));
     for (i = 0; i < cnt; i++) {
        rich[i] = richtmp[i];
     }
     free(richtmp);
     

					/* call ANSI C runtime library
					   function qsort to sort the 
					   richness array into ascending
					   order */
 
     qsort(rich, cnt, sizeof(int), compar);

					/* moving window */

     if(choice->wrum == 'm') {
        if (is_not_empty_buffer(buf,nrows+3,ncols+3)) {
	   if(center_is_not_zero(buf,nrows,ncols))
              mv_texture(nrows, ncols, buf, value, index, rich, cnt, cntwhole); 
        } 
     }

					 /* whole map, units, or regions */

     else  
        if (is_not_empty_buffer(buf,nrows+3,ncols+3))
           df_texture(nrows, ncols, buf, rich, cnt, cntwhole);

     for(i=0; i<nrows+3; i++) 
        free(*(buf+i));
     free(buf);
  }
  else
    free(richtmp);
  free(rich);
}






					/* CHECK BUFFER; RETURN 1 IF BUFFER
					   IS NOT EMPTY, 0 IF EMPTY */

is_not_empty_buffer(buf,rows,cols)

int **buf, rows, cols ;

{
 
  register int i, j ;

  for (i=0;i<rows;i++)
      for (j=0;j<cols;j++) 
          if(buf[i][j]) return (1) ;
  return(0) ;

}




					/* CHECK TO SEE IF THE CENTER PIXEL
					   IN THE BUFFER IS ZERO.  RETURN 1
					   IF IT IS, 0 IF NOT */

center_is_not_zero(buf,rows,cols)

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
CELL   **buf; 
int   row0, col0, nrows, ncols, index;
{
  CELL          *tmp, *tmp1;
  int           fr, ftmp;
  register int  i, j;

					/* if regions */

  if (choice->wrum == 'r') {
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
     fprintf(stderr, "Analyzing region number %d...\n", index);
  }

  tmp =  G_allocate_cell_buf();

					/* for each row */

  for(i = row0; i < row0 + nrows; i++){

 					/* if region, read in the corresponding
					   map row in the region file */

     if (choice->wrum == 'r') 
        G_get_map_row_nomask(fr, tmp1, i);

					/* allocate memory for 1 row, then
					   initialize each element of the
					   row to 0 */

     G_zero_cell_buf(tmp);

					/* read row i of the map into tmp */

     G_get_map_row(finput, tmp, i);

					/* for all the columns one by one */
 
     for(j = col0; j < col0 + ncols ; j++) {

					/* if the choice is not "by region" 
					   or if the column is in region att */

        if (choice->wrum != 'r' || *(tmp1+j) == index) {

					/* copy the contents of tmp into the
					   appropriate cell in buf */

 	   *(*( buf+i+1-row0) +j+1-col0) = *(tmp+j) ; 

        } 
     } 
  } 
  
  free(tmp);
  if (choice->wrum == 'r') {
    free(tmp1);
    G_close_cell(fr);
  }
}







					/* FIND UNCOUNTED ATTRIBUTES, 
					   COUNT THEM UP, AND ADD THEM TO
					   THE RICHNESS ARRAY IN UNSORTED
					   ORDER */

void  get_rich(att, rich, cnt)
int  att, rich[], *cnt;
{
  register int i;

					/* if this attribute is already
					   in the richness array, then
					   return */

  for(i = 0; i < *cnt; i++)
     if(att == rich[i]) break;

					/* if this attribute is not already
					   in the richness array, then make
					   it the "cnt" element of the 
					   array, then increment the cnt */

  if (i >= *cnt) {
     rich[*cnt] = att;
     ++(*cnt);
  }

}





					/* COMPARE */

int compar(i, j)
int  *i, *j;
{ 
  return(*i - *j);
}


