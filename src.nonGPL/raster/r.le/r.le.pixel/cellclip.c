                        /********************************/
                        /*    r.le.pixel/cellclip.c     */
                        /*                              */
                        /*            2.2               */
                        /*                              */
                        /*      12/1/97 version         */
                        /*                              */
                        /*      Programmer: Baker       */
                        /*      Univ. of Wyoming        */
                        /********************************/

#include "pixel.h"

extern struct CHOICE *choice;
extern int	     finput;


					/* CELL CLIP DRIVER */

void  cell_clip_drv(col0, row0, ncols, nrows, value, index, cntwhole, radius)
int   row0, col0, nrows, ncols, **value, index, cntwhole;
float radius;
{
  CELL         **buf;
  register int i,j;
  int	       *richtmp, *rich, cnt=0;	 

/*
  col0 = starting column for area to be clipped
  row0 = starting row for area to be clipped
  ncols = number of columns in area to be clipped
  nrows = number of rows in area to be clipped
  value =
  index = number of the region to be clipped, if there's a region map
  buf = pointer to array containing the clipped area, a smaller area
        than the original raster map to be read from finput
  pat = pointer to array containing the map of patch numbers
  cor = pointer to array containing the map of interior area
*/

				/* dynamically allocate storage for the
				   buffer that will hold the contents of
				   the window */

  buf = (CELL **)G_calloc(nrows+3, sizeof(CELL *));
  for(i = 0; i < nrows + 3; i++) {
     buf[i] = (CELL *)G_calloc(ncols+3, sizeof(CELL));
  } 

					/* call the cell_clip routine */

  cell_clip(buf, row0, col0, nrows, ncols, index, radius);

					/* dynamically allocate memory for
					   the richness array */

  richtmp = (int *)G_calloc(MAX, sizeof(int));

 					/* go through the sampling area
					   pixel by pixel */

  for(i = 1; i < nrows + 1; i++) {
     for(j = 1; j < ncols + 1; j++) {

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
	       if (center_is_not_zero(buf,nrows,ncols))
              mv_texture(nrows, ncols, buf, value, index, rich, cnt, cntwhole); 
        } 
     }

					 /* whole map, units, or regions */

     else  
        if (is_not_empty_buffer(buf,nrows+3,ncols+3))
           df_texture(nrows, ncols, buf, rich, cnt, cntwhole);

     for(i = 0; i < nrows + 3; i++) 
        free(*(buf+i));
     free(buf);
     free(rich);
  }
  else 
     free(richtmp);
}






					/* CHECK BUFFER; RETURN 1 IF BUFFER
					   IS NOT EMPTY, 0 IF EMPTY */

int is_not_empty_buffer(buf,rows,cols)

int **buf, rows, cols ;

{
 
  register int i, j ;

  for(i = 0;i < rows;i++)
     for(j = 0;j < cols;j++) 
        if(buf[i][j]) return (1) ;
  return(0) ;

}




					/* CHECK TO SEE IF THE CENTER PIXEL
					   IN THE BUFFER IS ZERO.  RETURN 1
					   IF IT IS, 0 IF NOT */

int center_is_not_zero(buf,rows,cols)

int **buf, rows, cols ;

{

  if (buf[(rows/2) + 1][(cols/2) + 1]) 
     return (1) ;
  return (0) ;

}




				/* OPEN THE RASTER FILE TO BE CLIPPED,
				   AND DO THE CLIPPING */

void cell_clip(buf, row0, col0, nrows, ncols, index, radius)
CELL   **buf; 
int   row0, col0, nrows, ncols, index;
float radius;
{
  CELL          *tmp, *tmp1;
  int           fr;
  register int  i, j;
  float center_row=0.0, center_col=0.0;
  double dist;

/*
  buf = pointer to array containing only the pixels inside the area 
	that was specified to be clipped, so a smaller array than the
        original raster map
  row0 = starting row for the area to be clipped out of the raster map
  col0 = starting col for the area to be clipped out of the raster map
  nrows = total number of rows in the area to be clipped
  ncols = total number of cols in the area to be clipped
  index = number of the region to be clipped, if there's a region map
  tmp = pointer to a temporary array to store a row of the raster map
  tmp1 = pointer to a temporary array to store a row of the region map
  fr = return value from attempting to open the region map
  i, j = indices to rows and cols of the arrays
*/

				/* if sampling by region was chosen */

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

				/* allocate memory to store a row of the
				   raster map */

  tmp =  G_allocate_cell_buf();

  				/* if circles are used for sampling, then
				   calculate the center of the area to be 
				   clipped, in pixels */

  if ((int)radius) {
     center_row = (float)((double)row0 + ((double)nrows - 1)/2);
     center_col = (float)((double)col0 + ((double)ncols - 1)/2);
  }

				/* for each row of the area to be clipped */

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

				/* if circles are used for sampling */

        if ((int)radius) {
           dist = sqrt(((double)i-center_row) *
                       ((double)i-center_row) + 
                       ((double)j-center_col) *
                       ((double)j-center_col));

				/* copy the contents of tmp into the
				   appropriate cell in buf */

           if (dist < radius)
              *(*(buf+i+1-row0)+j+1-col0) = *(tmp+j);
       }  

				/* if circles are not used and 
				   if the choice is not "by region" or
				   if this column is in region "index" */

       else {
          if (choice->wrum != 'r' || *(tmp1+j) == index) {

				/* copy the contents of tmp into the
				   appropriate cell in buf */

             *(*(buf+i+1-row0)+j+1-col0) = *(tmp+j);
          }
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


