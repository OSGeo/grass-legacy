				/********************************/
				/*	r.le.trace/main.c	*/
				/*				*/
				/*		2.1		*/
				/*				*/
				/*	Version 07/15/94	*/
				/*				*/
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/

#include "r.le.trace.h"
#include "stdio.h"

struct CHOICE 		*choice;
int   			finput;
int   			total_patches=0;
PATCH 			*patch_list = NULLPTR;
FILE  			*fp;



				/* MAIN PROGRAM */

main(argc, argv)
int  argc;
char *argv[];
{
  struct Cell_head  window;
  int               bot, right, t0, b0, l0, r0;
  double 	    Rw_l, Rscr_wl;
  char 		    tmp[20];
  void 		    set_map();

  setbuf (stdout, NULL);	/* unbuffered */
  setbuf (stderr, NULL);
  G_gisinit(argv[0]); 


  choice = (struct CHOICE *) G_calloc(1, sizeof(struct CHOICE));

  
  user_input(argc,argv) ;

  				/* setup the current window for display */

  G_system(" d.colormode float");
  G_system(" d.frame -e");
  Rw_l = (double)G_window_cols()/G_window_rows();
  R_open_driver(); 
  R_font("romant");
  G_get_set_window(&window);
  t0 = R_screen_top();
  b0 = R_screen_bot();
  l0 = R_screen_left();
  r0 = R_screen_rite();
  Rscr_wl = (double)(r0 - l0) / (b0 - t0);

  if(Rscr_wl > Rw_l){
    bot = b0;
    right = l0 + (b0 - t0) * Rw_l; 
  } else {
    right = r0;
    bot = t0 + (r0 - l0) / Rw_l; 
  }

  D_new_window("a", t0, bot, l0, right);
  D_set_cur_wind("a");
  D_show_window(D_translate_color("green"));
  R_set_window(t0, bot, l0, right);
  R_font("cyrilc");
  R_text_size(8, 8);
  R_close_driver();

  				/* invoke the setup modules */

  if (strcmp(choice->out,""))
     set_map(choice->fn, window, t0, bot, l0, right, choice->out);
  else
     set_map(choice->fn, window, t0, bot, l0, right, NULL);

  free(choice) ;
}





				/* DISPLAY A MESSAGE AND THE MAP, THEN
				   TRACE THE PATCHES AND DISPLAY THEM */

void set_map( name, window, top, bot, left, right, fn)
char   *name, *fn;
struct Cell_head    window;
int                 top, bot, left, right;
{
  char    cmd[30];
  int     i, k=0, j,  btn, d, class;
  double  msc[2];
  void    scr_cell(), cell_clip_drv(), show_patch();


				/* display a message and the map */

  G_system("clear");
  puts("\n\nR.LE.TRACE IS WORKING...\n\n");
  G_system("d.colormode mode=fixed");
  sprintf(cmd, "d.rast %s", name);
  G_system("d.erase");
  G_system(cmd);

  				/* setup the screen-cell array coordinate 
				   system conversion */

  scr_cell(&window, top, bot, left, right, msc);


				/* call the cell clip driver to trace the
				   patches in the window */

  cell_clip_drv(0, 0, window.cols, window.rows, NULL, 0);

				/* show the patches */
  
  show_patch(fn, msc, cmd);
}







				/* DISPLAY THE PATCHES */

void  show_patch(fn, msc, cmd)
char    *fn, *cmd;
double  *msc;
{
  PATCH   *tmp, *tmp0;
  register int  i;
  int		num, show = 0;
  char  	c;
  void          draw_patch(), patch_attr();

printf("show total_patches=%d\n",total_patches);

  if (!total_patches) return;
  
  if (G_yes("\n\nDisplay the traced boundary of a particular patch? ", 1))
     for(;;){
	 G_system("clear");
         printf("\n\n\n The patch number?  ");
         scanf("%d", &num);
	 rewind(stdin);
         if(num < 0) num = -num;
	 tmp = patch_list;
         while(tmp && tmp->num != num) tmp = tmp->next;
	 if(tmp) {
	    draw_patch(tmp, msc, 1);
	    patch_attr(NULL, tmp);		
	 } 
         else{
		G_warning("\nThe patch is not in the patch-list.\n");
	 	sleep(1);
	 }
	 if(!G_yes("\nDisplay another patch? ", 1)) break;
     } 
  rewind(stdin);
  G_system("clear");
  if (G_yes("\n\nDisplay the patches sequentially, with their size, perim., shape? ", 0))
	show = 1;
  rewind(stdin);
  
  if(fn){
     fn[strlen(fn)] = '\0';
     if(!(fp = fopen(fn, "w")))	
	G_fatal_error("Can't open file to write, exit.");
  }
  

  tmp = patch_list;
  while(tmp){   
       if(!show) goto next;	
       puts("\n\n\n <CR> - show patch.");
       puts("   s -  skip one patch.");
       puts("   r -  refresh screen.");
       puts("   q -  quit display and save.");
       c = getchar();
       if(c == 's' || c == 'r' || c == 'q'){
	   if(c == 's')
		getchar();
	   else if(c == 'q')
		show = 0;
	   else {
		getchar();
		G_system("d.erase");
		G_system(cmd);
                draw_patch(tmp, msc, 0);  
	   }
       } else 
           draw_patch(tmp, msc, 0); 
next:
       patch_attr(fp, tmp, show);

       tmp0 = tmp;
       tmp = tmp->next;
       free(tmp0->row);  free(tmp0->col);
       free(tmp0);
  }
  total_patches = 0;
  if(fn)  fclose(fp);
}


				/* DISPLAY PATCH ATTRIBUTES ON THE SCREEN */

void  patch_attr(fp, p, show)
PATCH *p;
int   show;
{
  double  shp1, shp2, shp3;

  shp1 = p->perim / p->area;
  shp2 = 0.282 * p->perim / sqrt(p->area);
  shp3 = 2.0 * sqrt(p->area / M_PI) / p->long_axis;

  if(!show) goto skip;

  printf("\n  Patch[%d] of total %d:\n", p->num, total_patches);
  printf("	Attribute             = %d\n", p->att);
  printf("	Size                  = %.2f\n", p->area);
  printf("	Perimeter             = %.2f\n", p->perim);
  printf("	Shape: P/A            = %.4f\n",  shp1);
  printf("	Shape: CP/A           = %.4f\n",  shp2);
  printf("	Shape: RCC            = %.4f\n",  shp3);

skip:
  if(!fp) return;
  if(p->num == 1)
	fprintf(fp, "Patch #    Attribute    Perimeter     Area           P/A-CPA-RCC\n");
	fprintf(fp, "%5d    %7d      %10.1f%10.1f%10.4f%10.4f%10.4f\n",
			 p->num, p->att, p->perim, p->area, shp1, shp2, shp3);
}






				/* DRAW PATCHES ON THE SCREEN */

void   draw_patch(p, m, opt)
PATCH  *p;
int    opt;
double *m;
{
  register int  i;
  int           r0, c0, r1, c1;
  char    	number[10];
  void  	draw_cross();

  G_sleep_on_error(0);

  R_open_driver();
  R_standard_color(D_translate_color("white"));
  r0 = p->row[0]*m[1];
  c0 = p->col[0]*m[0];
  R_move_abs(c0, r0);

  for(i = 1; i < p->npts; i++){
     r0 = p->row[i]*m[1];
     c0 = p->col[i]*m[0];
     if(abs(p->row[i] - p->row[i-1]) > 1 || 
	abs(p->col[i] - p->col[i-1]) > 1)
          R_move_abs(c0, r0);
     else {
	  if (!opt)
             R_cont_abs(c0, r0);
	  else {
	     /* R_close_driver(); */
	  /* R_open_driver(); */
	     R_cont_abs(c0, r0);
	  /* R_close_driver(); */
	  /* R_open_driver(); */
	  }
     }
  }
  r1 = p->c_row*m[1];
  c1 = p->c_col*m[0];
  R_move_abs(c1, r1);
  sprintf(number, "%d\0", p->num);
  R_text(number);  
  R_close_driver();
}

 




				/* SETUP THE CONVERSION BETWEEN SCREEN AND
				   ARRAY SYSTEMS */

void  scr_cell(wind, top, bot, left, right, m)
struct Cell_head  *wind;
int    top, bot, left, right;
double  *m;
{ 
  m[0] = (right - left) / (double)wind->cols;
  m[1] = (bot - top) / (double)wind->rows;
}






				/* DRIVER FOR CELL CLIPPING, TRACING,
				   AND CALCULATIONS */


void  cell_clip_drv(col0, row0, ncols, nrows, value, index)
int   row0, col0, nrows, ncols, index;
int  **value;
{
  CELL         	**buf, **pat, *row_buf, *cor_buf;
  int 		i, j, fd, fe, p;        
  PATCH  	*list_head;


  total_patches=0;

				/* open the cell file for input */

  if(0 > (finput = G_open_cell_old(choice->fn, G_mapset()))) {
     printf("\n");
     printf("   ********************************************************\n");
     printf("    The raster map you specified with the 'map=' parameter \n");
     printf("    was not found in your mapset.                          \n");
     printf("   ********************************************************\n");
     exit(1);
  }

				/* dynamically allocate storage for the
				   buffer that will hold the contents of
				   the window */

  buf = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
  for(i=0; i<nrows+3; i++)
      buf[i] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));

				/* dynamically allocate storage for the
				   buffer that will hold the map of the
				   patch numbers */

  if (choice->patchmap) {
     pat = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
     for(i=0; i<nrows+3; i++)
         pat[i] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));
  }

				/* clip out the sampling area */

  cell_clip(buf, row0, col0, nrows, ncols, index);

				/* trace the patches in the sampling area */

  trace(nrows, ncols, buf, pat);

				/* if a map of patches was requested */

  if (choice->patchmap) {
     fd = G_open_cell_new("num");
     for (i=1; i<nrows + 1; i++) {
	row_buf = G_allocate_cell_buf();
	G_zero_cell_buf(row_buf);
        for (j=1; j<ncols + 1; j++)
	   *(row_buf + j - 1) = *(*(pat + i) + j);
        G_put_map_row(fd,row_buf);
     }
  }

				/* close the num file and release the
				   memory allocated for it */

  if (choice->patchmap) {
     G_close_cell(fd);
     free(row_buf);
  }

  G_close_cell(finput);

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

  tmp =  G_allocate_cell_buf();

				/* for each row */

  for(i = row0; i < row0 + nrows; i++){

				/* allocate memory for 1 row, then 
				   initialize each element of the 
				   row to 0 */
 
    G_zero_cell_buf(tmp);

				/* read row i of the map into tmp */

    G_get_map_row(finput, tmp, i); 

				/* for all the columns one by one */

    for(j = col0; j < col0 + ncols; j++) {

				/* copy the contents of tmp into the
				   appropriate cell in buf */

       *(*(buf+i+1-row0)+j+1-col0) = *(tmp+j);
    }
  }  

  free(tmp);
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
				   after a pt has been traced */

  i=0;
  while(i++ < nrows) {							/*1*/
     j=0;
     while(j++ < ncols) {						/*2*/

 				/* if this pt contains a positive value, it 
				   may be the start of an untraced patch */

       if((class = *(*(buf+i)+j)) > 0 ) { 				/*3*/

				/* trace the patch from the current pt */

          list_head = patch_list;
	  if(find_any = get_bd(i, j, nrows, ncols, class, buf, list_head,
		 pat)){						/*4*/ 

 				/* if the first patch, make tmp point to
				   the patch list and add the first patch
				   to the list */

 	     if(total_patches == 0) {                
                patch_list = find_any;
		tmp = patch_list;
	     }
				/* add the next patch to the patch list */

             else {
                tmp->next = find_any;	     	 
	        tmp = tmp->next;
	     }

				/* increment the count of total patches */

	     total_patches ++;
       	      
	  }								/*4*/

				/* if i and j are now at or outside the
				   limits of the window, then quit */

    	  if(i >= nrows && j >= ncols)
		return;
       }								/*3*/

  				/* if this pt is the boundary point of an 
				   already traced patch or is outside the
				   window, do not start tracing; skip to 
				   next pt */

       else if ((class = *(*(buf+i)+j)) <= 0) { 			/*5*/    
				/* if i and j are now at or outside the
				   limits of the window, then quit */

    	  if(i >= nrows && j >= ncols)
             return;
       }								/*5*/
    }									/*2*/
  } 									/*1*/
}







				/* TRACE THE BOUNDARY OF A PATCH AND
				   SAVE THE PATCH CHARACTERISTICS IN
				   THE PATCH STRUCTURE */				   

PATCH *get_bd(row0, col0, nrows, ncols, class, buf, p_list, pat)
int   row0, col0, class, nrows, ncols;
CELL  **buf, **pat;
PATCH *p_list; 
{  
  int           	i = row0, j = col0, pts=0, di=0, dj=-1, 
                	not_done, k, m, i0, j0, tmp, lng=0, roww=0,
			rowe=0, coln=0, cols=0, c, d, row1, col1,
			di2, dj2, p, q, area, per, nozero;
  PATCH         	*patch;
  CELL			**patchmap;
  PT			*ptrfirst, *ptrthis, *ptrnew, *ptrfree;

				/* allocate memory for 1 patch to be
				   saved in the patch data structure */

  patch = (PATCH *)G_calloc(1, sizeof(PATCH));

				/* allocate memory for patchmap, which
				   will hold the boundaries found in
				   the buf array */

  patchmap = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
  for (m = 0; m < nrows + 3; m++)
     patchmap[m] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));

				/* if this is the first patch to be traced,
				   then set the next patch on the patch list
				   to NULL */

  if (total_patches == 0)  patch->next = NULLPTR;

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



				/* STEP 4: GO THROUGH THE RESULTING PATCHMAP
				   AND DETERMINE THE PATCH SIZE, AMOUNT OF 
				   PERIMETER AND, IF REQUESTED, THE CORE SIZE
				   AND EDGE SIZE */

     area = 0;
     per = 0;
     for (i = patch->n; i < patch->s + 1; i++) {
	for (j = patch->w; j < patch->e + 1; j++) {
	   if(*(*(patchmap + i) + j) ||
	      *(*(patchmap + i) + j) == -999) {
	      area++;
	      if (choice->perim2 == 0) {
		 if (j == 1 || j == ncols) per++;
	      }
	      if (j < ncols && *(*(patchmap + i) + j + 1) == 0) per++;
	      if (j > 1 && *(*(patchmap + i) + j - 1) == 0) per++;

				/* if a num map was requested with the -n flag,
				   then copy the patch numbers into pat array */

	      if (choice->patchmap)
	         *(*(pat + i) + j) = patch->num;
	   }
	}
     }
     for (j = patch->w; j < patch->e + 1; j++) {
        for (i = patch->n; i < patch->s + 1; i++) {
	   if(*(*(patchmap + i) + j) ||
	      *(*(patchmap + i) + j) == -999) {
	      if (choice->perim2 == 0) {
		 if (i == 1 || i == nrows) per++;
	      }
	      if (i < nrows && *(*(patchmap + i + 1) + j) == 0) per++;
	      if (i > 1 && *(*(patchmap + i - 1) + j) == 0) per++;
	   }
	}
     }
     patch->area = area;
     patch->perim = per;

				/* STEP 5: GO THROUGH THE RESULTING LIST OF PTS,
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

				/* long-axis step 1: find the largest
				   sum of squares between patch boundary
				   pts for the Related Circumscribing Circle
				   shape index */

        if (pts == 1) {
           lng = 2;
        }
        else {
           for (j = 0; j < i + 1; j++) {
              if ((tmp = (abs(*(patch->row + j) - *(patch->row + i)) + 1) * 
		 (abs(*(patch->row + j) - *(patch->row + i)) + 1) + 
                 (abs(*(patch->col + j) - *(patch->col + i)) + 1) *
		 (abs(*(patch->col + j) - *(patch->col + i)) + 1))>lng)
	         lng = tmp;
           }
        }
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

				/* patch long axis and center step 2: complete
				   the calculations */

     patch->long_axis = sqrt((double)(lng));
     patch->c_col = (int) (patch->c_col/pts + 0.5);
     patch->c_row = (int) (patch->c_row/pts + 0.5);


				/* STEP 6: MAKE NEXT PATCH NULL, FREE MEMORY,
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

 

