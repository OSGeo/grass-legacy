			/********************************/
			/*     r.le.setup/sample.c 	*/
			/*				*/
			/*             2.1              */
			/*				*/
			/*       07/25/94 version       */
			/*				*/
			/*      Programmer: Baker       */
			/*      Univ. of Wyoming 	*/
			/********************************/

#include "setup.h"

int  tag = 0;


				/* SAMPLING UNIT SETUP DRIVER */

void  sample(t0, b0, l0, r0, name, name1, name2, msc)
double   *msc;
int      t0, b0, l0, r0;
char     *name, *name1, *name2;
{
  int    btn, d, fmask;
  double tmp;

				/* determine whether the user will use
				   the keyboard or mouse to setup the 
				   sampling units */

keyboard:
  printf("\n    Which do you want to do?");
  printf("\n       (1) Use the keyboard to enter sampling unit parameters");
  printf("\n       (2) Draw the sampling units with the mouse");
  printf("\n                                                 Enter 1 or 2:   ");
  numtrap(1, &tmp);
  d = (int)(tmp);
  if (d < 1 || d > 2) {
     printf("     You did not enter a 1 or 2, try again\n");
     goto keyboard;
  }

  if (d == 1 || d == 2) {
                                /* return a value > 0 to fmask if there is
				   a MASK present */

     printf("\n    If a MASK is not present (see r.mask) a beep will sound\n");
     printf("    and a warning will be printed that can be ignored.\n");
     printf("    If a MASK is present there will be no warning.\n");
     fmask = G_open_cell_old("MASK",G_mapset());
     printf("\n");

				/* call the routine to setup sampling
				   units manually */
 
     if (d == 1)
        man_unit(t0, b0, l0, r0, name, name1, name2, msc, fmask); 
 
				/* call the routine to setup sampling 
				   units graphically */

     else if (d == 2)
        graph_unit(t0, b0, l0, r0, name, name1, name2, msc, fmask);

     G_close_cell(fmask);
  }

				/* if neither, then exit */

  else 
     exit(0);
}





				/* DEFINE SAMPLING UNITS MANUALLY */

void man_unit(t, b, l, r, n1, n2, n3, mx, fmask)
int     t, b, l, r, fmask;
double  *mx;
char    *n1, *n2, *n3 ;
{
  int      i, j, dx, dy, w_w, w_l, u_w, u_l, 
	   method, l0, t0, randflag=0, unit_num, num=0, scales, 
	   h_d=1, v_d=1, *ux, *uy, itmp, thick, sites, *row_buf, fr, k,
	   count=0, maxsize=0, nx=0, ny=0, numx=0, numy=0,
	   al=0, ar=0, at=0, ab=0, au_w=0, au_l=0;
  FILE     *fp ;
  double   dtmp, ratio, size, intv=0.0, start[2], cnt=0;
  char     *sites_mapset; 
  struct Cell_head   wind;

/*  VARIABLES:
	COORDINATES IN THIS ROUTINE ARE IN CELLS

	t	=	top row of sampling frame
	b	=	bottom row of sampling frame
	l	=	left col of sampling frame
	r	=	right col of sampling frame
	n1	=
	n2	=
	n3	=
	start[0]=	row of UL corner of starting pt for strata
	start[1]= 	col of UL corner of starting pt for strata
	mx[0]	=	cols of region/width of screen
	mx[1]	=	rows of region/height of screen

*/

 
  start[0] = 0.0;
  start[1] = 0.0;

  l = (int)((double)(l * mx[0]) + 0.5);
  r = (int)((double)(r * mx[0]) + 0.5);
  t = (int)((double)(t * mx[1]) + 0.5);
  b = (int)((double)(b * mx[1]) + 0.5);
  w_w = r-l;
  w_l = b-t;

				/* draw the sampling frame */

  R_open_driver();  
  R_standard_color(D_translate_color("grey"));
  draw_box((int)(l/mx[0] + 0.5), (int)(t/mx[1] + 0.5), (int)(r/mx[0] + 0.5),
     (int)(b/mx[1] + 0.5), 1);
  R_close_driver();

/*printf("t=%d b=%d w_l=%d mx[1]=%f l=%d r=%d w_w=%d mx[0]=%f\n",t , b, w_l, mx[1], l, r, w_w, mx[0]);
*/

  fp = fopen0("r.le.para/units", "w");
  G_sleep_on_error(0);
    
  				/* get the number of scales */

  do {
     printf("\n    How many different SCALES do you want (1-15)?   ");
     numtrap(1, &dtmp);  
     if (dtmp > 15 || dtmp < 1)
	printf("\n    Too many (>15) or too few scales; try again");
  }  while (dtmp < 1 || dtmp > 15 );
  fprintf(fp, "%10d    # of scales\n", (scales = (int)dtmp));

				/* for each scale */

  for(i = 0; i < scales; i++){	
     for(;;){ 
        G_system("clear");

	printf("\n\n    TYPE IN PARAMETERS FOR SCALE %d:\n", i+1);

  				/* get the distribution method */

        printf("\n    Methods of sampling unit distribution  \n");
        printf("       Random nonoverlapping    (1)\n");
        printf("       Systematic contiguous    (2)\n");
        printf("       Systematic noncontiguous (3)\n");
        printf("       Stratified random        (4)\n"); 
        printf("       Centered over sites      (5)\n"); 
        do {
           printf("                             Which METHOD?   "); 
           numtrap(1, &dtmp);
           if ((method = fabs(dtmp)) > 5 || method < 1)
              printf("\n    Choice must between 1-5; try again");
        } while(method > 5 || method < 1);

				/* for stratified random distribution,
				   determine the number of strata */

        if (method == 4) {
getstrata:	     
           printf("\n    Number of strata along the x-axis? (1-60)  ");
           numtrap(1, &dtmp);
	   h_d = fabs(dtmp);

           printf("\n    Number of strata along the y-axis? (1-60)  ");
           numtrap(1, &dtmp);
           v_d = fabs(dtmp);

           if (h_d < 1 || v_d < 1 || h_d > 60 || v_d > 60) {
 	      printf("\n    Number must be between 1-60; try again.");
	      goto getstrata;
	   }
        }

        if (method == 2 || method == 3 || method == 4) {
strata:
           printf("\n    Sampling frame row & col for upper left corner of");
           printf(" the strata?\n       Rows are numbered down and columns");
           printf(" are numbered to the right\n       Enter 1 1 to start in");
           printf(" upper left corner of sampling frame:  ");
           numtrap(2, start);
	   start[0] = start[0]-1.0;
	   start[1] = start[1]-1.0;
	   if (start[0] > w_l || start[0] < 0 || 
	       start[1] > w_w || start[1] < 0) {
	      printf("\n    The starting row and col you entered are outside");
	      printf(" the sampling frame\n       Try again\n");
	      goto strata;
	   }
	}


        if (method == 4) {

				/* call draw_grid with the left, top, width,
				   length, the number of horizontal and
				   vertical strata, and the starting row
				   and col for the strata */

  	   draw_grid((int)(l/mx[0] + 0.5), (int)(t/mx[1] + 0.5),
              (int)(w_w/mx[0] + 0.5), (int)(w_l/mx[1] + 0.5), h_d, v_d,
              (int)(start[0]/mx[1] + 0.5), (int)(start[1]/mx[0] + 0.5),
              mx[0], mx[1]);
	   if (!G_yes("    Are these strata OK?   ",1)) {
	      if (G_yes("\n\n    Refresh the screen?   ", 1)) {
	         paint_map(n1, n2, n3);
                 R_open_driver();  
   	         R_standard_color(D_translate_color("grey"));
     	         draw_box((int)(l/mx[0] + 0.5), (int)(t/mx[1] + 0.5),
                    (int)(r/mx[0] + 0.5), (int)(b/mx[1] + 0.5), 1);
    	         R_close_driver();
              }
	      goto getstrata;
           }
        }

	  			/* get the width/length ratio */

	printf("\n    Sampling unit SHAPE (#cols/#rows) expressed as real");
        printf(" number\n    (e.g., 10 cols/5 rows = 2.0) for sampling units");
        printf(" of scale %d? ", i+1);
      	numtrap(1, &ratio);
	if (ratio < 0) ratio = -ratio;

getsize:
	  			/* determine the maximum unit size, and 
				   display it on the screen */

	dtmp = (ratio > 1) ? 1/ratio : ratio;
	dtmp /= (h_d > v_d) ? h_d*h_d : v_d*v_d;

/*printf("w_w=%d wind.cols=%d start[0]=%d h_d=%d w_w-start[0]=%d (w_w - start[0])/h_d)=%d\n",w_w, wind.cols, (int)(start[0]),h_d, w_w - (int)(start[1]), w_l, (w_w - (int)(start[1]))/h_d);
*/

				/* determine the recommended maximum size
			 	   for sampling units */
tryagain:
        if (method == 1) {

	   if (fmask > 0) {
	      count = 0;
	      row_buf = G_allocate_cell_buf();
 	      fr = G_open_cell_old(n1, G_mapset());
	      for (j = t; j < b; j++) {
		 G_zero_cell_buf(row_buf);
	         G_get_map_row(fr, row_buf, j);
	         for (k = l; k < r; k++) {
		    if (*(row_buf + k)) count++;
	         }
	      }
	      free(row_buf);
	      G_close_cell(n1);
	      cnt = (double)(count);
	      if (cnt)
	         cnt = sqrt(cnt);
	      else
		 cnt = 0;
	      maxsize = ((cnt*dtmp/2)*(cnt*dtmp/2) > 1.0/dtmp) ?
                 (cnt*dtmp/2)*(cnt*dtmp/2) : 1.0/dtmp;
	      printf("\n    Recommended maximum SIZE is %d in %d cell total",
 	         maxsize, count);
              printf(" area\n");
	   }

	   else { 
	      printf("\n    Recommended maximum SIZE is");
              printf(" %d in %d cell total area\n",
                 (int)((w_l-(int)(start[0]))*(w_w-(int)(start[1]))*dtmp/2),
                 (w_l-(int)(start[0]))*(w_w-(int)(start[1])));
	      count = (w_l-(int)(start[0]))*(w_w-(int)(start[1]));
	      maxsize=(int)((w_l-(int)(start[0]))*(w_w-(int)(start[1]))*dtmp/2);
	   }
	}

        else if (method == 2 || method == 3 || method == 5) {
	   printf("\n    Recommended maximum SIZE is %d in %d cell total",
              (int)((w_l-(int)(start[0]))*(w_w-(int)(start[1]))*dtmp/2),
                    (w_l-(int)(start[0]))*(w_w-(int)(start[1])));
           printf(" area\n");
        }

        else if (method == 4) {
	   printf("\n    Recommended maximum SIZE is");
           printf(" %d in %d cell individual", (int)(w_w*w_l*dtmp/2), 
              ((w_w - (int)(start[1]))/h_d) * ((w_l - (int)(start[0]))/v_d));
	   printf(" stratum area\n");
        }

				/* get the unit size, display the calculated
				   size, and ask if it is OK */

        printf("    What size (in cells) for each sampling unit of scale %d?  ",
           i+1);
        numtrap(1, &size);	      	  
	thick = 1;
        if (size < 15 || ratio < 0.2 || ratio > 5) thick = 0;	  
        u_w = sqrt(size * ratio);
        u_l = sqrt(size / ratio);
	printf("\n    The nearest size is %d cells wide X %d cells high = %d",
           u_w, u_l, u_w*u_l);
        printf(" cells\n");
        if (!u_w || !u_l) {
           printf("\n    0 cells wide or high is not acceptable; try again");
           goto tryagain;
        }
	if (!G_yes("    Is this SIZE OK?   ",1))
	   goto getsize;

				/* for syst. noncontig. distribution, get
				   the interval between units */

	if (method==3) {
	   printf("\n    The interval, in cells, between the units of scale");
           printf(" %d?  ",i+1);
    	   numtrap(1, &intv); 	          
  	}

  				/* if the unit dimension + the interval
				   is too large, print a warning and
				   try getting another size */

  	if (u_w + intv > w_w / h_d || u_l + intv > w_l / v_d ) {      
	   printf("    Unit size too large for sampling frame; try again\n");
	   goto getsize;
	} 

				/* for stratified random distribution,
				   the number of units is the same as
				   the number of strata */

        if (method == 4)
           num = h_d * v_d;

				/* for the other distributions, calculate the
				   maximum number of units, then get the
				   number of units */

        else if (method == 1 || method == 2 || method == 3) {

	   if (method == 1) {
              if (!(unit_num = calc_num(w_w, w_l, ratio, u_w, u_l, method,
	         intv, (int)(start[1]), (int)(start[0]), u_w*u_l, count))){
 	         G_warning("Illegal sampling unit size, try again");
                 goto getsize;
	      }
              printf("\n    Maximum NUMBER of units in scale %d is %d\n",
	         i+1, unit_num);
              printf("    Usually 1/2 of this number can be successfully");
              printf(" distributed\n    More than 1/2 can sometimes be");
              printf(" distributed\n");
	   }

	   else if (method == 2 || method == 3) {
              numx = floor((double)(w_w - start[1]) / (u_w + intv));
              numy = floor((double)(w_l - start[0]) / (u_l + intv));

/*printf("w_w=%d start[1]=%d w_w - (int)(start[1])=%d remain=%d\n", w_w,(int)(start[1]), w_w - (int)(start[1]), (w_w - (int)(start[1])) % (numx*(u_w + (int)(intv))));
*/
              if (((w_w - (int)(start[1])) % (numx*(u_w+(int)(intv)))) >= u_w) 
		 numx ++;
              if (((w_l - (int)(start[0])) % (numy*(u_l+(int)(intv)))) >= u_l) 
		 numy ++;
	      unit_num = numx*numy;
              printf("\n    Maximum NUMBER of units in scale %d is %d as %d",
	         i+1, unit_num, numy);
              printf(" rows with %d units per row", numx);
	   }

	   do {
	      printf("\n    What NUMBER of sampling units do you want to try");
              printf(" to use?  ");
	      numtrap(1, &dtmp );

	      if ((num = dtmp) > unit_num || num < 1) {
 	         printf("\n    %d is greater than the maximum number of", num);
                 printf(" sampling units; try again\n");
              }

	      else if (method == 2 || method == 3) {
	         printf("\n    How many sampling units do you want per row?  ");
	         numtrap(1, &dtmp );
	         if ((nx = dtmp) > num) {
 	            printf("\n    Number in each row > number requested; try");
                    printf(" again\n");
                 }
	         else {
		    if (nx > numx) {
 	               printf("\n    Can't fit %d units in each row, try", nx);
                       printf(" again\n");
                    }
		    else {
		       if (num % nx)
			  ny = num/nx + 1;
		       else
			  ny = num/nx; 
		       if (ny > numy) {
 	                  printf("\n    Can't fit the needed %d rows, try", ny);
                          printf(" again\n");
                       }
		    }
		 }
	      }		
           } while (num > unit_num || num < 1 || nx > num || 
		    nx > numx || ny > numy);
	}

				/* dynamically allocate storage for arrays to
				   store the upper left corner of sampling
				   units */

	if (method != 5) {
	   ux = (int *)G_calloc(num+1, sizeof(int));
           uy = (int *)G_calloc(num+1, sizeof(int));
	}

	else { 
	   ux = (int *)G_calloc(250, sizeof(int));
           uy = (int *)G_calloc(250, sizeof(int));
	}

				/* calculate the upper left corner of sampling
				   units and store them in arrays ux and uy */

        if (!calc_unit_loc(t, b, l, r, ratio, u_w, u_l, method, intv, num, h_d, 
	   v_d, ux, uy, &sites, (int)(start[1]), (int)(start[0]), fmask, nx, 
           mx[0], mx[1]))
	   goto last;

        signal (SIGINT, SIG_DFL);
	if (method == 5) 
	   num = sites;	

 				/* draw the sampling units on the 
				   screen */
  
	if (method == 2 || method == 3) {
	   R_open_driver();
           R_standard_color(D_translate_color("red"));
           for (j = 0; j < num; j++) {
	      draw_box((int)((double)(ux[j])/mx[0]),
                       (int)((double)(uy[j])/mx[1]), 
	               (int)((double)(ux[j]+u_w)/mx[0]),
                       (int)((double)(uy[j]+u_l)/mx[1]), 1);
           }
	   R_close_driver();
        }

        if (G_yes("\n    Is this set of sampling units OK?   ", 1)) 
	   break;
last:
        signal (SIGINT, SIG_DFL);
        if (G_yes("\n    Refresh the screen?   ", 1)) {
	   paint_map(n1, n2, n3);
           R_open_driver();  
   	   R_standard_color(D_translate_color("grey"));
     	   draw_box((int)(l/mx[0]), (int)(t/mx[1]), (int)(r/mx[0]),
              (int)(b/mx[1]), 1);
    	   R_close_driver();
        }
     }

				/* save the sampling unit parameters
				   in r.le.para/units file */ 

     fprintf(fp, "%10d    # of units of scale %d.\n", num, (i+1));
     fprintf(fp, "%10d%10d   u_w, u_l of units in scale %d\n",  u_w, u_l,
        (i+1));

     for(j = 0; j < num; j++)
	fprintf(fp, "%10d%10d   left, top of unit[%d]\n", ux[j], uy[j], j+1);	

     if (i < scales - 1 && G_yes("\n\n    Refresh the screen?   ", 1)) {
	paint_map(n1, n2, n3);
        R_open_driver();  
   	R_standard_color(D_translate_color("grey"));
     	draw_box((int)(l/mx[0]), (int)(t/mx[1]), (int)(r/mx[0]),
           (int)(b/mx[1]), 1);
    	R_close_driver();
     }
  } 

				/* free dynamically allocated memory */

  free(ux); 
  free(uy);
  fclose(fp);
}






				/* FOR STRATIFIED RANDOM DISTRIBUTION,
				   DRAW THE STRATA ON THE SCREEN */

void   draw_grid(l, t, w_w, w_l, h_d, v_d, starty, startx, colratio, rowratio)
int   		l, t, w_w, w_l, h_d, v_d;
int 		startx, starty;
double 		colratio, rowratio;
{
   int j, k, l0, t0, itmp, dx, dy, initl, tmp;

/* VARIABLES:
	k	= the remainder after dividing the screen width/height
		     by the number of strata
	dx	= how many screen cols per stratum 
	dy	= how many screen rows per stratum 
	l0	= left side of screen + dx
	t0	= top of screen + dy
	w_w	= width of screen
	w_l	= height of screen
	h_d	= number of horizontal strata
	v_d	= number of vertical strata
*/

   R_open_driver();
   R_standard_color(D_translate_color("orange"));
   if (startx > 0) {
      dx = (int)((double)((int)(colratio *
         ((int)((double)(w_w-startx)/(double)(h_d))))/colratio + 0.5));
      l0 = l + startx;
   }
   else {
      dx = (int)((double)((int)(colratio *
         ((int)((double)(w_w)/(double)(h_d))))/colratio + 0.5));
      l0 = l;
   }
   if (starty > 0) {        
      dy = (int)((double)((int)(rowratio *
         ((int)((double)(w_l-starty)/(double)(v_d))))/rowratio + 0.5));

      t0 = t + starty;
   }
   else {
      dy = (int)((double)((int)(rowratio *
         ((int)((double)(w_l)/(double)(v_d))))/rowratio + 0.5));
      t0 = t;
   }
   initl = l0;
    
/*printf("l=%d t=%d w_w=%d w_l=%d h_d=%d v_d=%d startx=%d starty=%d l0=%d t0=%d dx=%d dy=%d\n",l,t,w_w,w_l,h_d,v_d,startx,starty,l0,t0,dx,dy);
*/

					/* draw the vertical strata */ 

   for (j = 1; j <= h_d - 1; j++){
      l0 += dx;	        
      R_move_abs(l0, t0);
      R_cont_rel(0, w_l - starty);	     
   }

					/* draw the horizontal strata */

   for (j = 1; j <= v_d - 1; j++){
      t0 += dy;
      R_move_abs(initl, t0);
      R_cont_rel(w_w - startx, 0);
   }

   R_close_driver();
}







				/* CALCULATE THE COORDINATES OF THE 
				   TOP LEFT CORNER OF THE SAMPLING
				   UNITS */

int  calc_unit_loc(top, bot, left, right, ratio, u_w, u_l, method, intv, 
                   num, h_d, v_d, ux, uy, sites, startx, starty, fmask, nx,
                   x, y)
int     top, bot, left, right, u_w, u_l, method, num, h_d, v_d, *ux, 
	*uy, *sites, startx, starty, fmask; 
double  ratio, intv, x, y;
{
  char	  *sites_mapset, sites_file_name[30], *desc;
  FILE	  *sites_fp;
  struct  Cell_head region;
  double  east_coord, north_coord;
  int     i, j, k, cnt=0, w_w = right - left, w_l = bot - top, exp1, exp2,
          dx = w_w, dy = w_l, l, t, left1 = left, top1 = top, n, tmp,
	  ulrow, ulcol, *row_buf, lap=0;

/*   VARIABLES:
	UNITS FOR ALL DIMENSION VARIABLES IN THIS ROUTINE ARE IN CELLS

	top	=	sampling frame top row in cells
	bot	=	sampling frame bottom row in cells
	left	=	sampling frame left in cells
	right	=	sampling frame right in cells
	left1	=	col of left side of sampling frame or each stratum
	top1	=	row of top side of sampling frame or each stratum
        l	=	random # cols to be added to left1
	r	=	random # rows to be added to top1
	ratio	=	
	u_w	=	sampling unit width in cells
	u_l	=	sampling unit length in cells
	method	=	method of sampling unit distribution (1-5)
	intv	=	interval between sampling units when method=3
	num	=	number of sampling units
	h_d	=	number of horizontal strata
	v_d	=	number of vertical strata
	ux	=
	uy	=
	sites	=
	startx	=	col of UL corner starting pt for strata
	starty	=	row of UL corner starting pt for strata
	dx	=	number of cols per stratum
	dy	=	number of rows per stratum
	w_w	=	width of sampling frame in cols
	w_l	=	length of sampling frame in rows
*/


				/* if user hits Ctrl+C, abort this 
				   calculation */

  setjmp(jmp);
  if (tag) {  
     tag = 0;
     return 0;
  }

				/* for syst. noncontig. distribution */

  if (method==3) {
     u_w += intv;  
     u_l += intv;
  }

				/* for stratified random distribution */

  if (method == 4){
     dx = (int)((double)(w_w - startx) / (double)(h_d));
     dy = (int)((double)(w_l - starty) / (double)(v_d));
  } 

				/* for syst. contig. and noncontig.
				   distribution */

  else if (method == 2 || method == 3) {	
     if (nx >= num)
        dx = (w_w-startx) - (num-1)*u_w;
     else {
        dx = (w_w-startx) - (nx-1)*u_w;
        dy = (w_l-starty) - (num/nx-1)*u_l;
     }
  }
 
  if (10 > (exp1 = (int)pow(10.0, ceil(log10((double)(dx-u_w+10)))))) exp1 = 10;
  if (10 > (exp2 = (int)pow(10.0, ceil(log10((double)(dy-u_l+10)))))) exp2 = 10;

				/* for random nonoverlapping and stratified 
				   random */

  if (method == 1 || method == 4){

     printf("\n   'Ctrl+C' and choose fewer units if the requested number");
     printf(" is not reached\n");

     for(i = 0; i < num; i++) {
      
				/* if Cntl+C */

        if (signal(SIGINT, SIG_IGN) != SIG_IGN) signal(SIGINT, f); 

				/* for stratified random distribution */

        if (method == 4) {
	   j = 0;
	   if (n = i % h_d)	   
              left1 += dx;
           else {
	      left1 = left + startx;
	      if (i < h_d)
	       top1 = top + starty;
	      else
	         top1 += dy;
	   }
           get_rd(exp1, exp2, dx, dy, u_w, u_l, &l, &t);

        }

/*printf("\n n=%d i=%d h_d=%d dx=%d dy=%d tmp=%d left=%d left1=%d top=%d top1=%d bottom=%d right=%d\n",n,i,h_d,dx,dy,tmp,left,left1,top,top1,bot,right);
*/

				/* for random nonoverlapping distribution */

        if (method == 1){

 				/* get random numbers */
back: 
           get_rd(exp1, exp2, dx, dy, u_w, u_l, &l, &t);

/*printf("left1=%d l=%d left=%d right=%d top1=%d t=%d top=%d bot=%d\n",left1,l,left,right,top1,t,top,bot);
*/
	   if (left1 + l + u_w > right || top1 + t + u_l > bot ||
	      left1 + l < left || top1 + t < top)
	      goto back;

				/* if there is a mask, check to see that
				   the unit will be within the mask area */

	   if (fmask > 0) {
	      row_buf = G_allocate_cell_buf();

/*printf("A l=%d left1=%d u_w=%d t=%d top1=%d u_l=%d t+top1=%d t+top1+u_w=%d w_l=%d w_w=%d dx=%d dy=%d\n",l,left1,u_w,t,top1,u_l,t+top1,t+top1+u_w,w_l,w_w,dx,dy);
*/
	      G_get_map_row_nomask(fmask, row_buf, t+top1);
	      if (!(*(row_buf+l+left1) && *(row_buf+l+left1+u_w-1)))
		 goto back; 
	      G_zero_cell_buf (row_buf);
	      G_get_map_row_nomask(fmask, row_buf, t+top1+u_l-1);
	      if (!(*(row_buf+l+left1) && *(row_buf+l+left1+u_w-1)))
		 goto back; 
	      free(row_buf);
	   }

				/* check for sampling unit overlap */

	   lap = 0; 
           for (j = 0; j < cnt; j++) {
	      if (overlap(l+left1, t+top1, ux[j], uy[j], u_w, u_l))
	         lap = 1;
	   }
           if (lap)
	      goto back;

           cnt ++;
        }
				/* fill the array of upper left coordinates 
				   for the sampling units */
	 	               
        *(ux+i) = l + left1;	
        *(uy+i) = t + top1;
 
				/* draw the sampling units on the 
				   screen */
  
	R_open_driver();
        R_standard_color(D_translate_color("red"));
	draw_box((int)((double)(ux[i])/x), (int)((double)(uy[i])/y), 
		 (int)((double)(ux[i]+u_w)/x), (int)((double)(uy[i]+u_l)/y),
		 1);
	R_close_driver();
        printf("    Distributed unit %4d of %4d requested\r",i+1,num);
     }
  } 

				/* for syst. contig. & syst. noncontig. */

  else if (method == 2 || method == 3) {
     for(i = 0; i < num; i++) { 
        *(ux + i) = left + startx + u_w * (i - nx*floor((double)i/nx));
        *(uy + i) = top + starty + u_l * floor((double)i/nx);
     }  
  }

				/* for centered over sites */

  else if (method==5){
     do {  
        sites_mapset=G_ask_sites_old("    Enter name of site map", sites_file_name) ;
     }  while (sites_mapset == NULL);
     if ((sites_fp = G_fopen_sites_old(sites_file_name,sites_mapset)) == NULL)
        printf("\nCan't open sites file %s\n",sites_file_name);
     *sites = 0;
     i = 0;
     n = 0;
     while( G_get_site(sites_fp,&east_coord,&north_coord,&desc) > 0) {              
	ulcol = ((int)(D_u_to_a_col(east_coord))) + 1 - u_w/2;
	ulrow = ((int)(D_u_to_a_row(north_coord))) + 1 - u_l/2;            
	if (ulcol <= left || ulrow <= top || ulcol+u_w-1 > right || ulrow+u_l-1 > bot) {
	   printf("    No sampling unit over site %d at east=%8.1f north=%8.1f\n",
	      n+1,east_coord,north_coord);
	   printf("       as it would extend outside the map\n");
	}
	else {
	   *(ux+i) = ulcol-1;
	   *(uy+i) = ulrow-1;            
	   i++;  
	}             
	n++;
	if (n > 250)
           G_fatal_error("There are more than the maximum of 250 sites\n");	   
     }
     printf("    Total sites with sampling units = %d\n",i);
     *sites = i;           
  }

  return 1;

}




				/* FIND THE CORRECT RANDOM NUMBER */

void  get_rd(exp1, exp2, dx, dy, u_w, u_l, l, t)
int   exp1, exp2, u_w, u_l, dx, dy, *l, *t;
{   
   int  rd;

   do{
     rd = rand();
     *l = rd % exp1;
     *t = rd % (exp1*exp2) / exp1 % exp2; 
   } while(dx < *l + u_w || dy < *t + u_l);
}



				/* */

void  f()
 {
  tag = 1;
  longjmp(jmp, 1);
 }





				/* CHECK IF 2 SAMPLING UNITS OVERLAP */

int  overlap(x1, y1, x2, y2, dx, dy)
int x1, y1, x2, y2, dx, dy;
{
  if (x1 >= x2+dx || x2 >= x1+dx || y1 >= y2+dy || y2 >= y1+dy) return 0;
  else return 1;
}







				/* CALCULATE MAXIMUM POSSIBLE NUMBER
				   OF SAMPLING UNITS */

int  calc_num(w_w, w_l, ratio, u_w, u_l, method, intv, startx, starty, 
	size, count) 
int  w_w, w_l, u_w, u_l, method, startx, starty, size, count;
double  ratio, intv;
{
  int        nx, ny, max;

				/* for random nonoverlapping */

  if (method == 1) {
     max = count/size;      
  }

				/* for syst. contig. distribution */

  else if (method == 2) {	
     nx = floor((double)(w_w - startx) / u_w);
     ny = floor((double)(w_l - starty) / u_l);
     max = nx*ny;
  }

				/* for syst. noncontig. distribution */
  
  else if (method==3) {
     nx = floor((double)(w_w - startx) / (u_w + intv));
     ny = floor((double)(w_l - starty) / (u_l + intv));
     max = nx*ny;
  } 
  return max;
}






				/* USE THE MOUSE TO DEFINE SAMPLING
				   UNITS GRAPHICALLY */

void  graph_unit(t, b, l, r, n1, n2, n3, mx, fmask)
double *mx;
int    t, b, l, r, fmask;
char   *n1, *n2, *n3;
{
  int  		 x0=0, y0=0, xp, yp, ux[250], uy[250], u_w, u_l, btn=0, k=0,
		 w_w=0, w_l=0, *row_buf, at, ab, al, ar,
		 tmpw, tmpl, au_w, au_l, lap=0, l0=0, r0=0, t0=0, b0=0;
  FILE           *fp;
  double	 tmp;
  register int   i, j;

/*  VARIABLES:
	COORDINATES IN THIS ROUTINE ARE IN CELLS

	t	=	top row of sampling frame
	b	=	bottom row of sampling frame
	l	=	left col of sampling frame
	r	=	right col of sampling frame
	n1	=
	n2	=
	n3	=
	mx[0]	=	cols of region/width of screen
	mx[1]	=	rows of region/height of screen

*/


  l0 = l;
  r0 = r;
  t0 = t;
  b0 = b;
 
  l = (int)((double)(l * mx[0]) + 0.5);
  r = (int)((double)(r * mx[0]) + 0.5);
  t = (int)((double)(t * mx[1]) + 0.5);
  b = (int)((double)(b * mx[1]) + 0.5);
  w_w = r-l;
  w_l = b-t;

				/* draw the sampling frame */

  R_open_driver();  
  R_standard_color(D_translate_color("grey"));
  draw_box((int)(l/mx[0]), (int)(t/mx[1]), (int)(r/mx[0]), (int)(b/mx[1]), 1);
  R_close_driver();

  fp = fopen0("r.le.para/units", "w");
  G_sleep_on_error(0);

  				/* get the number of scales */

  do {
     printf("\n    How many different SCALES do you want? (1-15)  ");
     numtrap(1, &tmp);
     if (tmp <1 || tmp > 15)
	printf("    Too many (>15) or too few scales, try again.\n");
  } while(tmp < 1 || tmp > 15);
  fprintf(fp, "%10d    # of scales\n", (int)(tmp));
  
  				/* for each scale */

  for(i = 0; i < tmp; i++){
     G_system("clear");
     printf("\n    Outline the standard sampling unit of scale %d.\n", i+1);
     printf("       Left button:     Check unit size\n");
     printf("       Middle button:   Move cursor\n");
     printf("       Right button:    Lower right corner of unit here\n");

     R_open_driver();
 
     do {
back1:
        R_get_location_with_box(x0, y0, &xp, &yp, &btn);

				/* convert the left (x0), right (y0),
				   top (y0), bottom (yp) coordinates in 
				   screen pixels to the nearest row and
				   column; do the same for the sampling
				   unit width (u_w) and height (u_l);
				   then convert back */ 

	ar = (int)((double)(xp)*mx[0] + 0.5);
	xp = (int)((double)(ar)/mx[0] + 0.5);
	al = (int)((double)(x0)*mx[0] + 0.5);
	x0 = (int)((double)(al)/mx[0] + 0.5);
        au_w = ar-al;
        u_w = (int)((double)(au_w)/mx[0] + 0.5); 
	ab = (int)((double)(yp)*mx[1] + 0.5);
        yp = (int)((double)(ab)/mx[1] + 0.5);
	at = (int)((double)(y0)*mx[1] + 0.5);
        y0 = (int)((double)(at)/mx[1] + 0.5); 
 	au_l = ab-at;
        u_l = (int)((double)(au_l)/mx[1] + 0.5); 

 				/* left button, check the size of the rubber
				   box in array system */

        if (btn == 1) {
           fprintf(stdout,"\n     Width=%d  length=%d  w/l=%5.2f  size %d"); 
	   fprintf(stdout," cells\r", au_w, au_l, (double)(au_w)/(double)(au_l),
	      au_w*au_l);
	}

				/* mid button, move the start point of the
				   rubber box */

	else if (btn == 2) {  
           R_move_abs(xp, yp);
           x0 = xp;
           y0 = yp;   
        }

				/* right button, outline the unit */
 
	else if ( btn == 3) {

	   if (ar > r || ab > b || al < l || at < t) {
	      printf("\n    The unit would be outside the map; try again\n");
	      goto back1; 
   	   }

	   if (au_w > w_w || au_l > w_l) {
	      printf("\n    The unit is too big for the sampling frame; ");
	      printf("try again\n");
	      goto back1;
	   }

				/* if there is a mask, check to see that
				   the unit will be within the mask area,
				   by checking to see whether the four 
				   corners of the unit are in the mask */

	   if (fmask > 0) {
	      row_buf = G_allocate_cell_buf();
	      G_get_map_row_nomask(fmask, row_buf, at);
	      if (!(*(row_buf + al) && *(row_buf + ar - 1))) {
		 printf("\n    The unit would be outside the mask; ");
		 printf("try again\n");
	         free(row_buf);
		 goto back1;
 	      }
	      G_zero_cell_buf (row_buf);
	      G_get_map_row_nomask(fmask, row_buf, ab-1);
	      if (!(*(row_buf + al) && *(row_buf + ar - 1))) {
		 printf("\n    The unit would be outside the mask; ");
		 printf("try again\n");
	         free(row_buf);
		 goto back1;
	      } 
	      free(row_buf);
	   }

	   if (xp-x0 && yp-y0) {  
              R_standard_color(D_translate_color("red"));
	      draw_box(x0, y0, xp, yp, 1);
	      G_system("clear");
	      printf("\n\n    The standard sampling unit has:\n");
	      printf("       columns = %d    rows = %d\n", ar-al, ab-at);
 	      printf("       width/length ratio = %5.2f\n",
                 (double)(ar-al)/(double)(ab-at));
	      printf("       size = %d cells\n",(ar-al)*(ab-at));
	      k = 0;
              ux[0] = al;
	      uy[0] = at;
	   }
	   else {
	      printf("\n    Unit has 0 rows and 0 columns; try again\n");
	      goto back1;
	   }
        }
     } while(btn != 3);
     R_close_driver();

     				/* use the size and shape of the 
				   standard unit to outline more units
				   in that scale */

     printf("\n    Outline more sampling units of scale %d?\n", i+1);
     printf("       Left button:     Exit\n");
     printf("       Middle button:   Not used\n");
     printf("       Right button:    Lower right corner of next unit here\n");  
     R_open_driver();

				 /* if not the left button (to exit) */ 

back2:
     while(btn != 1) {  
        R_get_location_with_box(xp-u_w, yp-u_l, &xp, &yp, &btn);

				/* convert the left (x0), right (y0),
				   top (y0), bottom (yp) coordinates in 
				   screen pixels to the nearest row and
				   column; do the same for the sampling
				   unit width (u_w) and height (u_l);
				   then convert back */ 

	ar = (int)((double)(xp)*mx[0] + 0.5);
	ab = (int)((double)(yp)*mx[1] + 0.5);
	xp = (int)((double)(ar)/mx[0] + 0.5);
        yp = (int)((double)(ab)/mx[1] + 0.5);
	al = (int)((double)(xp-u_w)*mx[0] + 0.5);
	at = (int)((double)(yp-u_l)*mx[0] + 0.5);
	x0 = (int)((double)(al)/mx[0] + 0.5);
        y0 = (int)((double)(at)/mx[1] + 0.5); 


				  /* if right button, outline the unit */

        if (btn == 3){

	   if (ar > r || ab > b || al < l || at < t) {
	      printf("\n    The unit would be outside the map; try again");
	      goto back2;
   	   }

				/* if there is a mask, check to see that
				   the unit will be within the mask area */

	   if (fmask > 0) {
	      row_buf = G_allocate_cell_buf();
	      G_get_map_row_nomask(fmask, row_buf, at);
	      if (!(*(row_buf + al) && *(row_buf + ar - 1))) {
		 printf("\n    The unit would be outside the mask; ");
		 printf("try again");
		 free(row_buf);
		 goto back2;
 	      }
	      G_zero_cell_buf (row_buf);
	      G_get_map_row_nomask(fmask, row_buf, ab-1);
	      if (!(*(row_buf + al) && *(row_buf + ar - 1))) {
		 printf("\n    The unit would be outside the mask; ");
		 printf("try again");
	         free(row_buf);
		 goto back2;
	      } 
	      free(row_buf);
	   }

				/* check for sampling unit overlap */
 
	   lap = 0;
           for (j = 0; j < k + 1; j++) {
              if (overlap(al, at, ux[j], uy[j], au_w, au_l)) {
	         printf("\n    The unit would overlap a previously drawn "); 
	         printf("unit; try again");
	            lap = 1;
	      }
           }
	   if (lap)
	      goto back2;

	   k ++;
           printf("\n    %d sampling units have been placed", (k + 1));  	    
	   ux[k] = al;
	   uy[k] = at;
           R_standard_color(D_translate_color("red"));
           draw_box(x0, y0,  xp, yp, 1);
        } 
     }
     R_close_driver();

     				/* save the sampling units in the
				   r.le.para/units file */

     fprintf(fp, "%10d    # of units of scale %d\n", k+1, i+1);
     fprintf(fp, "%10d%10d   u_w, u_l of units in scale %d\n",
        (int)(u_w*mx[0]), (int)(u_l*mx[1]), i+1);
     for(j = 0; j < k + 1; j++)
        fprintf(fp, "%10d%10d   left, top of unit[%d]\n", ux[j], uy[j], j+1);

     if (i < tmp-1 && G_yes("\n    Refresh the screen?   ", 1)) {
	paint_map(n1, n2, n3);
        R_open_driver();
        R_standard_color(D_translate_color("red"));
        R_close_driver();
     }
  }
 
 fclose(fp);
}





				/* DRAW A RECTANGULAR BOX WITH 
				   THICKNESS OF "THICK" */

void draw_box(x0, y0, xp, yp, thick)
int  x0, y0, xp, yp, thick;
{
  int i;

  for(i=0; i<= thick; i++){
     R_move_abs(x0+i, y0+i);
     R_cont_abs(x0+i, yp-i);
     R_cont_abs(xp-i, yp-i);
     R_cont_abs(xp-i, y0+i);
     R_cont_abs(x0+i, y0+i);

     R_move_abs(x0-i, y0-i);
     R_cont_abs(x0-i, yp+i);
     R_cont_abs(xp+i, yp+i);
     R_cont_abs(xp+i, y0-i);
     R_cont_abs(x0-i, y0-i);
   }
}	
 




				/* READ USER DIGITAL INPUT FROM THE SCREEN */

void numtrap(n, a)
int    n;
double *a;
{
  char   num[31], *s;
  int    i=0, j, k=1, c;


  while (i < n){

     s = num;

				/* find the first period, minus sign, 
				   or digit in the user input */

     while((c=getchar()) != '.' && c != '-' && !isdigit(c));	

				/* if it is a decimal pt, then get
				   characters as long as they are
				   digits and append them to the 
				   end of the num array */

 
     if (c == '.') {
	*s++ = c;
	while ((c=getchar()) && isdigit(c) && k < 30) {
	   *s++ = c;
	   k++;
	}
     }
           

				/* if it is not a period, but
				   is a minus sign or digit, then
				   get characters as long as they 
				   are digits and append them to the
				   end of the num array. If a decimal
				   pt is found, then append this also
				   and keep getting digits */

     else if (isdigit(c) || c=='-'){   
	*s++ = c;
	while ((c=getchar()) && isdigit(c) && k < 30) {
	   *s++ = c;
	   k++;
	}
        if (c == '.') {
	   *s++ = c;
	   while ((c=getchar()) && isdigit(c) && k < 30) {
	      *s++ = c;
	      k++;
	   }
        }
     }

     *s = '\0';

				/* once the user's input has been
				   read, convert the digits of the num
				   array into a double and store it
				   in the i++ element of the "a" array */ 

     *(a + i++) = atof(num);

				/* keep getting characters as long as
				   they are not spaces, commas, tabs,
				   or end of line */

     while(c != ' ' && c != ',' && c != '\t' && c != '\n') c = getchar();
  }

				/* print the user input on the screen */

  for(i = 0; i < n; i++)
     printf(" (Input[%d] = %.3f)\n", i+1, *(a+i));
}

