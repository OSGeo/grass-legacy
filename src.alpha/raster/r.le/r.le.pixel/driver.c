                 		/********************************/
		                /* 	r.le.pixel/driver.c     */
                                /*                              */
				/*		2.1		*/
				/*				*/
                                /*      07/05/94 version        */
                                /*                              */
				/*      Programmer: Baker	*/
				/*	Univ. of Wyoming	*/
		                /********************************/
#include "sys/types.h"
#include "sys/times.h"


#include "r.le.pixel.h"

extern struct CHOICE *choice;
int	finput, g_scale=1, g_unit=1;
char 	cmdbuf[100]; 


					/* RUN R.LE.PIXEL IN FOREGROUND */

void  texture_fore()
{  
  FILE  *fp0, *fp1, *fp2, *fp3, *fp4, *fp5 ;

  fputs("\nR.LE.PIXEL IS WORKING....;\n\n", stderr);

					/* check for input raster map */

  if(0 > (finput = G_open_cell_old(choice->fn, G_mapset()))) {
     printf("\n");
     printf("   ********************************************************\n");
     printf("    The raster map you specified with the 'map=' parameter \n");
     printf("    was not found in your mapset.                          \n");
     printf("   ********************************************************\n");
     exit(1);
  }

					/* if running moving window, 
					   get the parameters, and start the
					   moving window driver */

  if (choice->wrum == 'm')
     mv_driver();

  else {

     if (choice->att[0]) {
        fp0 = fopen0("r.le.out/b1-4.out", "w");
        fclose(fp0);
     } 

     if (choice->div[0]) {
        fp1 = fopen0("r.le.out/d1-4.out", "w");
        fclose(fp1);
     } 

     if (choice->te2[0]) {
        fp2 = fopen0("r.le.out/t1-5.out", "w");
        fclose(fp2);
     } 

     if (choice->jux[0]) {
        fp3 = fopen0("r.le.out/j1-2.out", "w");
        fclose(fp3);
     }

     if (choice->edg[0]) {
        if (choice->edg[1]) {
           fp4 = fopen0("r.le.out/e1.out", "w");
           fclose(fp4);
	}
        if (choice->edg[2]) {
           fp5 = fopen0("r.le.out/e2.out", "w");
           fclose(fp5);
	}
     }

     if (choice->wrum == 'w' || choice->wrum == 'r') 
        whole_reg_driver();
     else if (choice->wrum == 'u')
        unit_driver();
  }

  G_close_cell(finput);

  fputs("R.LE.PIXEL IS DONE;  ", stderr);

  if (choice->wrum != 'm')
     fputs("OUTPUT FILES IN SUBDIRECTORY \"r.le.out\"\n", stderr);

}




					/* MOVING WINDOW DRIVER */

void  mv_driver()

/* variables: nc = #cols. in search area minus (1/2 width of mov. wind. + 1) =
			number of cols with values in out map
	      nr = #rows in search area minus (1/2 height of mov. wind. + 1) =
			number of rows with values in out map
	      x0 = starting column for upper L corner of search area
	      y0 = starting row for upper L corner of search area
	     u_w = width of mov. wind. in cells
	     u_l = width of mov. wind. in cells
*/

{
  register int  i, j;
  int      nr, nc, u_w, u_l, x0, y0, fd, fin, row, col, *buf, 
	   d, fmask, *row_buf, *tmp_buf, m, p, *richwhole, *att_buf, 
	   cntwhole=0;
  int	   b1, b2, b3, b4, d1, d2, d3, d4, t1, t2, t3, t4, t5, j1, j2, e1, e2;
  long     finished_time;
  int	   **buff=NULL;
  struct Cell_head wind;


					/* open the appropriate output
					   maps */

  if(choice->att[1]) b1 = G_open_cell_new_random("b1");
  if(choice->att[2]) b2 = G_open_cell_new_random("b2");
  if(choice->att[3]) b3 = G_open_cell_new_random("b3");
  if(choice->att[4]) b4 = G_open_cell_new_random("b4");

  if(choice->div[1]) d1 = G_open_cell_new_random("d1");
  if(choice->div[2]) d2 = G_open_cell_new_random("d2");
  if(choice->div[3]) d3 = G_open_cell_new_random("d3");
  if(choice->div[4]) d4 = G_open_cell_new_random("d4");

  if(choice->te2[1]) t1 = G_open_cell_new_random("t1");
  if(choice->te2[2]) t2 = G_open_cell_new_random("t2");
  if(choice->te2[3]) t3 = G_open_cell_new_random("t3");
  if(choice->te2[4]) t4 = G_open_cell_new_random("t4");
  if(choice->te2[5]) t5 = G_open_cell_new_random("t5");

  if(choice->jux[1]) j1 = G_open_cell_new_random("j1");
  if(choice->jux[2]) j2 = G_open_cell_new_random("j2");

  if(choice->edg[1]) e1 = G_open_cell_new_random("e1");
  if(choice->edg[2]) e2 = G_open_cell_new_random("e2");


  					/* get the moving window parameters */

  read_mwind(&u_w, &u_l, &nc, &nr, &x0, &y0);

  					/* check for an unacceptable 
					   moving-window size */

  if(nc < 1 || nr < 1) {
     printf("\n");
     printf("   *******************************************************\n");
     printf("    The moving window size specified in file r.le.para/   \n");
     printf("    move_wind is less than 1 row or column.  Check this   \n");
     printf("    file or redefine the moving window using r.le.setup.  \n");
     printf("   *******************************************************\n");
     exit(1);
  }

  					/* check for an unacceptable
					   search area */

  G_get_set_window(&wind);
  if(wind.rows < nr + y0 || wind.cols < nc + x0) {
     printf("\n");
     printf("   *******************************************************\n");
     printf("    Moving window search area in file r.le.para/move_wind \n");
     printf("    does not match the dimensions of the current region.  \n");    
     printf("    You must either rerun r.le.setup to make a new        \n"); 	
     printf("    r.le.para/move_wind file or reset the region to match \n");    
     printf("    the r.le.para/move_wind file 		               \n");    
     printf("   *******************************************************\n");
     exit(1);
  }


					/* begin main moving window loop 
					   section */

					/* set the d parameter for the 
					   performance meter */

  if(nr*nc > 10000)
      d = nr*nc/1000;
  else if(nr*nc > 2500)
      d = nr*nc/100;
  else 
      d = 10;
  					/* return a value > 0 to fmask if
					   there is a MASK present */

  printf("If a MASK is not present (see r.mask) a beep will sound and a\n");
  printf("   warning will be printed that can be ignored.\n");
  printf("If a MASK is present there will be no warning.\n");
  fmask = G_open_cell_old("MASK",G_mapset()) ;
  printf("\n") ;

					/* allocate memory for the buffer */

  buff = (int **)G_calloc(nc + 1, sizeof(int *));

					/* allocate memory for each measure */

  for(p = 0; p < nc + 1; p++)
     buff[p] = (int *)G_calloc(17, sizeof(int)); 

					/* allocate memory for a row buffer if
					   there is a mask */

  if (fmask > 0)                
     row_buf = G_allocate_cell_buf();


  if (choice->edg[2] || choice->jux[0]) {

					/* dynamically allocate memory for
					   the richness array */

     richwhole = (int *)G_calloc(MAX, sizeof(int));

					/* initialize the richness array
					   elements with the value -999 */

     for(i = 0; i < MAX; i++)	
        richwhole[i] = -999;

     att_buf = G_allocate_cell_buf();

 					/* go through the search area
					   pixel by pixel */

     for(i = 0; i < nr + 2; i++) {
	G_zero_cell_buf(att_buf);
	G_get_map_row(finput, att_buf, i);
        for(j = 0; j < nc + 2; j++) {
           if (*(att_buf+j)) 

					/* call get_rich_whole to tally up the
					   number of different attributes
					   in the search area and fill
					   the richness array with those
					   attributes */

	      get_rich_whole(*(att_buf+j), richwhole, &cntwhole);
        }
     }
     free(att_buf);
     free(richwhole);
  }

					/* main loop for clipping & measuring 
					   using the moving-window; index i
					   refers to which moving window, not
					   the row of the original map */

  for(i = 0; i < nr; i++) {

					/* zero the buffer before filling it
					   again */

     for (m = 0; m < nc + 1; m++) {
	for (p = 0; p < 17; p++)
	   *(*(buff + m) + p) = 0;
     }

    					/* if there is a MASK, then read in 
					   a row of MASK - this part skips 
					   cells with the value "0" in the 
					   MASK to speed up the moving window
					   process */

     if (fmask > 0) {                  
	G_zero_cell_buf(row_buf);
	G_get_map_row_nomask(fmask, row_buf, y0+i+u_l/2);

        				/* for each cell whose value is "1" 
					   in MASK */

        for (j = 0; j < nc; j++) {

           				/* display #cells left to do */

           if (i==0 && j==0)
	      fprintf (stdout,"TOTAL WINDOWS = %8d\n",nr*nc);
           meter(nr*nc, (i*nc + (j+1)), d);    

					/* call the cell clip driver */

 	   if (row_buf[x0+j+u_w/2])
	      cell_clip_drv(x0+j, y0+i, u_w, u_l, buff, j, cntwhole);
	}
     }

    					/* if there is no MASK, then clip
					   and measure at every cell */

     else {
                 
        for (j = 0; j < nc; j++) {

           				/* display #cells left to do */

           if (i==0 && j==0)
	      fprintf (stdout,"TOTAL WINDOWS = %8d\n",nr*nc);
           meter(nr*nc, (i*nc + (j+1)), d);

					/* call the cell clip driver */

	   cell_clip_drv(x0+j, y0+i, u_w, u_l, buff, j, cntwhole);

	}
    }

    					/* get the moving-window center */

    row = i+ y0 + u_l/2;   
    col = x0 + u_w/2;


					/* copy the chosen measures into
					   a row buffer which is then fed
					   into the chosen output maps */

    tmp_buf = G_allocate_cell_buf();

    if(choice->att[1]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 0);
       G_put_map_row_random(b1, tmp_buf, row, col, nc+1);
    }
    if(choice->att[2]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 1);
       G_put_map_row_random(b2, tmp_buf, row, col, nc+1);
    }
    if(choice->att[3]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 2);
       G_put_map_row_random(b3, tmp_buf, row, col, nc+1);
    }
    if(choice->att[4]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 3);
       G_put_map_row_random(b4, tmp_buf, row, col, nc+1);
    }
    if(choice->div[1]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 4);
       G_put_map_row_random(d1, tmp_buf, row, col, nc+1);
    }
    if(choice->div[2]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 5);
       G_put_map_row_random(d2, tmp_buf, row, col, nc+1);
    }
    if(choice->div[3]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 6);
       G_put_map_row_random(d3, tmp_buf, row, col, nc+1);
    }
    if(choice->div[4]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 7);
       G_put_map_row_random(d4, tmp_buf, row, col, nc+1);
    }
    if(choice->te2[1]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 8);
       G_put_map_row_random(t1, tmp_buf, row, col, nc+1);
    }
    if(choice->te2[2]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 9);
       G_put_map_row_random(t2, tmp_buf, row, col, nc+1);
    }
    if(choice->te2[3]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 10);
       G_put_map_row_random(t3, tmp_buf, row, col, nc+1);
    }
    if(choice->te2[4]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 11);
       G_put_map_row_random(t4, tmp_buf, row, col, nc+1);
    }
    if(choice->te2[5]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 12);
       G_put_map_row_random(t5, tmp_buf, row, col, nc+1);
    }
    if(choice->jux[1]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 13);
       G_put_map_row_random(j1, tmp_buf, row, col, nc+1);
    }
    if(choice->jux[2]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 14);
       G_put_map_row_random(j2, tmp_buf, row, col, nc+1);
    }
    if(choice->edg[1]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 15);
       G_put_map_row_random(e1, tmp_buf, row, col, nc+1);
    }
    if(choice->edg[2]) {
       G_zero_cell_buf(tmp_buf);
       for (m=0; m<nc; m++)
	  *(tmp_buf + m) = *(*(buff + m) + 16);
       G_put_map_row_random(e2, tmp_buf, row, col, nc+1);
    }


    free(tmp_buf);
  }

  time(&finished_time); 
  fprintf (stdout,"\nACTUAL COMPLETION = %s",asctime(localtime(&finished_time)) );  
  fflush(stdout);

					/* free the memory allocated for the 
					   mask and other buffer */

  if (fmask > 0)
     free(row_buf);
  for (p=0; p<nc+1; p++)
     free(buff[p]);
  free(buff);

					/* close the raster files, set the
					   color table for the new raster
					   map and compress the map */


  if(choice->att[1]) {
     G_close_cell(b1);
     set_colors("b1");
     sprintf(cmdbuf,"%s %s","r.compress","b1") ;
     system(cmdbuf);
  }
  if(choice->att[2]) {
     G_close_cell(b2);
     set_colors("b2");
     sprintf(cmdbuf,"%s %s","r.compress","b2") ;
     system(cmdbuf);
  }
  if(choice->att[3]) {
     G_close_cell(b3);
     set_colors("b3");
     sprintf(cmdbuf,"%s %s","r.compress","b3") ;
     system(cmdbuf);
  }
  if(choice->att[4]) {
     G_close_cell(b4);
     set_colors("b4");
     sprintf(cmdbuf,"%s %s","r.compress","b4") ;
     system(cmdbuf);
  }
  if(choice->div[1]) {
     G_close_cell(d1);
     set_colors("d1");
     sprintf(cmdbuf,"%s %s","r.compress","d1") ;
     system(cmdbuf);
  }
  if(choice->div[2]) {
     G_close_cell(d2);
     set_colors("d2");
     sprintf(cmdbuf,"%s %s","r.compress","d2") ;
     system(cmdbuf);
  }
  if(choice->div[3]) {
     G_close_cell(d3);
     set_colors("d3");
     sprintf(cmdbuf,"%s %s","r.compress","d3") ;
     system(cmdbuf);
  }
  if(choice->div[4]) {
     G_close_cell(d4);
     set_colors("d4");
     sprintf(cmdbuf,"%s %s","r.compress","d4") ;
     system(cmdbuf);
  }
  if(choice->te2[1]) {
     G_close_cell(t1);
     set_colors("t1");
     sprintf(cmdbuf,"%s %s","r.compress","t1") ;
     system(cmdbuf);
  }
  if(choice->te2[2]) {
     G_close_cell(t2);
     set_colors("t2");
     sprintf(cmdbuf,"%s %s","r.compress","t2") ;
     system(cmdbuf);
  }
  if(choice->te2[3]) {
     G_close_cell(t3);
     set_colors("t3");
     sprintf(cmdbuf,"%s %s","r.compress","t3") ;
     system(cmdbuf);
  }
  if(choice->te2[4]) {
     G_close_cell(t4);
     set_colors("t4");
     sprintf(cmdbuf,"%s %s","r.compress","t4") ;
     system(cmdbuf);
  }
  if(choice->te2[5]) {
     G_close_cell(t5);
     set_colors("t5");
     sprintf(cmdbuf,"%s %s","r.compress","t5") ;
     system(cmdbuf);
  }
  if(choice->jux[1]) {
     G_close_cell(j1);
     set_colors("j1");
     sprintf(cmdbuf,"%s %s","r.compress","j1") ;
     system(cmdbuf);
  }
  if(choice->jux[2]) {
     G_close_cell(j2);
     set_colors("j2");
     sprintf(cmdbuf,"%s %s","r.compress","j2") ;
     system(cmdbuf);
  }
  if(choice->edg[1]) {
     G_close_cell(e1);
     set_colors("e1");
     sprintf(cmdbuf,"%s %s","r.compress","e1") ;
     system(cmdbuf);
  }
  if(choice->edg[2]) {
     G_close_cell(e2);
     set_colors("e2");
     sprintf(cmdbuf,"%s %s","r.compress","e2") ;
     system(cmdbuf);
  }

  G_close_cell(fmask);
}







					/* SET "OUT" RASTER FILE COLOR
					   TABLE TO G-Y-R */

void  set_colors(name)
char *name;
{
  struct Colors colors;
  struct Range  range;

  G_read_range(name, G_mapset(), &range);
  G_make_gyr_colors(&colors, range.pmin, range.pmax);
  G_write_colors(name, G_mapset(), &colors);
}






					/* OPEN OUTPUT FILE WITH ERROR TRAP */

FILE  *fopen0(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))){
     printf("\n");
     printf("   ******************************************\n");
     printf("    Can't open output file \"%s\"            \n",name);
     printf("    Do you have write permission in r.le.out \n");
     printf("    subdirectory?                            \n");    
     printf("   ******************************************\n");
     exit(1);
  }
  return fp;
}





					/* OPEN INPUT FILE WITH ERROR TRAP */

FILE  *fopen1(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))){
     printf("\n");
     printf("   ******************************************************\n");
     printf("    You chose a moving window or sampling units analysis \n");
     printf("       but r.le.pixel can't find file \"%s\"             \n",name);
     printf("       which defines the moving window or sampling units \n");
     printf("    First use r.le.setup for to setup a moving window or \n");
     printf("       sampling units to make this file                  \n"); 
     printf("   ******************************************************\n");
     exit(1);
  }
  return fp;
}



					/* OPEN WEIGHT FILE WITH ERROR TRAP */

FILE  *fopen2(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))){
     printf("\n");
     printf("   ***************************************************\n");
     printf("    You chose a juxtaposition measure, but r.le.pixel \n");
     printf("       can't find file \"%s\"             		   \n",name);
     printf("       which defines the weights for different edges  \n");
     printf("    First use a text editor to make this file         \n");   
     printf("   ***************************************************\n");
     exit(1);
  }
  return fp;
}



					/* OPEN EDGE FILE WITH ERROR TRAP */

FILE  *fopen3(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))){
     printf("\n");
     printf("   ***************************************************\n");
     printf("    You chose an edge measure, but r.le.pixel         \n");
     printf("       can't find file \"%s\"             		   \n",name);
     printf("       which defines the types of edges to be counted \n");
     printf("    First use a text editor to make this file         \n");   
     printf("   ***************************************************\n");
     exit(1);
  }
  return fp;
}



					/* READ IN THE MOVING WINDOW
					   PARAMETERS */

void  read_mwind(uw, ul, nc, nr, x0, y0)
int *uw, *ul, *nc, *nr, *x0, *y0;
{
  FILE  *fp;
  int   ww, wl;
  char  *buf;

  fp = fopen1("r.le.para/move_wind", "r");
  buf = G_malloc(513);
  
  fgets(buf, 512, fp);
  sscanf(buf, "%d%d", uw, ul);
  fgets(buf, 512, fp);
  sscanf(buf, "%d%d", &ww, &wl);
  fgets(buf, 512, fp);
  sscanf(buf, "%d%d", x0, y0);
  *nc = ww - *uw + 1;  *nr = wl - *ul + 1;

  free(buf);
  fclose(fp);
}






					/* PERFORMANCE METER - DISPLAYS
					   THE PROGRESS OF THE MOVING WINDOW
					   AS A COUNT AND COMPLETION TIME 
                                           WHILE THE PROGRAM RUNS */

void   meter(n, i, div)
int  n, i, div;
{ 
  long  current_time , time_left , elapsed , complete ;
  static long start ;
  float window_time ;
  static  int  k=0 ;
  char  done[30] ;
  int d ;

  *done = '\0' ;

  if(i <= 1){                  
     time(&start) ;
  }
 
  if( i <=10 )
      d = 1;
  else
      d=div;

  if(k> 2000){
     if(fseek(stdout, 0L, 0))
        G_fatal_error("Can't reset the \"stdout\", exit.\n");
     k = 0;
  }

 if((n-i) % d == 0 ){
 
    time(&current_time);
    elapsed = current_time - start ;
    window_time = ((float) elapsed) / (i+1) ;
    time_left =  (long)((n-i) * window_time) ;
    complete = current_time + time_left ;
    strncpy(done,asctime(localtime(&complete)),24) ;
    printf ("WINDOWS LEFT  = %8d     EST. COMPLETION = %s\r",(n-i),done );  
    fflush(stdout);
    k ++; 
  } 
 
}


					/* READ IN THE SAMPLING UNIT 
					   PARAMETERS AND RUN R.LE.PIXEL */

void  unit_driver()
{
  int              top, left, u_w, u_l, nscl, nu, fd, *richwhole, *att_buf;
  char 		   *buf, unitname[10], istr[3];
  register int     i, j, k, m;
  int              cntwhole=0;
  struct Cell_head wind;
  FILE             *fp;
  CELL		   **units, *unit_buf;

  G_get_set_window(&wind);

  fp = fopen1("r.le.para/units", "r");

  buf = G_malloc(513);

					/* get the number of scales */

  fgets(buf, 512, fp);
  sscanf(buf, "%d", &nscl);


  if (choice->edg[2] || choice->jux[0]) {

					/* dynamically allocate memory for
					   the richness array */

     richwhole = (int *)G_calloc(MAX, sizeof(int));

					/* initialize the richness array
					   elements with the value -999 */

     for(i = 0; i < MAX; i++)	
        richwhole[i] = -999;

     att_buf = G_allocate_cell_buf();

 					/* go through the search area
					   pixel by pixel */

     for(i = 0; i < wind.rows; i++) {
	G_zero_cell_buf(att_buf);
	G_get_map_row(finput, att_buf, i);
        for(j = 0; j < wind.cols; j++) { 
           if (*(att_buf+j)) 

					/* call get_rich_whole to tally up the
					   number of different attributes
					   in the search area and fill
					   the richness array with those
					   attributes */

	      get_rich_whole(*(att_buf+j), richwhole, &cntwhole);
        }
     }
     free(att_buf);
     free(richwhole);
  }

                      			/* dynamically allocate storage for the
                                   	   buffer that will hold the map of the
                                   	   sampling units */

  if (choice->units) {
     units = (CELL **)G_calloc(wind.rows + 3, sizeof(CELL *));
     for(i=0; i<wind.rows+3; i++)
        units[i] = (CELL *)G_calloc(wind.cols + 3, sizeof(CELL));
  }

					/* for each scale */

  for(i=0; i<nscl; i++) {
     g_scale = i+1;
     fgets(buf, 512, fp);
     sscanf(buf, "%d", &nu);
     fgets(buf, 512, fp);
     sscanf(buf, "%d%d", &u_w, &u_l);

     if (choice->units) {
        for (k=0; k<wind.rows + 3; k++) {
	   for (m=0; m<wind.cols + 3; m++)
	      *(*(units + k) + m) = 0;
        }

             if (i==0)  G_strcpy(istr,"1");
        else if (i==1)  G_strcpy(istr,"2");
        else if (i==2)  G_strcpy(istr,"3");
        else if (i==3)  G_strcpy(istr,"4");
        else if (i==4)  G_strcpy(istr,"5");
        else if (i==5)  G_strcpy(istr,"6");
        else if (i==6)  G_strcpy(istr,"7");
        else if (i==7)  G_strcpy(istr,"8");
        else if (i==8)  G_strcpy(istr,"9");
        else if (i==9)  G_strcpy(istr,"10");
        else if (i==10)  G_strcpy(istr,"11");
        else if (i==11)  G_strcpy(istr,"12");
        else if (i==12)  G_strcpy(istr,"13");
        else if (i==13)  G_strcpy(istr,"14");
        else if (i==14)  G_strcpy(istr,"15");
        else if (i > 14) {
           printf("\n");
           printf("   ***************************************************\n");
           printf("    You cannot choose more than 15 scales             \n");
           printf("   ***************************************************\n");
           exit(0);
        }
     }

					/* for each unit */

     for(j=0; j<nu; j++) {
	g_unit = j+1;
        fgets(buf, 512, fp);
        sscanf(buf, "%d%d", &left, &top);
           
		   			/* call cell_clip driver */

	run_clip(wind.cols, wind.rows, u_w, u_l, left, top, units, j,
	         cntwhole) ;
     } 

					/* if a map of the sampling units
					   was requested */
  
     if (choice->units) {
	G_strcpy(unitname,"units_");
	G_strcat(unitname,istr);
        fd = G_open_cell_new(unitname);
        unit_buf = G_allocate_cell_buf();
        for (k=1; k<wind.rows + 1; k++) {
           G_zero_cell_buf(unit_buf);
           for (m=1; m<wind.cols + 1; m++)
              *(unit_buf + m - 1) = *(*(units + k) + m);
           G_put_map_row(fd,unit_buf);
        }
        G_close_cell(fd);
        free(unit_buf);
     }
  }

  if (choice->units) {
     for (m=0; m<wind.rows + 3; m++)
        free(units[m]);
     free(units);
  }
  free(buf);
  fclose(fp);
}





					/* CHECK FOR OUT-OF MAP UNIT, THEN
					   CALL CELL CLIP DRIVER */


void  run_clip(ncols, nrows, u_w, u_l, left, top, units, id, cntwhole)
int  ncols, nrows, u_w, u_l, left, top, **units, id, cntwhole;
{
int i,j;

  G_sleep_on_error(0); 

  if (ncols < left + u_w || nrows < top + u_l) {
     printf("\n");
     printf("   ******************************************************\n");
     printf("    Sampling units do not fit within the current region. \n");
     printf("    Either correct the region or redo the sampling unit  \n");
     printf("    selection using r.le.setup.  This error message came \n");
     printf("    from an analysis of the r.le.para/units file and the \n");
     printf("    current region setting.                              \n");
     printf("   ******************************************************\n");
     exit(1);
  }	

  if (choice->units) {
     for (i = top + 1; i < top + 1 + u_l; i++) {
        for (j = left + 1; j < left + 1 + u_w; j++)
           *(*(units + i) + j) = id+1;
     }
  }
	
  cell_clip_drv(left, top, u_w, u_l, NULL, 0, cntwhole);
}





					/* CLIP THE REGION, THEN
					   RUN R.LE.PIXEL */


void  whole_reg_driver()
{
  register int     i, j;
  int		   *row_buf, regcnt, found, fr,
		   nrows, ncols, *richwhole, cntwhole=0, *att_buf;
  REGLIST   	   *ptrfirst, *ptrthis, *ptrnew;



  nrows = G_window_rows();
  ncols = G_window_cols();

  g_scale = 1;

  if (choice->edg[2] || choice->jux[0]) {

					/* dynamically allocate memory for
					   the richness array */

     richwhole = (int *)G_calloc(MAX, sizeof(int));

					/* initialize the richness array
					   elements with the value -999 */

     for(i = 0; i < MAX; i++)	
        richwhole[i] = -999;

     att_buf = G_allocate_cell_buf();

 					/* go through the search area
					   pixel by pixel */

     for(i = 0; i < nrows; i++) {
	G_zero_cell_buf(att_buf);
	G_get_map_row(finput, att_buf, i);
        for(j = 0; j < ncols; j++) { 
           if (*(att_buf+j)) 

					/* call get_rich_whole to tally up the
					   number of different attributes
					   in the search area and fill
					   the richness array with those
					   attributes */

	      get_rich_whole(*(att_buf+j), richwhole, &cntwhole);
        }
     }
     free(att_buf);
/*     free(richwhole);*/
  }



  if(choice->wrum != 'r')
     cell_clip_drv(0, 0, ncols, nrows, NULL, 0, cntwhole);

  else {
     regcnt = 0;
     fr = G_open_cell_old(choice->reg, G_mapset());
     row_buf = G_allocate_cell_buf();
     for (i = 0; i < nrows; i++) {
        G_zero_cell_buf (row_buf);
        G_get_map_row(fr, row_buf, i);
        for (j = 0; j < ncols; j++) {
	   if(*(row_buf + j)) {
	      if (regcnt == 0)
	         ptrfirst = (REGLIST *) NULL;
	      ptrthis = ptrfirst;
	      found = 0;
	      while (ptrthis) {
	         if (*(row_buf + j) == ptrthis->att) {
		    if (j < ptrthis->w) ptrthis->w = j;
		    if (j > ptrthis->e) ptrthis->e = j;
		    if (i < ptrthis->n) ptrthis->n = i;
		    if (i > ptrthis->s) ptrthis->s = i;
		    found = 1;
	         }
	         ptrthis = ptrthis->next;
	      }
	      if (!found) {
	         ptrnew = (REGLIST *)G_calloc(1,sizeof(REGLIST));
	         if(ptrfirst == (REGLIST *) NULL)
		    ptrfirst = ptrthis = ptrnew;
	         else {
		    ptrthis = ptrfirst;
		    while (ptrthis->next != (REGLIST *) NULL)
		       ptrthis = ptrthis->next;
		    ptrthis->next = ptrnew;
		    ptrthis = ptrnew;
	         }
	         ptrthis->att = *(row_buf + j);
	         ptrthis->n = i;
	         ptrthis->s = i;
	         ptrthis->e = j;
	         ptrthis->w = j;
	         regcnt++;
	      }
           }
	}
     }
     g_unit = 0;
     ptrthis = ptrfirst;
     while (ptrthis) {
	g_unit = ptrthis->att;
	cell_clip_drv (ptrthis->w, ptrthis->n, ptrthis->e - ptrthis->w + 1, 
		       ptrthis->s - ptrthis->n + 1, NULL, ptrthis->att,
		       cntwhole);
	ptrthis = ptrthis->next;
     }
     G_close_cell(fr);
     free (row_buf);
     free(ptrnew);
  }
}






					/* RUN R.LE.PIXEL IN BACKGROUND */

void  texture_back()
{
    char  *mapset, *mailfile, *result;
    int   i, n;
    long  t1, t2;
    FILE  *fd;
  
    puts("\nYou`ll get a mail message when r.le.pixel is done.\n"); 

    if (i = G_fork()) {
	printf("   pid = %d\n", i);
	exit(0);
    }

    mailfile = G_tempfile();
    unlink (mailfile);
    close(creat(mailfile,0666));

					/* open stderr to /dev/null so all 
					   GRASS error messages will be
					   mailed to the user */

    freopen ("/dev/null","w",stderr);
    freopen ("/dev/null","w",stdout);

    G_suppress_warnings(1);

    time(&t1);

					 /* call the foreground module */

    texture_fore();
    time(&t2); 

    mail (mailfile, t1, t2);
    unlink (mailfile);
}






					/* SEND MAIL TO USER WHEN BACKGROUND
					   VERSION IS DONE */

void  mail (mailfile, t1, t2)
char *mailfile;
long  t1, t2;
{
    FILE *fd, *mail, *popen();
    char *G_program_name();
    char buf[1024];
    int any;

    if(!(mail = popen ("mail `whoami`","w"))) return;

    fprintf (mail, "Subject: r.le.pixel analysis.\n\n"); 
    fprintf(mail, "   r.le.pixel took %d seconds to run.\n", t2 - t1); 
    fprintf(mail, "   Check dir. \"r.le.out\" for the results.\n"); 
    fprintf(mail, "		if not running moving window.\n");
    any = 0;
    fd = fopen0 (mailfile, "r");
    
     while (fgets(buf, sizeof buf, fd))	{
	    fprintf (mail, "%s", buf);
	    any = 1;
     }
     fclose (fd);
    
    if (!any)
	fprintf (mail, "DONE\n");

    pclose (mail);
}







                                        /* FIND UNCOUNTED ATTRIBUTES, 
                                           COUNT THEM UP, AND ADD THEM TO
                                           THE RICHNESS ARRAY IN UNSORTED
                                           ORDER */

void  get_rich_whole(att, rich, cnt)
int  att, rich[], *cnt;
{
  register int i;
                                        /* if this attribute is already
                                           in the richness array, then
                                           return */

  for(i=0; i<*cnt; i++)
     if(att == rich[i]) break;

                                        /* if this attribute is not already
                                           in the richness array, then make
                                           it the "cnt" element of the 
                                           array, then increment the cnt */

  if(i >= *cnt) {
     rich[*cnt] = att;
     ++(*cnt);
  }

}


