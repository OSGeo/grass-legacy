				/********************************/
				/*	r.le.patch/driver.c	*/
				/*				*/
				/*		2.1		*/
				/*				*/
				/*	Version 06/15/94	*/
				/*				*/
				/*      Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/
#include "r.le.patch.h"

  int                  ntype, finput, n_scale=1, n_unit=1;
  static int	       para1, para2, para3, para4, para5;
  float                *shape_PA, *shape_CPA, *shape_RCC, 
	               *size_cl, **recl_tb;
  extern struct CHOICE  *choice;
  char cmdbuf[100] ;

					/* RUN R.LE.PATCH IN FOREGROUND */

void  patch_fore()
{ 
 
  fputs("\nR.LE.PATCH IS WORKING....;\n\n", stderr);

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

  if (choice->wrum == 'm') {
     get_para();
     mv_driver();
     if (para1 || para2 || para3 || para4 || para5)
        free_para();
  }




  else {
     open_files();
     get_para();
     if (choice->wrum != 'u') 
        whole_reg_driver();
     else 
        unit_driver();
     if (para1 || para2 || para3 || para4 || para5)
        free_para();
  }

					/* close the raster file and
					   print a completion message */


  G_close_cell(finput);
  fputs("\nR.LE.PATCH IS DONE;  ", stderr);
  if (choice->wrum != 'm')
     fputs("OUTPUT FILES IN SUBDIRECTORY \"r.le.out\"\n", stderr);

}





					/* SETUP THE OUTPUT FILES WHEN
					   SAM=W,U,R */

void  open_files()
{
  FILE   *fp;
  char	 path[30];

  if (choice->att[1] || choice->att[2] || 
      choice->att[3] || choice->att[4]) {
     fp = fopen0("r.le.out/a1-4.out", "w");
     fclose(fp);  
  }

  if (choice->att[5]) {
     fp = fopen0("r.le.out/a5.out", "w");
     fclose(fp);  
  }

  if (choice->att[6]) {
     fp = fopen0("r.le.out/a6.out", "w");
     fclose(fp);  
  }

  if (choice->att[7]) {
     fp = fopen0("r.le.out/a7.out", "w");
     fclose(fp);  
  }

  if (choice->size[1] || choice->size[2]) {
     fp = fopen0("r.le.out/s1-2.out", "w");
     fclose(fp);
  }

  if (choice->size[3]) {
     fp = fopen0("r.le.out/s3.out", "w");
     fclose(fp);
  }

  if (choice->size[4]) {
     fp = fopen0("r.le.out/s4.out", "w");
     fclose(fp);
  }

  if (choice->size[5]) {
     fp = fopen0("r.le.out/s5.out", "w");
     fclose(fp);
  }

  if (choice->size[6]) {
     fp = fopen0("r.le.out/s6.out", "w");
     fclose(fp);
  }

  if (choice->core[1] || choice->core[2] || 
      choice->core[3] || choice->core[4]) {
     fp = fopen0("r.le.out/c1-4.out", "w");
     fclose(fp);
  }

  if (choice->core[5]) {
     fp = fopen0("r.le.out/c5.out", "w");
     fclose(fp);
  }

  if (choice->core[6]) {
     fp = fopen0("r.le.out/c6.out", "w");
     fclose(fp);
  }

  if (choice->core[7]) {
     fp = fopen0("r.le.out/c7.out", "w");
     fclose(fp);
  }

  if (choice->core[8]) {
     fp = fopen0("r.le.out/c8.out", "w");
     fclose(fp);
  }

  if (choice->core[9]) { 
     fp = fopen0("r.le.out/c9c.out", "w");
     fclose(fp);
     fp = fopen0("r.le.out/c9e.out", "w");
     fclose(fp);
  }

  if (choice->core[10]) { 
     fp = fopen0("r.le.out/c10c.out", "w");
     fclose(fp);
     fp = fopen0("r.le.out/c10e.out", "w");
     fclose(fp);
  }

  if (choice->shape[1] || choice->shape[2]) {
     fp = fopen0("r.le.out/h1-2.out", "w");
     fclose(fp);
  }

  if (choice->shape[3]) {
     fp = fopen0("r.le.out/h3.out", "w");
     fclose(fp);
  }

  if (choice->shape[4]) {
     fp = fopen0("r.le.out/h4.out", "w");
     fclose(fp);
  }

  if (choice->shape[5]) {
     fp = fopen0("r.le.out/h5.out", "w");
     fclose(fp);
  }

  if (choice->shape[6]) {                
     fp = fopen0("r.le.out/h6.out", "w");
     fclose(fp);
  }

  if (choice->fract){
     fp = fopen0("r.le.out/f1.out", "w");
     fclose(fp);
  }

  if (choice->perim[1] || choice->perim[2] || 
      choice->perim[3]) {
     fp = fopen0("r.le.out/p1-3.out", "w");
     fclose(fp);
  }

  if (choice->perim[4]) {
     fp = fopen0("r.le.out/p4.out", "w");
     fclose(fp);
  }

  if (choice->perim[5]) {
     fp = fopen0("r.le.out/p5.out", "w");
     fclose(fp);
  }

  if (choice->perim[6]) {
     fp = fopen0("r.le.out/p6.out", "w");
     fclose(fp);
  }

  if (choice->wrum != 'm' && (strcmp(choice->out,""))) {
     sprintf(path, "r.le.out/%s", choice->out);
     fp = fopen0(path, "w");
     fclose(fp);
  }

}




					/* OPEN OUTPUT FILE, WITH ERROR TRAP */

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
  }
  return fp;
}



					/* OPEN INPUT FILE, WITH ERROR TRAP */

FILE  *fopen1(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))) {
     printf("\n");
     printf("   ******************************************************\n");
     printf("    You chose a moving window or sampling units analysis \n");
     printf("       but r.le.patch can't find file \"%s\"             \n",name);
     printf("       which defines the moving window or sampling units \n");
     printf("    First use r.le.setup for to setup a moving window or \n");
     printf("       sampling units to make this file                  \n"); 
     printf("   ******************************************************\n");
     exit(1);
     exit(1);
  }
  return fp;
}


					/* OPEN INPUT FILE, WITH ERROR TRAP */

FILE  *fopen2(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))) {
     printf("\n");
     printf("   **************************************************\n");
     printf("    You chose a 'by gp' or 'by class' analysis  	  \n");
     printf("       but r.le.patch can't find file \"%s\"         \n",name);
     printf("       which defines the attribute groups or classes \n");
     printf("    First use r.le.setup to create this file         \n"); 
     printf("   **************************************************\n");
     exit(1);
     exit(1);
  }
  return fp;
}


					/* MOVING WINDOW DRIVER PROG. */

void  mv_driver()
{
  register int  i, j;
  int      nr, nc, u_w, u_l, x0, y0, fd, row, col, d, fmask, 
	   *row_buf, *tmp_buf, m, p;
  int      a1,a2,a3,a4,a5,a6,a7,s1,s2,s3,s4,s5,s6,c1,c2,c3,c4,c5,
	   c6,c7,c8,c9,c10,h1,h2,h3,h4,h5,h6,p1,p2,p3,p4,p5,p6,f1;
  long     finished_time;
  int	   **buff=NULL;
  struct Cell_head wind;

/* variables: 	nc = #cols. in search area minus width of mov. wind.
		nr = #rows in search area minus height of mov. wind.
		x0 = starting column for upper L corner of mov. wind.
		y0 = starting row for upper L corner of mov. wind.
		u_w = width of mov. wind. in cells
		u_l = width of mov. wind. in cells
*/

					/* open the appropriate output
					   maps */

  if(choice->att[1]) a1 = G_open_cell_new_random("a1");
  if(choice->att[2]) a2 = G_open_cell_new_random("a2");
  if(choice->att[3]) a3 = G_open_cell_new_random("a3");
  if(choice->att[4]) a4 = G_open_cell_new_random("a4");
  if(choice->att[5]) a5 = G_open_cell_new_random("a5");
  if(choice->att[6]) a6 = G_open_cell_new_random("a6");
  if(choice->att[7]) a7 = G_open_cell_new_random("a7");

  if(choice->size[1]) s1 = G_open_cell_new_random("s1");
  if(choice->size[2]) s2 = G_open_cell_new_random("s2");
  if(choice->size[3]) s3 = G_open_cell_new_random("s3");
  if(choice->size[4]) s4 = G_open_cell_new_random("s4");
  if(choice->size[5]) s5 = G_open_cell_new_random("s5");
  if(choice->size[6]) s6 = G_open_cell_new_random("s6");

  if(choice->core[1]) c1 = G_open_cell_new_random("c1");
  if(choice->core[2]) c2 = G_open_cell_new_random("c2");
  if(choice->core[3]) c3 = G_open_cell_new_random("c3");
  if(choice->core[4]) c4 = G_open_cell_new_random("c4");
  if(choice->core[5]) c5 = G_open_cell_new_random("c5");
  if(choice->core[6]) c6 = G_open_cell_new_random("c6");
  if(choice->core[7]) c7 = G_open_cell_new_random("c7");
  if(choice->core[8]) c8 = G_open_cell_new_random("c8");
  if(choice->core[9]) c9 = G_open_cell_new_random("c9");
  if(choice->core[10]) c10 = G_open_cell_new_random("c10");

  if(choice->shape[1]) h1 = G_open_cell_new_random("h1");
  if(choice->shape[2]) h2 = G_open_cell_new_random("h2");
  if(choice->shape[3]) h3 = G_open_cell_new_random("h3");
  if(choice->shape[4]) h4 = G_open_cell_new_random("h4");
  if(choice->shape[5]) h5 = G_open_cell_new_random("h5");
  if(choice->shape[6]) h6 = G_open_cell_new_random("h6");

  if(choice->fract)    f1 = G_open_cell_new_random("f1");

  if(choice->perim[1]) p1 = G_open_cell_new_random("p1");
  if(choice->perim[2]) p2 = G_open_cell_new_random("p2");
  if(choice->perim[3]) p3 = G_open_cell_new_random("p3");
  if(choice->perim[4]) p4 = G_open_cell_new_random("p4");
  if(choice->perim[5]) p5 = G_open_cell_new_random("p5");
  if(choice->perim[6]) p6 = G_open_cell_new_random("p6"); 

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
					   search area and clip it */

  G_get_set_window(&wind);
  if(wind.rows < nr + y0 || wind.cols < nc + x0) {
     printf("\n");
     printf("   *******************************************************\n");
     printf("    Moving window search area in file r.le.para/move_wind \n");
     printf("    does not match the dimensions of the current region.  \n");
     printf("    You must either rerun r.le.setup to make a new        \n");
     printf("    r.le.para/move_wind file or reset the region to match \n");
     printf("    the r.le.para/move_wind file                          \n");
     printf("   *******************************************************\n");
     exit(1);
  }

					/* set the d parameter for the 
					   performance meter */

  if(nr*nc > 10000)
      d = nr*nc/1000;
  else if(nr*nc > 2500)
      d = nr*nc/100;
  else 
      d=10;

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

  for(p=0; p<nc+1; p++)
     buff[p] = (int *)G_calloc(36, sizeof(int)); 

					/* allocate memory for a row buffer if
					   there is a mask */

  if (fmask > 0)                
     row_buf = G_allocate_cell_buf();

 					/* main loop for clipping & measuring 
					   using the moving-window */

  for(i = 0; i < nr; i++) {

					/* zero the buffer before filling it
					   again */

     for (m=0; m<nc+1; m++) {
	for (p=0; p<36; p++)
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
	      cell_clip_drv(x0+j, y0+i, u_w, u_l, buff, j);
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

           cell_clip_drv(x0+j, y0+i, u_w, u_l, buff, j);
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
        G_put_map_row_random(a1, tmp_buf, row, col, nc+1);
     }
     if(choice->att[2]) {
        G_zero_cell_buf(tmp_buf);
        for (m=0; m<nc; m++)
           *(tmp_buf + m) = *(*(buff + m) + 1);
        G_put_map_row_random(a2, tmp_buf, row, col, nc+1);
     }
     if(choice->att[3]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 2);
        G_put_map_row_random(a3, tmp_buf, row, col, nc+1);
     }
     if(choice->att[4]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 3);
        G_put_map_row_random(a4, tmp_buf, row, col, nc+1);
     }
     if(choice->att[5]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 4);
        G_put_map_row_random(a5, tmp_buf, row, col, nc+1);
     }
     if(choice->att[6]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 5);
        G_put_map_row_random(a6, tmp_buf, row, col, nc+1);
     }
     if(choice->att[7]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 6);
        G_put_map_row_random(a7, tmp_buf, row, col, nc+1);
     }
     if(choice->size[1]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 7);
        G_put_map_row_random(s1, tmp_buf, row, col, nc+1);
     }
     if(choice->size[2]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 8);
        G_put_map_row_random(s2, tmp_buf, row, col, nc+1);
     }
     if(choice->size[3]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 9);
        G_put_map_row_random(s3, tmp_buf, row, col, nc+1);
     }
     if(choice->size[4]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 10);
        G_put_map_row_random(s4, tmp_buf, row, col, nc+1);
     }
     if(choice->size[5]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 11);
        G_put_map_row_random(s5, tmp_buf, row, col, nc+1);
     }
     if(choice->size[6]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 12);
        G_put_map_row_random(s6, tmp_buf, row, col, nc+1);
     }
     if(choice->core[1]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 13);
        G_put_map_row_random(c1, tmp_buf, row, col, nc+1);
     }
     if(choice->core[2]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 14);
        G_put_map_row_random(c2, tmp_buf, row, col, nc+1);
     }
     if(choice->core[3]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 15);
        G_put_map_row_random(c3, tmp_buf, row, col, nc+1);
     }
     if(choice->core[4]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 16);
        G_put_map_row_random(c4, tmp_buf, row, col, nc+1);
     }
     if(choice->core[5]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 17);
        G_put_map_row_random(c5, tmp_buf, row, col, nc+1);
     }
     if(choice->core[6]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 18);
        G_put_map_row_random(c6, tmp_buf, row, col, nc+1);
     }
     if(choice->core[7]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 19);
        G_put_map_row_random(c7, tmp_buf, row, col, nc+1);
     }
     if(choice->core[8]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 20);
        G_put_map_row_random(c8, tmp_buf, row, col, nc+1);
     }
     if(choice->core[9]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 21);
        G_put_map_row_random(c9, tmp_buf, row, col, nc+1);
     }
     if(choice->core[10]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 22);
        G_put_map_row_random(c10, tmp_buf, row, col, nc+1);
     }
     if(choice->shape[1]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 23);
        G_put_map_row_random(h1, tmp_buf, row, col, nc+1);
     }
     if(choice->shape[2]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 24);
        G_put_map_row_random(h2, tmp_buf, row, col, nc+1);
     }
     if(choice->shape[3]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 25);
        G_put_map_row_random(h3, tmp_buf, row, col, nc+1);
     }
     if(choice->shape[4]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 26);
        G_put_map_row_random(h4, tmp_buf, row, col, nc+1);
     }
     if(choice->shape[5]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 27);
        G_put_map_row_random(h5, tmp_buf, row, col, nc+1);
     }
     if(choice->shape[6]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 28);
        G_put_map_row_random(h6, tmp_buf, row, col, nc+1);
     }
     if(choice->fract) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 29);
        G_put_map_row_random(f1, tmp_buf, row, col, nc+1);
     }
     if(choice->perim[1]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 30);
        G_put_map_row_random(p1, tmp_buf, row, col, nc+1);
     }
     if(choice->perim[2]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 31);
        G_put_map_row_random(p2, tmp_buf, row, col, nc+1);
     }
     if(choice->perim[3]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 32);
        G_put_map_row_random(p3, tmp_buf, row, col, nc+1);
     }
     if(choice->perim[4]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 33);
        G_put_map_row_random(p4, tmp_buf, row, col, nc+1);
     }
     if(choice->perim[5]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 34);
        G_put_map_row_random(p5, tmp_buf, row, col, nc+1);
     }
     if(choice->perim[6]) {
        G_zero_cell_buf(tmp_buf);
	for (m=0; m<nc; m++)
	   *(tmp_buf + m) = *(*(buff+ m) + 35);
        G_put_map_row_random(p6, tmp_buf, row, col, nc+1);
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
     G_close_cell(a1);
     set_colors("a1");
     sprintf(cmdbuf,"%s %s","r.compress","a1") ;
     system(cmdbuf);
  }
  if(choice->att[2]) {
     G_close_cell(a2);
     set_colors("a2");
     sprintf(cmdbuf,"%s %s","r.compress","a2") ;
     system(cmdbuf);
  }
  if(choice->att[3]) {
     G_close_cell(a3);
     set_colors("a3");
     sprintf(cmdbuf,"%s %s","r.compress","a3") ;
     system(cmdbuf);
  }
  if(choice->att[4]) {
     G_close_cell(a4);
     set_colors("a4");
     sprintf(cmdbuf,"%s %s","r.compress","a4") ;
     system(cmdbuf);
  }
  if(choice->att[5]) {
     G_close_cell(a5);
     set_colors("a5");
     sprintf(cmdbuf,"%s %s","r.compress","a5") ;
     system(cmdbuf);
  }
  if(choice->att[6]) {
     G_close_cell(a6);
     set_colors("a6");
     sprintf(cmdbuf,"%s %s","r.compress","a6") ;
     system(cmdbuf);
  }
  if(choice->att[7]) {
     G_close_cell(a7);
     set_colors("a7");
     sprintf(cmdbuf,"%s %s","r.compress","a7") ;
     system(cmdbuf);
  }
  if(choice->size[1]) {
     G_close_cell(s1);
     set_colors("s1");
     sprintf(cmdbuf,"%s %s","r.compress","s1") ;
     system(cmdbuf);
  }
  if(choice->size[2]) {
     G_close_cell(s2);
     set_colors("s2");
     sprintf(cmdbuf,"%s %s","r.compress","s2") ;
     system(cmdbuf);
  }
  if(choice->size[3]) {
     G_close_cell(s3);
     set_colors("s3");
     sprintf(cmdbuf,"%s %s","r.compress","s3") ;
     system(cmdbuf);
  }
  if(choice->size[4]) {
     G_close_cell(s4);
     set_colors("s4");
     sprintf(cmdbuf,"%s %s","r.compress","s4") ;
     system(cmdbuf);
  }
  if(choice->size[5]) {
     G_close_cell(s5);
     set_colors("s5");
     sprintf(cmdbuf,"%s %s","r.compress","s5") ;
     system(cmdbuf);
  }
  if(choice->size[6]) {
     G_close_cell(s6);
     set_colors("s6");
     sprintf(cmdbuf,"%s %s","r.compress","s6") ;
     system(cmdbuf);
  }
  if(choice->core[1]) {
     G_close_cell(c1);
     set_colors("c1");
     sprintf(cmdbuf,"%s %s","r.compress","c1") ;
     system(cmdbuf);
  }
  if(choice->core[2]) {
     G_close_cell(c2);
     set_colors("c2");
     sprintf(cmdbuf,"%s %s","r.compress","c2") ;
     system(cmdbuf);
  }
  if(choice->core[3]) {
     G_close_cell(c3);
     set_colors("c3");
     sprintf(cmdbuf,"%s %s","r.compress","c3") ;
     system(cmdbuf);
  }
  if(choice->core[4]) {
     G_close_cell(c4);
     set_colors("c4");
     sprintf(cmdbuf,"%s %s","r.compress","c4") ;
     system(cmdbuf);
  }
  if(choice->core[5]) {
     G_close_cell(c5);
     set_colors("c5");
     sprintf(cmdbuf,"%s %s","r.compress","c5") ;
     system(cmdbuf);
  }
  if(choice->core[6]) {
     G_close_cell(c6);
     set_colors("c6");
     sprintf(cmdbuf,"%s %s","r.compress","c6") ;
     system(cmdbuf);
  }
  if(choice->core[7]) {
     G_close_cell(c7);
     set_colors("c7");
     sprintf(cmdbuf,"%s %s","r.compress","c7") ;
     system(cmdbuf);
  }
  if(choice->core[8]) {
     G_close_cell(c8);
     set_colors("c8");
     sprintf(cmdbuf,"%s %s","r.compress","c8") ;
     system(cmdbuf);
  }
  if(choice->core[9]) {
     G_close_cell(c9);
     set_colors("c9");
     sprintf(cmdbuf,"%s %s","r.compress","c9") ;
     system(cmdbuf);
  }
  if(choice->core[10]) {
     G_close_cell(c10);
     set_colors("c10");
     sprintf(cmdbuf,"%s %s","r.compress","c10") ;
     system(cmdbuf);
  }
  if(choice->shape[1]) {
     G_close_cell(h1);
     set_colors("h1");
     sprintf(cmdbuf,"%s %s","r.compress","h1") ;
     system(cmdbuf);
  }
  if(choice->shape[2]) {
     G_close_cell(h2);
     set_colors("h2");
     sprintf(cmdbuf,"%s %s","r.compress","h2") ;
     system(cmdbuf);
  }
  if(choice->shape[3]) {
     G_close_cell(h3);
     set_colors("h3");
     sprintf(cmdbuf,"%s %s","r.compress","h3") ;
     system(cmdbuf);
  }
  if(choice->shape[4]) {
     G_close_cell(h4);
     set_colors("h4");
     sprintf(cmdbuf,"%s %s","r.compress","h4") ;
     system(cmdbuf);
  }
  if(choice->shape[5]) {
     G_close_cell(h5);
     set_colors("h5");
     sprintf(cmdbuf,"%s %s","r.compress","h5") ;
     system(cmdbuf);
  }
  if(choice->shape[6]) {
     G_close_cell(h6);
     set_colors("h6");
     sprintf(cmdbuf,"%s %s","r.compress","h6") ;
     system(cmdbuf);
  }
  if(choice->fract) {
     G_close_cell(f1);
     set_colors("f1");
     sprintf(cmdbuf,"%s %s","r.compress","f1") ;
     system(cmdbuf);
  }
  if(choice->perim[1]) {
     G_close_cell(p1);
     set_colors("p1");
     sprintf(cmdbuf,"%s %s","r.compress","p1") ;
     system(cmdbuf);
  }
  if(choice->perim[2]) {
     G_close_cell(p2);
     set_colors("p2");
     sprintf(cmdbuf,"%s %s","r.compress","p2") ;
     system(cmdbuf);
  }
  if(choice->perim[3]) {
     G_close_cell(p3);
     set_colors("p3");
     sprintf(cmdbuf,"%s %s","r.compress","p3") ;
     system(cmdbuf);
  }
  if(choice->perim[4]) {
     G_close_cell(p4);
     set_colors("p4");
     sprintf(cmdbuf,"%s %s","r.compress","p4") ;
     system(cmdbuf);
  }
  if(choice->perim[5]) {
     G_close_cell(p5);
     set_colors("p5");
     sprintf(cmdbuf,"%s %s","r.compress","p5") ;
     system(cmdbuf);
  }
  if(choice->perim[6]) {
     G_close_cell(p6);
     set_colors("p6");
     sprintf(cmdbuf,"%s %s","r.compress","p6") ;
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
  *nc = ww - *uw + 1; 
  *nr = wl - *ul + 1;

  free(buf);
  fclose(fp);
}


					/* PERFORMANCE METER - DISPLAYS
					   THE PROGRESS OF THE MOVING WINDOW
					   AS A COUNT AND ESTIMATED COMPLETION
                                           TIME WHILE THE PROGRAM RUNS */

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

  if( i < 10 )
     d = 1 ;
  else
     d = div ;

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
     fprintf (stdout,"WINDOWS LEFT  = %8d     EST. COMPLETION = %s\r",(n-i),done); 
     fflush(stdout);
     k ++; 
  } 
}
					/* READ IN THE PARAMETERS FOR
					   GROUPS & CLASSES */


void  get_para()
{
  register int i, j, k;
  float        *tmp;

					/* set the parameter flags to 0 */
  
  para1 = para2 = para3 = para4 = para5 = 0;

					/* read the reclass table for
					   attribute gps */

  if (choice->att[5] || choice->att[6] || choice->size[3] || choice->size[4]
     || choice->size[6] || choice->core[5] || choice->core[6]
     || choice->core[7] || choice->core[8] || choice->core[10] 
     || choice->shape[3] || choice->shape[4] || choice->shape[6] 
     || choice->perim[4] || choice->perim[5] || choice->perim[6]) {

     para1 = 1;
     recl_tb = (float **)G_calloc(25, sizeof(float *));
     tmp = (float *)G_calloc(50, sizeof(float));
     if (choice->wrum == 'm') k = 1;
     else k = 25;
     for(i=0; i<k; i++){  	 
       read_para("recl_tb", i+1, tmp);
       if(*tmp < 2) break;
       recl_tb[i] = (float *)G_malloc(50*sizeof(float));
       for(j=0; j<*tmp; j++)
   	  recl_tb[i][j] = tmp[j];
     }
     if (choice->wrum == 'm') ntype = 1;
     else ntype = i;
     free(tmp);
     if(!ntype) {
        printf("\n");
        printf("   ********************************************************\n");
        printf("    The attribute group file (r.le.para/recl_tb) seems to  \n");
        printf("    be incorrect as no attribute groups were found.  Check \n");
        printf("    this file or make it again using r.le.setup.           \n");
        printf("   ********************************************************\n");
        exit(1);
     }
  }

					/* read the size classes */

  if(choice->size[5] || choice->size[6] || choice->core[9] ||
     choice->core[10]) {
     para2 = 1;
     size_cl = (float *)G_calloc(20, sizeof(float));
     read_line("size", 1, -999, NULL, size_cl);
  }

					/* read shape index classes */ 

  if(choice->Mx[1] && (choice->shape[5] || choice->shape[6])){
     para3 = 1;
     shape_PA = (float *)G_calloc(20, sizeof(float));
     read_line("shape_PA", 1, -999, NULL, shape_PA);
  } 
  else if(choice->Mx[2] && (choice->shape[5] || choice->shape[6])){
     para4 = 1;
     shape_CPA = (float *)G_calloc(20, sizeof(float));
     read_line("shape_CPA", 1, -999, NULL, shape_CPA);
  } 
  else if(choice->Mx[3] && (choice->shape[5] || choice->shape[6])){
     para5 = 1;
     shape_RCC = (float *)G_calloc(20, sizeof(float));
     read_line("shape_RCC", 1, -999, NULL, shape_RCC);
  }
}







					/* RELEASE GLOBAL MEMORY */

void  free_para()
{
  register int i;

  if (para1) {
     for(i=0; i<ntype; i++)
        free(recl_tb[i]);
     free(recl_tb);
  }
  if (para2)
     free(size_cl);
  if (para3)
     free(shape_PA);
  if (para4)
     free(shape_CPA);
  if (para5)
     free(shape_RCC);
}





					/* READ IN ONE ATTRIBUTE GP LINE */

void  read_para(name, line, value)
char   *name;
int    line;
float  *value;
{
  FILE  *fp;
  int   i=0, cnt=1, j;
  float tmp;
  char  *buf, path[30];

  sprintf(path, "r.le.para/%s", name);
  fp = fopen2(path, "r"); 	

  buf = G_malloc(256);

  while(fgets(buf, 256, fp) && i < line-1) i++;
  fclose(fp);

  for(i=0; ; i++) {
     if(*(buf+i) == 'e' || *(buf+i) == '=')	
	break;
     else if( *(buf+i) == 't'){
	*(value + cnt) = -999;
	cnt ++;
	i+= 4;
     } 
     else if(isdigit( *(buf+i) )){
	sscanf(buf+i, "%f", value+cnt);
	while(isdigit( *(buf+i) )) i++;
	cnt ++;
     }
  }
  *value = cnt;
  free(buf);
}





					/* READ IN ONE CLASS LINE */

void  read_line(name, line, n, value, fvalue)
char   *name;
int    line, n, *value;
float  *fvalue;
{
  FILE  *fp;
  int   i;
  char  path[30], *buf;

  sprintf(path, "r.le.para/%s", name);

  fp = fopen2(path, "r");
  buf = G_malloc(256);
 
  for(i=0; i<line-1; i++)
     	fgets(buf, 256, fp);
  free(buf);

  if(n > 0)
     for(i=0; i<n; i++)
   	fscanf(fp, "%d", value+i);
  else {
     for(i=1; ; i++){
   	fscanf(fp, "%f", fvalue+i);
	if(fvalue[i] <= -999) break;
     }
     if(3 > (fvalue[0] = i)){	
        buf = G_malloc(40);
 	sprintf(buf, "\n No data in file\"%s\"; use r.le.setup\n", path);
  	G_fatal_error(buf);
	free(buf);
     }
  }     
  fclose(fp);
}






					/* READ IN SAMPLING UNIT PARAMETERS
					   AND RUN R.LE.PATCH */

void  unit_driver()
{
  int              top, left, u_w, u_l, nscl, nu, fd;
  char 		   *buf, unitname[10], istr[3];
  register int     i, j, k, m;
  static int       cnt = 0;
  struct Cell_head wind;
  FILE             *fp;
  CELL		   **units, *unit_buf;


  G_get_set_window(&wind);
  fp = fopen1("r.le.para/units", "r");

  buf = G_malloc(513);
 
  					/* get the number of scales */

  fgets(buf, 512, fp);
  sscanf(buf, "%d", &nscl);

                      			/* dynamically allocate storage for the
                                   	   buffer that will hold the map of the
                                   	   sampling units */

  if (choice->units) {
     units = (CELL **)G_calloc(wind.rows + 3, sizeof(CELL *));
     for(i=0; i<wind.rows+3; i++)
        units[i] = (CELL *)G_calloc(wind.cols + 3, sizeof(CELL));
  }

  					/* for each scale */

  for(i=0; i<nscl; i++){  
     n_scale = i+1;
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
        n_unit = j+1;
        fgets(buf, 512, fp);
        sscanf(buf, "%d%d", &left, &top);

		   			/* call cell_clip driver */

        run_clip(wind.cols, wind.rows, u_w, u_l, left, top, units, j);
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

void  run_clip(ncols, nrows, u_w, u_l, left, top, units, id)
int  ncols, nrows, u_w, u_l, left, top, **units, id;
{
int i,j;

  G_sleep_on_error(0); 

					/* check unit */

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
	
  cell_clip_drv(left, top, u_w, u_l, NULL, 0); 
}






					/* CLIP THE REGION, THEN
					   RUN R.LE.PATCH */

void  whole_reg_driver()
{
  register int     i, j;
  int		   *row_buf, regcnt, found, fr,
		   nrows, ncols;
  REGLIST   	   *ptrfirst, *ptrthis, *ptrnew;


  nrows = G_window_rows();
  ncols = G_window_cols();

  n_scale = 1;

  if (choice->wrum != 'r')
     cell_clip_drv(0, 0, ncols, nrows, NULL, 0);

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
     n_unit = 0;
     ptrthis = ptrfirst;
     while (ptrthis) {
	n_unit = ptrthis->att;
	cell_clip_drv (ptrthis->w, ptrthis->n, ptrthis->e - ptrthis->w + 1, 
		       ptrthis->s - ptrthis->n + 1, NULL, ptrthis->att);
	ptrthis = ptrthis->next;
     }
     G_close_cell(fr);
     free (row_buf);
     free(ptrnew);
  }
}






					/* RUN R.LE.PATCH IN BACKGROUND */

void  patch_back()
{
    char  *mapset, *mailfile, *result;
    int   i, n;
    long  t1, t2;
    FILE  *fd;

    puts("\nYou'll get a mail message when \"r.le.patch\" is done.\n");
    if (i= G_fork()) {
	printf("    pid = %d\n", i);
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

    fd = fopen0 (mailfile,"a");

    time(&t1);

					 /* call the foreground module */

    patch_fore();    
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

    fprintf (mail, "Subject: r.le.patch analysis.\n\n"); 
    fprintf(mail, "   r.le.patch took %d seconds to run.\n", t2 - t1);

    any = 0;
    if(fd = fopen0 (mailfile, "r"))
    {
	while (fgets(buf, sizeof buf, fd))
	{
	    fprintf (mail, "%s", buf);
	    any = 1;
	}
	fclose (fd);
    }
    if (!any){
	fprintf (mail, "DONE\n\n");
        fprintf(mail, "   Check dir. \"r.le.out\" for the results,\n"); 
        fprintf(mail, "     if not moving window.\n"); 
    }
    pclose (mail);
}
