	                 	/********************************/
			        /* 	r.le.dist/driver.c	*/
                                /*                              */
				/*		2.1		*/
				/*				*/
	                        /*       07/10/94 version       */
        	                /*                              */
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
			        /********************************/



#include "sys/times.h"
#include "sys/types.h"
#include "r.le.dist.h"

  int                  ntype, m1, m2, m3, nrst, adj, finput, 
		       n_scale=1, n_unit=1;
  float                *dist_CC, *dist_CE, *dist_EE, 
		       **from_to, **recl_tb;
  extern struct CHOICE  *choice;
  char cmdbuf[100] ;


					/* RUN R.LE.DIST IN FOREGROUND */

void  dist_fore()
{  
  fputs("\nR.LE.DIST IS WORKING...;\n\n", stderr);

					/* check for input raster map */
  
  if(0 > (finput = G_open_cell_old(choice->fn, G_mapset()))) {
     printf("\n");
     printf("   ********************************************************\n");
     printf("    The raster map you specified with the 'map=' parameter \n");
     printf("    was not found in your mapset.                          \n");
     printf("   ********************************************************\n");
     exit(1);
  }
					/* set global variables m1, m2, and
					   m3, plus adj and nrst that indicate
					   the choices */

  if(choice->mn[0] || choice->mn[1])
     adj=1;  
  else if(choice->mn[2] || choice->mn[3] || choice->mn[4])
     m1 = 1;
  else if(choice->mn[5] || choice->mn[6])
     m2 = 1;
  else 
     m3 = 1;
  if(m1 || m2 || m3) 
     nrst = 1;

					/* if running moving window, 
					   print a message, get the 
					   parameters, and start the
					   moving window driver */

  if (choice->wrum == 'm'){
     get_para();
     mv_driver();
     free_para();
  } 

					/* if using whole map, units, 
					   or regions to sample, clean
					   out the old files, get the
					   parameters, and start the 
					   appropriate driver */

  else {	
     open_files();
     get_para();
     if (choice->wrum != 'u') {
        whole_reg_driver(); }
     else 
        unit_driver();
     free_para();
  }

					/* close the raster file and
					   print a completion message */

  G_close_cell(finput);
  fputs("\nR.LE.DIST IS DONE;  ", stderr);
  if (choice->wrum != 'm')
     fputs("OUTPUT FILES IN SUBDIRECTORY \"r.le.out\"\n", stderr);
}




					/* SETUP THE OUTPUT FILES WHEN
					   SAM=W,U,R */

void  open_files()
{
  FILE  *fp;
  char	path[30];

  if(choice->mm[0] || choice->mm[1]){		
     fp = fopen0("r.le.out/n1-2.out", "w");
     fclose(fp);
  }

  if(choice->mm[2]){
     fp = fopen0("r.le.out/n3.out", "w");
     fclose(fp);
  }

  if(choice->mm[3]){
     fp = fopen0("r.le.out/n4.out", "w");
     fclose(fp);
  }

  if(choice->mm[4]){
     fp = fopen0("r.le.out/n5.out", "w");
     fclose(fp);
  }

  if(choice->mm[5]){
     fp = fopen0("r.le.out/n6.out", "w");
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

  if(!(fp = fopen(name, flag))){
     printf("\n");
     printf("   ******************************************************\n");
     printf("    You chose a moving window or sampling units analysis \n");
     printf("       but r.le.dist can't find file \"%s\"              \n",name);
     printf("       which defines the moving window or sampling units \n");
     printf("    First use r.le.setup for to setup a moving window or \n");
     printf("       sampling units to make this file                  \n"); 
     printf("   ******************************************************\n");
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
     printf("       but r.le.dist can't find file \"%s\"          \n",name);
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
  int      nr, nc, u_w, u_l, x0, y0, fd, fin, row, col, d, fmask,
	   *row_buf, *tmp_buf, m, p;
  int	   n1,n2,n3,n4,n5,n6;
  long     finished_time;
  int	   **buff=NULL;
  struct Cell_head wind;

/* variables: nc = #cols. in search area minus width of mov. wind.
	      nr = #rows in search area minus height of mov. wind.
	      x0 = starting column for upper L corner of mov. wind.
	      y0 = starting row for upper L corner of mov. wind.
	     u_w = width of mov. wind. in cells
	     u_l = width of mov. wind. in cells
*/

					/* open the appropriate output
					   maps */

  if(choice->mm[0]) n1 = G_open_cell_new_random("n1");
  if(choice->mm[1]) n2 = G_open_cell_new_random("n2");
  if(choice->mm[2]) n3 = G_open_cell_new_random("n3");
  if(choice->mm[3]) n4 = G_open_cell_new_random("n4");
  if(choice->mm[4]) n5 = G_open_cell_new_random("n5");
  if(choice->mm[5]) n6 = G_open_cell_new_random("n6");

  					/* get the moving-window parameters */

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

  					/* check for an unacceptable search 
					   area and clip it */

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

					/* begin main moving window loop 
					   section */

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

  for(p = 0; p < nc + 1; p++)
     buff[p] = (int *)G_calloc(6, sizeof(int)); 

					/* allocate memory for a row buffer if
					   there is a mask */

  if (fmask > 0)                
     row_buf = G_allocate_cell_buf();

  					/* main loop for clipping & measuring
					   using the moving-window */

  for(i = 0; i < nr; i++){
  
					/* zero the buffer before filling it
					   again */

     for (m = 0; m < nc + 1; m++) {
	for (p = 0; p < 6; p++)
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

        for(j = 0; j < nc; j++) {

           				/* display #cells left to do */

           if(i==0 && j==0)
	      fprintf (stdout,"TOTAL WINDOWS = %8d\n",nr*nc);
           meter(nr*nc, (i*nc + (j+1)), d);
 
					/* call the cell clip driver */
        
	   if(row_buf[x0+j+u_w/2])
	     cell_clip_drv(x0+j, y0+i, u_w, u_l, buff, j);
        }
     }

    					/* if there is no MASK, then clip 
				 	   and measure at every cell */

     else {
                 
        for(j = 0; j < nc; j++) {

           				/* display #cells left to do */

           if(i==0 && j==0) 
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

     if(choice->mm[0]) {
        G_zero_cell_buf(tmp_buf);
        for (m = 0; m < nc; m++)
	   *(tmp_buf + m) = *(*(buff + m) + 0);
        G_put_map_row_random(n1, tmp_buf, row, col, nc+1);
     }
     if(choice->mm[1]) {
        G_zero_cell_buf(tmp_buf);
        for (m = 0; m < nc; m++)
  	   *(tmp_buf + m) = *(*(buff + m) + 1);
        G_put_map_row_random(n2, tmp_buf, row, col, nc+1);
     }
     if(choice->mm[2]) {
        G_zero_cell_buf(tmp_buf);
        for (m = 0; m < nc; m++)
	   *(tmp_buf + m) = *(*(buff + m) + 2);
        G_put_map_row_random(n3, tmp_buf, row, col, nc+1);
     }
     if(choice->mm[3]) {
        G_zero_cell_buf(tmp_buf);
        for (m = 0; m < nc; m++)
	   *(tmp_buf + m) = *(*(buff + m) + 3);
        G_put_map_row_random(n4, tmp_buf, row, col, nc+1);
     }
     if(choice->mm[4]) {
        G_zero_cell_buf(tmp_buf);
        for (m = 0; m < nc; m++)
	   *(tmp_buf + m) = *(*(buff + m) + 4);
        G_put_map_row_random(n5, tmp_buf, row, col, nc+1);
     }
     if(choice->mm[5]) {
        G_zero_cell_buf(tmp_buf);
        for (m = 0; m < nc; m++)
	   *(tmp_buf + m) = *(*(buff + m) + 5);
        G_put_map_row_random(n6, tmp_buf, row, col, nc+1);
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

  if(choice->mm[0]) {
     G_close_cell(n1);
     set_colors("n1");
     sprintf(cmdbuf,"%s %s","r.compress","n1") ;
     system(cmdbuf);
  }
  if(choice->mm[1]) {
     G_close_cell(n2);
     set_colors("n2");
     sprintf(cmdbuf,"%s %s","r.compress","n2") ;
     system(cmdbuf);
  }
  if(choice->mm[2]) {
     G_close_cell(n3);
     set_colors("n3");
     sprintf(cmdbuf,"%s %s","r.compress","n3") ;
     system(cmdbuf);
  }
  if(choice->mm[3]) {
     G_close_cell(n4);
     set_colors("n4");
     sprintf(cmdbuf,"%s %s","r.compress","n4") ;
     system(cmdbuf);
  }
  if(choice->mm[4]) {
     G_close_cell(n5);
     set_colors("n5");
     sprintf(cmdbuf,"%s %s","r.compress","n5") ;
     system(cmdbuf);
  }
  if(choice->mm[5]) {
     G_close_cell(n6);
     set_colors("n6");
     sprintf(cmdbuf,"%s %s","r.compress","n6") ;
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







					/* READ IN THE PARAMETERS FOR
					   GROUPS, CLASSES, RECLASSING, ETC. */

void  get_para()
{
  register int  i, j;
  float         *tmp;

  if( m1 || m2 ||  m3 || choice->mm[2] || choice->mm[3] || choice->mm[5] ) {

     recl_tb = (float **)G_calloc(25, sizeof(float *));
     tmp = (float *)G_calloc(50, sizeof(float));
  
					/* read the attribute group lower 
					   and upper limits into tmp array */ 

     for(i=0; i<25; i++){  	
        read_para("recl_tb", i+1, tmp);
        if(*tmp < 2) break;

					/* read the reclass table */

        recl_tb[i] = (float *)G_malloc(50*sizeof(float));   	
        for(j=0; j<*tmp; j++) {
	   recl_tb[i][j] = tmp[j];
	}
     }
     ntype = i;
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

					/* read from-to attribute groups */

  if(m3){   
     from_to = (float **)G_calloc(2, sizeof(float *));
     tmp = (float *)G_calloc(50, sizeof(float));
  
     for(i = 0; i < 2; i++){  	 
        read_para("from_to", i+1, tmp);
        if (*tmp < 2) break;

        from_to[i] = (float *)G_malloc(50*sizeof(float));   	
        for(j=0; j<*tmp; j++) {
	    from_to[i][j] = tmp[j];
        }
     }
     free(tmp);
     if (i < 1) {
        printf("\n");
        printf("   *******************************************************\n");
        printf("    There is something wrong with the r.le.para/from_to   \n");
        printf("    file used for distance methods m7-m9.  Check this     \n");
        printf("    file or make it again using r.le.setup.               \n");
        printf("   *******************************************************\n");
        exit(1);
     }      
  }

  					/* read distance classes */

  if( choice->mm[4] || choice->mm[5] ){
     if(choice->mn[0] || choice->mn[2] || choice->mn[5] || choice->mn[7]){	
         dist_CC = (float *)G_calloc(20, sizeof(float));
         read_line("dist_cc", 1, -999, NULL, dist_CC);
     }
     if(choice->mn[1] || choice->mn[3] || choice->mn[6] || choice->mn[8]){	
         dist_CE = (float *)G_calloc(20, sizeof(float));
         read_line("dist_ce", 1, -999, NULL, dist_CE);
     }
     if(choice->mn[4] || choice->mn[9]){	
         dist_EE = (float *)G_calloc(20, sizeof(float));
         read_line("dist_ee", 1, -999, NULL, dist_EE);
     }  
  }
}




					/* RELEASE THE GLOBAL MEMORY */


void  free_para()
{
  register int i;

  for(i=0; i<ntype; i++)
     free(recl_tb[i]);
  free(recl_tb);
  free(dist_CC);
  free(dist_CE);
  free(dist_EE);
  free(from_to);
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
 	sprintf(buf, "\n No data in file\"%s\", exit.\n", path);
  	G_fatal_error(buf);
	free(buf);
     }     
  }     
  fclose(fp);
}




					/* READ IN SAMPLING UNIT PARAMETERS
					   AND RUN R.LE.DIST */

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
     for (m = 0; m < wind.rows + 3; m++)
        free(units[m]);
     free(units);
  }
  free(buf);
  fclose(fp);
}




					/* CHECK FOR OUT-OF-MAP UNIT, THEN
					   CALL CELL CLIP DRIVER */

void  run_clip(ncols, nrows, u_w, u_l, left, top, units, id)
int  ncols, nrows, u_w, u_l, left, top, **units, id;
{
int i, j;

  G_sleep_on_error(0); 

  					/* check unit */

  if(ncols < left + u_w || nrows < top + u_l){
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
					   RUN R.LE.DIST */

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





					/* RUN R.LE.DIST IN BACKGROUND */

void  dist_back()
{
    char  *mapset, *mailfile, *result;
    int   i, n;
    long  t1, t2;
    FILE  *fd;

    puts("\nYou'll get a mail message when \"r.le.dist\" is done.\n");
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

    fd = fopen0 (mailfile,"a");

    time(&t1);

					 /* call the foreground module */

    dist_fore();   
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

    fprintf (mail, "Subject: r.le.dist analysis.\n\n"); 
    fprintf(mail, "   r.le.dist took %d seconds to run.\n\n", t2 - t1);
    any = 0;
    if(fd = fopen (mailfile, "r"))
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
        fprintf(mail, "     if not moving window. \n\n"); 
    }
    pclose (mail);
}
