/**************************  cn.c  ******************************
*
*   This program uses   landcover map and a map of hydrologic soils
*   groups to produce a map of runoff.  A design storm or runoff
*   map is required.
*
******************************************************************/



# include "segment.h";

# include "gis.h";

# include "wrat.h";


SEGMENT Hsoil_seg,
	landcover_seg, 
	runoff_seg, 
        rain_seg;

cn()

{

int  lc,  hs, runoff, raini, cni;

float  run, s, cn, cn1, cn3;
	
CELL *cell;



	Hsoil_file = G_tempfile();
	landcover_file = G_tempfile();
        runoff_file = G_tempfile();
	rain_file = G_tempfile();

	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
	cell = G_allocate_cell_buf();

/********** find mapsets for input maps ***************/
 


  Hsoil_mapset = G_find_cell(Hsoil_map_name,"");
  landcover_mapset = G_find_cell(landcover_map_name,"");
  if(rmap == 0)
      rain_mapset = G_find_cell(rain_map_name,"");

   

/*    open needed map layers    */

      Hsoil_map_fd = G_open_cell_old(Hsoil_map_name, Hsoil_mapset);
	   if (Hsoil_map_fd == 0)
	      printf("unable to open soils map"), exit(0);

      landcover_map_fd = G_open_cell_old(landcover_map_name, landcover_mapset);
           if (landcover_map_fd == 0)
              printf("unable to open landcover map."), exit(0);

       runoff_map_fd = G_open_cell_new(runoff_map_name, runoff_mapset);
           if (runoff_map_fd == 0)
              printf("unable to open runoff map."), exit(0);

    if(rmap == 0)
      {
      rain_map_fd = G_open_cell_old(rain_map_name, rain_mapset);
	   if (rain_map_fd == 0)
	      printf("unable to open rainfall map"), exit(0);
      }
 

/*     set parameters for segments   */
	len = sizeof(CELL);
	srows = rows/5 + 2;
	scols = cols/5 + 2;

/* create segment files for elevation & drainage direction */

	Hsoil_seg_fd = creat(Hsoil_file, 0666);
	segment_format(Hsoil_seg_fd, rows, cols, srows, scols, len);
	close(Hsoil_seg_fd);

	landcover_seg_fd = creat(landcover_file, 0666);
	segment_format(landcover_seg_fd, rows, cols, srows, scols, len);
	close(landcover_seg_fd);

	runoff_seg_fd = creat(runoff_file, 0666);
	segment_format(runoff_seg_fd, rows, cols, srows, scols, len);
	close(runoff_seg_fd);

    if (rmap == 0)
       {
	rain_seg_fd = creat(rain_file, 0666);
    ret=segment_format(rain_seg_fd, rows, cols, srows, scols, len);
	close(rain_seg_fd);
printf("creating rain  %d  %d  seg\n", ret, rain_seg_fd);
       }


printf("starting to initi segments\n");
/*  open iniitialize & segment files  */

      Hsoil_seg_fd = open (Hsoil_file,2);
	ret = segment_init(&Hsoil_seg, Hsoil_seg_fd,3);
	   if (ret == -1)
	      printf("could not initialize Hsoil_seg "), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      landcover_seg_fd = open (landcover_file,2);
	ret = segment_init(&landcover_seg, landcover_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize landcover_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      runoff_seg_fd = open (runoff_file,2);
	ret = segment_init(&runoff_seg, runoff_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize runoff_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

    if (rmap == 0)
      {
      rain_seg_fd = open (rain_file,2);
	ret = segment_init(&rain_seg, rain_seg_fd,3);
	   if (ret == -1)
	      printf("could not initialize rainfall_seg "), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
printf("initilizing rain to segments  %d  %d  \n", ret, rain_seg_fd);
      }


/***  read soils file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (Hsoil_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&Hsoil_seg, cell, r);
	    }
	    segment_flush(&Hsoil_seg);

/***  read landcover file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (landcover_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&landcover_seg, cell, r);
	    }
	    segment_flush(&landcover_seg);


/***  if rmap == 0 read rainfall file into segment  ************/
 
      if (rmap == 0 )
         {
printf("reading rain to segments\n");
	 for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (rain_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&rain_seg, cell, r);
	    }
	    segment_flush(&rain_seg);
        }


/********** call the function that does the work  ***********/

  for(r=0;r<rows;r++)
     {
     for(c=0;c<cols;c++)
        {
        cni = cnrunoff(r,c);      /* get curve number amc = 2 */
        cn = cni;

        if(amc < 2)
           {
           cn1 = 2.2439 + 0.16498*cn + .007692*cn*cn;
           cn = cn - (2-amc) * (cn-cn1);               /* interpolate between */
           }                                           /* amc 1 & 2           */

        if(amc > 2)
           { 
           cn3 = 6.9307 + 1.5960*cn - .0067373*cn*cn;
           cn = cn + (amc - 2) * (cn3 - cn);           /* interpolate between */
           }                                           /* amc 2 & 3           */

        if(cn >0)
          {
          if (rmap == 0)                     /* get rainfall     */
             {
             ret = segment_get(&rain_seg, &raini, r, c);
             rain = raini;
             rain = rain/100;
             }

         s = 1000/cn-10;

         if(.2*s < rain)
            {
            run = ((rain - 0.2 * s )*(rain - 0.2 * s)) / (rain + 0.8 * s);
            runoff = run*100;                 /* convert to 100th of in */
            }

          if(.2*s>=rain)         /* initial extraction >= rainfall */
            runoff = 0;
if(runoff < 0)
    {
    segment_get(&landcover_seg, &lc, r,c);
    segment_get(&Hsoil_seg, &hs, r,c);

    printf("WOW  !! %d  %f %d ar %d  %d      %d  %d  \n",runoff, cn,cni, r, c, lc, hs);
    sleep (3);
    runoff = 0;
   }
         segment_put(&runoff_seg, &runoff, r, c);

         } /********* enf if cn >0  ****************/
      }
    fprintf(stderr," processing row %d  out of %d \r", r, rows);
    fflush(stderr);
    }
  fprintf(stderr,"\n");


/*************  write to cell file  ********************/

	segment_flush(&runoff_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&runoff_seg, cell, r);
	   if(G_put_map_row(runoff_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  release segments, close all files  ********************/

	segment_release(&Hsoil_seg);
	segment_release(&landcover_seg);
	segment_release(&runoff_seg);
        if (rmap == 0)
	   segment_release(&rain_seg);

	close (Hsoil_seg_fd);
	close (landcover_seg_fd);
	close (runoff_seg_fd);
        if (rmap == 0)
	   close (rain_seg_fd);

	G_close_cell(Hsoil_map_fd);
	G_close_cell(landcover_map_fd);
	G_close_cell(runoff_map_fd);
        if (rmap == 0)
	   G_close_cell(rain_map_fd);
return(1);

}  /************************  end of main *****************************/


cnrunoff(r,c)
int r,c;
{
int  landcover, hydsoil;
static lastlc;

		segment_get(&Hsoil_seg, &hydsoil, r, c);
		segment_get(&landcover_seg, &landcover, r, c);

                if(landcover != 16)          /* save lc in case */
                    lastlc = landcover;      /* next cells are  */
                if(landcover == 16)          /* cloud (16)      */
                    landcover = lastlc;

                if(landcover == 0 || hydsoil == 0 )
                    return (0);


		if(hydsoil == 1)
		   {
		   if(landcover == 1)
			return (66);
		   if(landcover == 2 || landcover == 3)
			return (63);
		   if(landcover == 4)
			return (58);
		   if(landcover == 5 || landcover == 6)
			return (38);
		   if(landcover == 7 || landcover == 9)
			return (38);
		   if(landcover == 8)
			return (30);
		   if(landcover == 10)
			return (25);
		   if(landcover == 14)
			return (72);
		   if(landcover == 15)
			return (77);
		  }


		if(hydsoil == 2)
		   {
		   if(landcover == 1)
			return (76);
		   if(landcover == 2 || landcover == 3)
			return (74);
		   if(landcover == 4)
			return (72);
		   if(landcover == 5 || landcover == 6)
			return (61);
		   if(landcover == 7 || landcover == 9)
			return (61);
		   if(landcover == 8)
			return (58);
		   if(landcover == 10)
			return (55);
		   if(landcover == 14)
			return (79);
		   if(landcover == 15)
			return (86);
                   }

		if(hydsoil == 3)
		   {
		   if(landcover == 1)
			return (78);
		   if(landcover == 2 || landcover == 3)
			return (83);
		   if(landcover == 4)
			return (83);
		   if(landcover == 5 || landcover == 6)
			return (74);
		   if(landcover == 7 || landcover == 9)
			return (74);
		   if(landcover == 8)
			return (71);
		   if(landcover == 10)
			return (70);
		   if(landcover == 14)
			return (85);
		   if(landcover == 15)
			return (91);
                   }

		if(hydsoil == 4)
		   {
		   if(landcover == 1)
			return (87);
		   if(landcover == 2 || landcover == 3)
			return (85);
		   if(landcover == 4)
			return (85);
		   if(landcover == 5 || landcover == 6)
			return (80);
		   if(landcover == 7 || landcover == 9)
			return (80);
		   if(landcover == 8)
			return (78);
		   if(landcover == 10)
			return (77);
		   if(landcover == 14)
			return (88);
		   if(landcover == 15)
			return (94);
                   }

		if (landcover == 11 || landcover == 12 || landcover == 13)
                       return (100);
                if (hydsoil == 5) 
                       return (100);


}     /************** end of cnrun  *******************/
