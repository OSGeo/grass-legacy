/************************** Qp.c ******************************
*
*   This program uses  runoff, elevation, drainage direction 
*   and drainage accumulation maps to produce peak discharge and 
*   mean daily discharge maps. The program adds the contributed 
*   from each cell to produce a mean_runoff map (which is actually
*   the total of the runoffs from higher cells)  The mean_runoff map
*   is divided by the draonage accumulation for a true mean runoff
*   later.  Length finder ( a function) finds the maximum length
*   of a the watershed above each cell.  This information is then
*   used in emperical formulas to calculate the peak discharge.
*   mean discharge is the total runoff divided by the number of
*   seconds per day.
*
******************************************************************/

# include "segment.h";

# include "gis.h";

# include "math.h";

# include "wrat.h";


SEGMENT  dir_seg, da_seg, ideal_seg, qp_seg, runoff_seg, meanrunoff_seg;


int RR, CC;


qp()

{


CELL *cell;



	dir_file = G_tempfile();
	da_file = G_tempfile();
	ideal_file = G_tempfile();
	runoff_file = G_tempfile();
        meanrunoff_file = G_tempfile();
	qp_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
	cell = G_allocate_cell_buf();


strcpy(meanrunoff_map_name, "cute");

ideal_mapset = G_find_cell(ideal_map_name, "");



/*    open needed map layers    */

	dir_map_fd = G_open_cell_old(dir_map_name, dir_mapset);
           if (dir_map_fd == 0)
              printf("unable to open drainage direction map."), exit(0);

	da_map_fd = G_open_cell_old(da_map_name, da_mapset);
           if (da_map_fd == 0)
              printf("unable to open drainage accumulation map."), exit(0);

	ideal_map_fd = G_open_cell_old(ideal_map_name, ideal_mapset);
           if (ideal_map_fd == 0)
              printf("unable to open elevation map."), exit(0);

	runoff_map_fd = G_open_cell_old(runoff_map_name, runoff_mapset);
           if (runoff_map_fd == 0)
              printf("unable to open runoff map."), exit(0);

	qp_map_fd = G_open_cell_new(qp_map_name, qp_mapset);
           if (qp_map_fd == 0)
              printf("unable to open peak Discharge map."), exit(0);

	meanrunoff_map_fd = G_open_cell_new (meanrunoff_map_name);
           if (meanrunoff_map_fd == 0)
              printf("unable to open meanrunoff map."), exit(0); 



/*     set parameters for segments   */

	len = sizeof(CELL);
	srows = rows/5 + 2;
	scols = cols/5 + 2;


/* create segment files for elevation & drainage direction */

	dir_seg_fd = creat(dir_file, 0666);
	segment_format(dir_seg_fd, rows, cols, srows, scols, len);
	close(dir_seg_fd);

	da_seg_fd = creat(da_file, 0666);
	segment_format(da_seg_fd, rows, cols, srows, scols, len);
	close(da_seg_fd);

	ideal_seg_fd = creat(ideal_file, 0666);
	segment_format(ideal_seg_fd, rows, cols, srows, scols, len);
	close(ideal_seg_fd);

	runoff_seg_fd = creat(runoff_file, 0666);
	segment_format(runoff_seg_fd, rows, cols, srows, scols, len);
	close(runoff_seg_fd);

	meanrunoff_seg_fd = creat(meanrunoff_file, 0666);
	segment_format(meanrunoff_seg_fd, rows, cols, srows, scols, len);
	close(meanrunoff_seg_fd);

	qp_seg_fd = creat(qp_file, 0666);
	segment_format(qp_seg_fd, rows, cols, srows, scols, len);
	close(qp_seg_fd);


/*  open iniitialize & segment files  */

      dir_seg_fd = open (dir_file,2);
	ret = segment_init(&dir_seg, dir_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize dir_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      da_seg_fd = open (da_file,2);
	ret = segment_init(&da_seg, da_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize da_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      ideal_seg_fd = open (ideal_file,2);
	ret = segment_init(&ideal_seg, ideal_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize ideal_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      runoff_seg_fd = open (runoff_file,2);
	ret = segment_init(&runoff_seg, runoff_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize runoff_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      meanrunoff_seg_fd = open (meanrunoff_file,2);
	ret = segment_init(&meanrunoff_seg, meanrunoff_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize meanrunoff_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);


      qp_seg_fd = open (qp_file,2);
	ret = segment_init(&qp_seg, qp_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize qp_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);



/***  read drainage direction file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (dir_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&dir_seg, cell, r);
	    }
	    segment_flush(&dir_seg);


/******* read drainage accumulation into segments  ******/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (da_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&da_seg, cell, r);
	    }
	    segment_flush(&da_seg);



/************** read elevation onto segments  ****************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (ideal_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&ideal_seg, cell, r);
	    }
	    segment_flush(&ideal_seg);


/************* read runoff into segments  *******************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (runoff_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&runoff_seg, cell, r);
	    }
	    segment_flush(&runoff_seg);


/********** call the functions that do the work  ***********/

	ret = qprunoff();

	ret = lengthfinder();


/***********  write Qp & Q to cell files *********************/

	segment_flush(&qp_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&qp_seg, cell, r);
	   if(G_put_map_row(qp_map_fd, cell, r)<0)
	      exit(1);
	   }


        segment_flush(&meanrunoff_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&meanrunoff_seg, cell, r);
	   if(G_put_map_row(meanrunoff_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  release segments, close all files  ********************/

	segment_release(&dir_seg);
	segment_release(&da_seg);
	segment_release(&qp_seg);
	segment_release(&runoff_seg);
	segment_release(&meanrunoff_seg);

	close (dir_seg_fd);
	close (da_seg_fd);
	close (qp_seg_fd);
	close (runoff_seg_fd);
	close (meanrunoff_seg_fd);

	G_close_cell(dir_map_fd);
	G_close_cell(da_map_fd);
	G_close_cell(qp_map_fd);
	G_close_cell(runoff_map_fd);
	G_close_cell(meanrunoff_map_fd);

        strcpy(command, "g.remove rast=cute\n");
        G_system(command);

return (1);

}  /************************  end of main *****************************/


/********************************* runoff  ********************************
*
*   runoff summs the runoff from a designated input map down to that point
*   point.  That summ is used at the end of lengthfind to calculate 
*   Qp and Qm.  The runoff map layer is not saved when the program ends.
*   Runoff finds every cell with a drainage accumulation = 1 anf follows
*   the drainage direction map adding runoff in two stages.  The first 
*   stage is before the path encounters a path already drained, a "stream".
*   during the first stage the runoff values are summed.  Once a stream 
*   is encountered the tributary runoff is added to each cell down the
*   path until it exits the window.
*
**************************************************************************/

qprunoff()
{
	int r, c, da_value, ro, mro, next_mro;

	  for (RR=0; RR<=rows-1; RR++)
	    {
            fprintf(stderr,"processing row %d of %d through step one\r ",RR,rows-1);
	    fflush(stderr);
	    for (CC=0; CC<=cols-1; CC++)
		{
	        segment_get(&da_seg, &da_value, RR, CC);
		if (da_value == 1)
		   {

		   r=RR, c=CC;
	           segment_get(&runoff_seg, &ro, r, c);
		   segment_put(&meanrunoff_seg, &ro, r, c); 
                   next_mro = ro;
                   nextrc(&r, &c);

		   while (!cexit(r,c) && !qpstream(r,c) )
	               {
	                segment_get(&runoff_seg, &ro, r , c);
		        next_mro = ro + next_mro;
		        segment_put(&meanrunoff_seg, &next_mro, r, c); 
                        segment_flush(&meanrunoff_seg);
                        nextrc(&r, &c);
		       }


		  while ( !cexit(r, c) )
		       {
		        segment_get(&meanrunoff_seg, &ro, r, c);
		        mro = ro + next_mro;
	                segment_put(&meanrunoff_seg, &mro, r, c);
                        segment_flush(&meanrunoff_seg);
                        nextrc(&r, &c);
		       }
		 }  /********** end if da[RR][CC] ==1 *****/
	    }  /***** end for C ********/
     }  /*****  end for R *****/
  fprintf(stderr,"\n");
} /************************ end gfind **************************/





/*********************** lengthfinder *******************************
**
**   length finder starts at each RR,CC cell and runs up hill
**  fillowing the path with the greatest drainage accumulation 
**  value until it finds a da = 1 ie the top of the path.  These
**  values are then used to calculate the peak Q and Mean Q using
**  the emperical relationships presented in AgNPS which is taken 
**  from Smith & Williams (1980) and used in CREAMS. 
**
********************************************************************/ 

lengthfinder()
{

int r, c, ele_bottom, ele_top, dir_value, max_da, exit=0,
    da_value, maxr, maxc, top, runoff, iQp;


double ewres, nsres, length, addlength, diaglength, area, da_f,
       slope, aterm, sterm, rterm, lterm, Qp, Qm, runoff_value,
       ele_top_f, ele_bottom_f;

ewres = window.ew_res;
nsres = window.ns_res;

diaglength = hypot(ewres, nsres);       

for (RR=0; RR<rows-1; RR++)
    {
    for(CC=0; CC<cols-1; CC++)
       {
       r = RR, c = CC, length = 0;
       segment_get(&da_seg, &top, r, c);
       segment_get(&dir_seg,&dir_value, RR, CC);
       if(dir_value > 0 && top > 0)
           {
           if (top == 1 )
              dlength(&length, &r, &c, &dir_value);

           while (top > 1 )
                {
                stepup(&r, &c, &addlength);

 	        length = length + addlength;

	        segment_get(&da_seg, &top, r, c);


	        }   /****** end while  top > 1  *********/

            /*************** collect values  **********/
	    segment_get(&meanrunoff_seg, &runoff, RR, CC);
	    segment_get(&da_seg, &da_value, RR, CC);
	    segment_get(&ideal_seg, &ele_bottom, RR, CC);
	    segment_get(&ideal_seg, &ele_top, r, c);   

            /*********** convert to floating pt data types ***/

            ele_top_f = ele_top;
	    ele_bottom_f = ele_bottom;
	    da_f = da_value;
	    runoff_value = runoff;    /* RO was in 100ths if inches */
            runoff_value = runoff_value /100;

            /******************* do initial calculations ********/

	    runoff_value = runoff_value / da_f;    /* area is in cells */	

	    area = da_f * ewres * nsres / 10000 * 2.47;
                                               /* convert area to acres */

	    slope = (ele_top_f - ele_bottom_f) / length;
	    if (slope <= 0)
                slope = .001;                /* no slope <= 0 allowed  */

	    length = length * 39.47 / 12;    /* convert meters to feet */
          
            if ( runoff_value > 0  )
               {
               /**********  solve Qp equation *************/
               aterm = pow(area,0.7);
               sterm = pow(slope, 0.159);
               rterm = pow(runoff_value, (pow(area,0.166)*.824));
               lterm = 1/pow(length*length/(area*43560),.187);

               Qp = 8.484 * aterm * sterm * rterm * lterm;

               /******  convert to interger format for storage in map ********/

               iQp = Qp;

              /******  put the results the appropriate layers  **********/

              segment_put(&qp_seg, &iQp, RR, CC);

              }  /* end if runoff_value > 0 */

            } /*** end if dir_value > 0 **************/

	 }  /********** end of for (CC=0...) ******************/
         fprintf(stderr,"percessing row %d out of %d through step two\r",RR,rows-2);
	 fflush(stderr);

      }    /********** end of for (RR=0...) ******************/
  fprintf(stderr,"\n");
return(1);

} /******************* end of lengthfinder() ***********************/

           

stepup(rr,cc,al)
int *rr, *cc;
double *al;

{
int r, c, da_value, dir_value, max_da, maxr, maxc;
double addlength, diaglength, ewres, nsres;

    ewres = window.ew_res;
    nsres = window.ns_res;
    diaglength = .5 * hypot(ewres, nsres);       
    r = *rr;
    c= *cc;

     if(c<cols-1)
       {
       segment_get(&dir_seg,&dir_value, r, c+1);
       if (dir_value == 5)
          {
          segment_get(&da_seg, &da_value, r, c+1);
	  if(max_da < da_value)
              max_da = da_value, maxr = r, maxc = c+1, addlength = ewres;
          }
      }
                  
      if (c<cols-1 && r>0)
          {
          segment_get(&dir_seg,&dir_value, r-1, c+1);
          if (dir_value == 6)
              {
  	      segment_get(&da_seg, &da_value, r-1, c+1);
	      if(max_da < da_value)
              max_da = da_value, maxr = r-1, maxc = c+1, addlength = diaglength;
             }
         }

     if (r>0)
        {
        segment_get(&dir_seg, &dir_value, r-1, c);
        if (dir_value == 7)
	   {
	   segment_get(&da_seg, &da_value, r-1 , c);
	   if(max_da < da_value)
              max_da = da_value, maxr = r-1 , maxc = c, addlength = nsres;
           }
        }
 
     if (c>0 && r > 0)
        {
        segment_get(&dir_seg, &dir_value, r-1, c-1);
        if (dir_value == 8)
           {
	   segment_get(&da_seg, &da_value, r-1, c-1);
	   if(max_da < da_value)
           max_da = da_value, maxr = r-1, maxc = c-1, addlength = diaglength;
           }
        }
                  
    if (c>0 )
       {
       segment_get(&dir_seg, &dir_value, r, c-1);
       if (dir_value == 1)
	  {
	  segment_get(&da_seg, &da_value, r, c-1);
	  if(max_da < da_value)
             max_da = da_value, maxr = r, maxc = c-1, addlength = ewres;
          }
       }
                  
    if (c>0 && r<rows-1)
       {
       segment_get(&dir_seg, &dir_value, r+1, c-1);
       if (dir_value == 2)
	  {
          segment_get(&da_seg, &da_value, r+1, c-1);
	  if(max_da < da_value)
          max_da = da_value, maxr = r+1, maxc = c-1, addlength = diaglength;
          }
       }
                  
    if (r<rows-1)
       {
       segment_get(&dir_seg, &dir_value, r+1, c);
       if (dir_value == 3)
	  {
	  segment_get(&da_seg, &da_value, r+1, c);
	  if(max_da < da_value)
             max_da = da_value, maxr = r+1, maxc = c, addlength = nsres;
          }
       }
                  
    if (c < cols-1 && r < rows-1)
       {
       segment_get(&dir_seg, &dir_value, r+1, c+1);
       if (dir_value == 4)
          {
	  segment_get(&da_seg, &da_value, r+1, c+1);
	  if(max_da < da_value)
          max_da = da_value, maxr = r+1, maxc = c+1, addlength = diaglength;
          }
      }

     *al = addlength;
     *rr = maxr;
     *cc =maxc;

} /*************** end stepup *******************************/


dlength(l, dir_value)
double *l;
int  dir_value;
    {
    double length, ewres, nsres;

    ewres = window.ew_res;
    nsres = window.ns_res;

    if (dir_value == 1 || dir_value == 5)
        length = .5 * ewres;
    if (dir_value == 3 || dir_value == 7)
        length = .5 * nsres;
    if (dir_value==2 || dir_value==4 || dir_value==6 || dir_value==8)
        length = .5 * hypot(ewres, nsres);       

    *l = length;
    }


/***************************** cstream ****************************/
/**   cstream() checks for the intersection with an established  **/
/**   stream from the draining of a previous path                **/
/******************************************************************/

qpstream(r,c)
int r,c;
{
int out_value;

	segment_get(&meanrunoff_seg, &out_value, r, c);
        if(out_value > 0 )
           return 1;
        else
   	   return 0;

}  /*********************   end cstreasm  ****************/

