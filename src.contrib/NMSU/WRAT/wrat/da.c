
 /*************************** da.c ******************************
*   This program uses a drainage direction to assign drainage 
*   accumulations. 
*
*
******************************************************************/


# include "segment.h";

# include "gis.h";

# include "wrat.h";

struct Range range;
struct Colors colors;

SEGMENT  dir_seg, da_seg;


int   RR,CC;



da()
{


CELL *cell;


	dir_file = G_tempfile();
	da_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
	cell = G_allocate_cell_buf();


/*    open needed map layers    */


	dir_map_fd = G_open_cell_old(dir_map_name, dir_mapset);
           if (dir_map_fd == 0)
              printf("unable to open drainage direction map."), exit(0);

	da_map_fd = G_open_cell_new(da_map_name, da_mapset);
           if (da_map_fd == 0)
              printf("unable to open drainage accumulation map."), exit(0);



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


/***  read drainage direction file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (dir_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&dir_seg, cell, r);
	    }
	    segment_flush(&dir_seg);

/********** call the functions that do the work  ***********/
        

	findtops();

	dafind();


/***********  write da to cell file  *********************/

	segment_flush(&da_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&da_seg, cell, r);
	   if(G_put_map_row(da_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  release segments, close all files  ********************/

	segment_release(&dir_seg);
	segment_release(&da_seg);

	close (dir_seg_fd);
	close (da_seg_fd);




	G_close_cell(dir_map_fd);
	G_close_cell(da_map_fd);


return(1);

}  /************************  end of main *****************************/





/*************************  findtops   ***************************/
/**  findtops searches the dir[][] matrix for cells into which  **/
/**  no other cells drain.  these are marked with a 1 in the    **/
/**  da[][] matrix as "1".  these are the "tops of hills".      **/
/*****************************************************************/


findtops()
{
   int  downslope=0, r=0, c=0, top=1, dir_value;

	for (r=0; r<=rows-1; r++)            /**   check all directions  **/
	    {                              /**  for uphill cells       **/
	     for (c=0; c<=cols-1; c++)       /**  note bounds checks     **/
		{
		downslope=0;

   	        segment_get(&dir_seg, &dir_value, r, c);
                if(dir_value == 0)
                   downslope=1;
                else
                   {
                   if(c > 0)
                     {
   	              segment_get(&dir_seg, &dir_value, r, c-1);
	   	      if (dir_value == 1 ) 
		         downslope=1;
                      }

                   if(c<cols-1)
                     {
	              segment_get(&dir_seg, &dir_value, r, c+1);
	              if (dir_value == 5 )
		         downslope=1;
                      }

                   if(c >0 && r< rows-1)
                     {
	              segment_get(&dir_seg, &dir_value, r+1, c-1);
		      if (dir_value == 2 )
		         downslope=1;
                      }

                   if(r < rows-1)
                     {
	              segment_get(&dir_seg, &dir_value, r+1, c);
		      if (dir_value == 3 ) 
		         downslope=1;
                      }

                   if(r > 0)
                     {
	              segment_get(&dir_seg, &dir_value, r-1, c);
		      if (dir_value == 7 )
		         downslope=1;
                      }

                   if(c<cols-1 && r < rows-1 )
                     {
	              segment_get(&dir_seg, &dir_value, r+1, c+1);
		      if (dir_value == 4 )
		         downslope=1;
                      }

                   if(c < cols-1 && r > 0)
                     {
	              segment_get(&dir_seg, &dir_value, r-1, c+1);
		      if (dir_value == 6)
		         downslope=1;
                      }

                   if(c > 0 && r > 0)
                     {
	              segment_get(&dir_seg, &dir_value, r-1, c-1);
		      if (dir_value == 8 )
		         downslope=1;
                      }
                   }

   /*  if no neighbors drain toward the cell put 1 in da_seg at r,c */

		if (!downslope)
		   segment_put(&da_seg, &top, r, c );
		}   /*  end c  */
	   }        /*  end r  */
   segment_flush(&da_seg);
 }    /***********************  end fintops  ***********************/


/****************************  dafind  *********************************/
/**  dafind() searches the da_map for cells marked 1 in findtops()    **/
/**  from each top dafind flows down hill according to dir_map adding **/
/**  up the drainage accumulation along the way.  Once the path       **/
/**  intersects an exisisting path the tributary area is added to     **/
/**  each cell in the existing "stream" until it exits the map.       **/
/***********************************************************************/


dafind()
{
	int r, c, da_value,
           next_da;

	  for (RR=0; RR<=rows-1; RR++)
	    {
            fprintf(stderr,"processing row %d of %d\r",RR,rows-1);
	    fflush(stderr);
	    for (CC=0; CC<=cols-1; CC++)
		{
	        segment_get(&da_seg, &da_value, RR, CC);
		if (da_value == 1)
		   {
		   r=RR, c=CC;
                   nextrc(&r, &c);
                   next_da = 1;

		   while (!cexit(r,c) && !stream(r,c) )
	               {
		        next_da = next_da + 1;
		        segment_put(&da_seg, &next_da, r, c); 
                        segment_flush(&da_seg);
                        nextrc(&r, &c);
		       }


		  while ( !cexit(r, c) )
		       {
		        segment_get(&da_seg, &da_value, r, c);
		        da_value = da_value + next_da;
	                segment_put(&da_seg, &da_value, r, c);
                        segment_flush(&da_seg);
                        nextrc(&r, &c);
		       }
		 }  /********** end if da[RR][CC] ==1 *****/
	    }  /***** end for C ********/
     }  /*****  end for R *****/
  fprintf(stderr,"\n");
} /************************ end gfind **************************/




nextrc(nr,nc)
int  *nr,*nc;
{
int dir, r, c;
r = *nr;
c = *nc;

segment_get(&dir_seg, &dir, r, c);

switch(dir)
      {
       case 1:
          c=c+1;
          break;

        case 2:
           c=c+1, r=r-1;
           break;
		 
        case 3:
           r=r-1;
           break;
 
         case 4:
            c=c-1, r=r-1;
            break;
			 
          case 5:
            c=c-1;
            break;
			 
          case 6:
	    c=c-1, r=r+1;
            break;

          case 7:
            r=r+1;
            break;

          case 8:
	    c=c+1, r=r+1;
            break;
        }
       *nr = r;
       *nc = c;
}



/******************************* cexit ***************************/
/**   cexit checks for a drainage path leaving the matrix        **/
/**   returning 1 if exiting, 0 if not.                          **/
/******************************************************************/

int cexit(r,c)
int r, c;
{
int dir;
        segment_get(&dir_seg, &dir, r, c);

        if(r < 0 || r > rows-1 || c < 0 || c > cols-1 || dir == 0)
           return(1);
        else 
	   return(0);

}  /*************************  end cexit **********************/


/****************************** stream ****************************/
/**   cstream() checks for the intersection with an established  **/
/**   stream from the draining of a previous path                **/
/******************************************************************/

stream(r,c)
int r,c;
{
int da;

	segment_get(&da_seg, &da, r, c);
        if(da > 0 )
           return 1;
        else
   	   return 0;

}  /*********************   end cstreasm  ****************/

