/************************ ideal **************************************
*
*   Ideal produces an idealized elevation model that has no "pits"
*   and every cell can drain to a boarder cell.  It copies the original 
*   dem to the "ideal" layer.  Then it drains each cell filling every 
*   pit along the way.
*
*************************************************************************/


# include "segment.h";

# include "gis.h";

# include "wrat.h";


SEGMENT elev_seg, ideal_seg, lake_seg, out_seg;


int s, rmin, cmin, rx, ry, cx, cy,
    lake_seg_fd, out_seg_fd,
    lake_map_fd, out_map_fd;



ideal ()

{


char  lake_map_name[50],  out_map_name[50];
	
char *lake_mapset, *out_mapset,
     *lake_file, *out_file;

CELL *cell;

	ideal_file = G_tempfile();
	lake_file = G_tempfile();
        out_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
	cell = G_allocate_cell_buf();

/*   if not in command line, ask for map names */

                                         /* find mapsets */
  ideal_mapset = G_mapset();


strcpy(lake_map_name, "cutelake");
strcpy(out_map_name, "cuteout");

/*    open needed map layers    */

	elev_map_fd = G_open_cell_old(elev_map_name, elev_mapset);
	   if (elev_map_fd == 0)
	      printf("unable to open elevation map"), exit(0);

	ideal_map_fd = G_open_cell_new(ideal_map_name, ideal_mapset);
           if (ideal_map_fd == 0)
              printf("unable to open processed ele.  map."), exit(0);

	lake_map_fd = G_open_cell_new(lake_map_name, lake_mapset);
           if (lake_map_fd == 0)
              printf("unable to open  lake map."), exit(0);

	out_map_fd = G_open_cell_new(out_map_name, out_mapset);
           if (out_map_fd == 0)
              printf("unable to open  out map."), exit(0);



/*     set parameters for segments   */
	len = sizeof(CELL);
	srows = rows/6 + 2;
	scols = cols/6 + 2;


/* create segment files for elevation & drainage direction */

	ideal_seg_fd = creat(ideal_file, 0666);
	segment_format(ideal_seg_fd, rows, cols, srows, scols, len);
	close(ideal_seg_fd);

	lake_seg_fd = creat(lake_file, 0666);
	segment_format(lake_seg_fd, rows, cols, srows, scols, len);
	close(lake_seg_fd);

	out_seg_fd = creat(out_file, 0666);
	segment_format(out_seg_fd, rows, cols, srows, scols, len);
	close(out_seg_fd);


/*  open iniitialize & segment files  */

      ideal_seg_fd = open (ideal_file,2);
	ret = segment_init(&ideal_seg, ideal_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize ideal_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      lake_seg_fd = open (lake_file,2);
	ret = segment_init(&lake_seg, lake_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize lake_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      out_seg_fd = open (out_file,2);
	ret = segment_init(&out_seg, out_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize out_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);


/***  read elevation file into segment **********/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (elev_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&ideal_seg, cell, r);
	    }

/******* call the functions that do the work  ******/


	control();


/*************  write to cell file  *****************/

	segment_flush(&ideal_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&ideal_seg, cell, r);
	   if(G_put_map_row(ideal_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  release segments, close all files  ***/

	segment_release(&elev_seg);
	segment_release(&ideal_seg);
	segment_release(&lake_seg);
	segment_release(&out_seg);


	close (elev_seg_fd);
	close (ideal_seg_fd);
	close (lake_seg_fd);
	close (out_seg_fd);

	G_close_cell(elev_map_fd);
	G_close_cell(ideal_map_fd);


return(1);

}  /************************  end of main **************************/

/*********************** control **********************************
*
*   control runs the row by row search ofr pits and counts the
*   number of pits processed.
*
********************************************************************/

control()

{
int r,c, out, pit, pits=0;

for(r=1;r<rows-1;r++)
   {
   fprintf(stderr," Processing row %d of %d\r",r,rows-1);
   fflush(stderr);
   for(c=1;c<cols-1;c++)
      {
      segment_get(&out_seg, &out, r,c);
      if(out==0)                          /* avoids filling the same   */
        {                                /* lake at window edge again */
        pit=pitfiller(r,c);
        if(pit)
          pits++;
        }
      }
   }
   fprintf(stderr,"\n");
}


/************************* outfil ***********************************
*  
*  Marks lakes that drain to the edge so they need not be drained
*  again.
*
*********************************************************************/

fillout()
{
int r, c, out=1, lake=0;

for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {
      segment_get(&lake_seg, &lake, r,c);
      if(lake)
         segment_put(&out_seg, &out, r,c);
      }
   }
}


/***************************** pitfiller **************************
*
*   Pit filler does most of the work.  It is passed the R & C 
*   from control.  setsearch defines the search area around R,C
*   a lake is defined as the R,C cell and adjacent nonlake cells
*   are tested until an outlet is found.  If no outlet is found
*   the lake is filled to the altitude of the minimum neighbor and
*   the search is continued until an outlet (cell adjacent to and
*   lower then the lake) is found or the lake hits the edge of the
*   window.  
*
*******************************************************************/
pitfiller(R,C)
int R,C;
{
int r, c, nele, wedge, lake, oldmin;
static int lakelevel, out=0;
resetlake();                                 /* makes all cell = 0 */
segment_get(&ideal_seg, &lakelevel, R, C);
s=0,lake=1;
segment_put(&lake_seg, &lake, R, C);

while(!out)
{
setsearch(R,C);                          /* define search area  */


oldmin=30000;
for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {
      segment_get(&lake_seg, &lake, r, c);
      if(lake)
        {

        if(r>0 &&c>0)
          {
           segment_get(&lake_seg, &lake, r-1,c-1);
           if(!lake)
             {
             segment_get(&ideal_seg, &nele, r-1,c-1);
             if(nele<oldmin)
                oldmin=nele;
             }
          }
 
        if(r>0)
          {
           segment_get(&lake_seg, &lake, r-1,c);
           if(!lake)
             {
             segment_get(&ideal_seg, &nele, r-1,c);
             if(nele<oldmin)
                oldmin=nele;
             }
          }
 
        if(r>0 &&c<cols-1)
          {
     	  segment_get(&lake_seg, &lake, r-1,c+1);
          if(!lake)
            {
            segment_get(&ideal_seg, &nele, r-1,c+1);
            if(nele<oldmin)
               oldmin=nele;
            }
          }
 
        if(c>0)
          {
          segment_get(&lake_seg, &lake, r,c-1);
          if(!lake)
            {
            segment_get(&ideal_seg, &nele, r,c-1);
            if(nele<oldmin)
              oldmin=nele;
            }
         }
 
        if(c<cols-1)
          {
          segment_get(&lake_seg, &lake, r,c+1);
          if(!lake)
            {
            segment_get(&ideal_seg, &nele, r,c+1);
            if(nele<oldmin)
              oldmin=nele;
            }
         }
 
        if(r<rows-1 &&c>0)
          {
          segment_get(&lake_seg, &lake, r+1,c-1);
          if(!lake)
            {
            segment_get(&ideal_seg, &nele, r+1,c-1);
            if(nele<oldmin)
            oldmin=nele;
            }
         }          
 
        if(r<rows-1)
          {
          segment_get(&lake_seg, &lake, r+1,c);
          if(!lake)
            {
            segment_get(&ideal_seg, &nele, r+1,c);
            if(nele<oldmin)
              oldmin=nele;
            }
         }
 
        if(r<rows-1 && c<cols-1)
          {
          segment_get(&lake_seg, &lake, r+1,c+1);
          if(!lake)
            {
            segment_get(&ideal_seg, &nele, r+1,c+1);
            if(nele<oldmin)
              oldmin=nele;
            }
         }

        } /*  end if lake */
      }  /* end c */
     }  /* end r */

   
     if(oldmin<lakelevel)             /* outlet found */
       return(0);                     /* go to next R,C */


     if(oldmin>=lakelevel)            /* no outlet found */
       {
       lakelevel = oldmin;       
       marklake(lakelevel,R,C);      /* fill pit and mark lake */
       }

     wedge=0;
     if(rx==0 || ry==rows-1 || cx==0 || cy==0)
        wedge = wedgetest();                   /* test for lake at */
                                               /* edge of window   */

    if(wedge)                                  /* go to next R,C */
      {
      fillout();
      return(1);
      }


    s=s+1;                  /* expand the search for an outlet */

 } /*** end while out ********/

} /****************** end pitfiller  ******************************/
 
/*********************  set search *****************************
*
*   set search sets the rx, ry, cx & cy that are used to limit
*   the part of the window searched for sells to be drained.
*
****************************************************************/

setsearch(R,C)
int R,C;
{
     rx=R-s, ry=R+s;
     cx=C-s, cy=C+s;

     if(cx<0)
        cx=0;
     if(cy>cols-1)
        cy=cols-1;
     if(rx<0)
        rx=0;
     if(ry==rows-1)
        ry=rows-1;
}

/********************* makrlake **********************************
*
*   marklake searches the current search area for lake cells.  
*   When found the cells adjacent to the lake cells are tested.
*   If they are not lake cells and are <= lake level they are 
*   marked "2" new lake cells.  
*      Then all lake cells are set =1 and their elevations 
*   raised to the lake level.
*
******************************************************************/

marklake(lakelevel,R,C)
int lakelevel,R,C;
{
int r,c,ele, lake=1, newlake=2;
segment_put(&lake_seg, &lake, R,C);

for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {
      segment_get(&lake_seg, &lake, r, c);
      if(lake==1)
        {
        
        if(r>0 && c>0)                   /* bounds check */
          {
          segment_get(&lake_seg, &lake, r-1, c-1);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r-1, c-1);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r-1, c-1);
            }
         }
        
        if(r>0)
          {
          segment_get(&lake_seg, &lake, r-1, c);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r-1, c);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r-1, c);
            }
         }
        
        if(r>0 && c<cols-1)
          {
          segment_get(&lake_seg, &lake, r-1, c+1);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r-1, c+1);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r-1, c+1);
            }
         }
        
        if(c>0)
          {
          segment_get(&lake_seg, &lake, r, c-1);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r, c-1);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r, c-1);
            }
         }
        
        if(c<cols-1)
          {
          segment_get(&lake_seg, &lake, r, c+1);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r, c+1);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r, c+1);
            }
         }
        
        if(r<rows-1 && c>0)
          {
          segment_get(&lake_seg, &lake, r+1, c-1);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r+1, c-1);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r+1, c-1);
            }
         }
        
        if(r<rows-1)
          {
          segment_get(&lake_seg, &lake, r+1, c);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r+1, c);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r+1, c);
            }
         }
        
        if(r<rows-1 && c<cols-1)
          {
          segment_get(&lake_seg, &lake, r+1, c+1);
          if(lake==0)
            {
            segment_get(&ideal_seg, &ele, r+1, c+1);
            if(ele<=lakelevel)
              segment_put(&lake_seg, &newlake, r+1, c+1);
            }
         }

       } /************* end if lake  *************/
      } /**********  end for c ***********/
   }   /********* enf for r *************/

lake=1;
for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {
      segment_get(&lake_seg, &newlake, r, c);     /* up date all   */
      if(newlake==2)                              /* lake cells    */
        segment_put(&lake_seg, &lake, r,c);       /* to 1 and fill */
      if(newlake==2 || newlake==1)                /* to lakelevel  */
        segment_put(&ideal_seg, &lakelevel, r, c);
      }
   }

}  /****************** end marklake ****************************/


/**********************  wedgetest *******************************
*
*   Wedge test searches the borders of the window within the 
*   current search area to see if current lake has reached the 
*   window edge.
*
*****************************************************************/

wedgetest()
{
int r,c, lake=0;
if(rx == 0)
  {
  for(c=0;c<cols;c++)
     {
     segment_get(&lake_seg, &lake, 0, c);
     if(lake)
       return(1);
     }
   }

if(ry == rows-1)
  {
  for(c=0;c<cols;c++)
     {
     segment_get(&lake_seg, &lake, rows-1, c);
     if(lake)
       return(1);
     }
   }

if(cx == 0)
  {
  for(r=0;r<rows;r++)
     {
     segment_get(&lake_seg, &lake, r, 0);
     if(lake)
       return(1);
     }
   }

if(cy == cols-1)
  {
  for(r=0;r<rows;r++)
     {
     segment_get(&lake_seg, &lake, r, cols-1);
     if(lake)
       return(1);
     }
   }

return(0);
}


/**************************  resetlake *****************************
*
*   reset lake sets all cells in the lake layer in the current
*   search area = 0 in preprration for the next round of pitfinder.
*
********************************************************************/
resetlake()
{
int r,c,lake=0;

for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {	
      segment_put(&lake_seg, &lake, r, c);
      }
   }
}

/**************************  lake print ****************************
*
*   A utility to see what the lake looks like 
*
********************************************************************/

lakeprint()
{
int r, c, lake;
for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {
      segment_get(&lake_seg, &lake, r,c);
      printf("%d ",lake);
      }
    printf("\n");
   }
}
