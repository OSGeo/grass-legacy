/************************ upall.c  ******************************
*
*   This program uses a dem to assign drainage directions. 
*
******************************************************************/


# include "segment.h";

# include "gis.h";

# include "wrat.h";

struct Colors colors;

SEGMENT ideal_seg, dir_seg, da_seg;


int rmin, cmin, rx, ry, cx, cy,
    rsmax, rsmin, csmax,csmin;

upall()
{

CELL *cell;
char hold_da[80];

	ideal_file = G_tempfile();
	dir_file = G_tempfile();
	da_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
	cell = G_allocate_cell_buf();

        strcpy(hold_da, da_map_name);
        strcpy(da_map_name, "cme");

/*    open needed map layers    */

	ideal_map_fd = G_open_cell_old(ideal_map_name, ideal_mapset);
	   if (ideal_map_fd == 0)
	      printf("unable to open elevation map"), exit(0);

	dir_map_fd = G_open_cell_new(dir_map_name, dir_mapset);
           if (dir_map_fd == 0)
              printf("unable to open drainage direction map."), exit(0);

	da_map_fd = G_open_cell_new(da_map_name, da_mapset);
           if (da_map_fd == 0)
              printf("unable to open drainage accumulation map."), exit(0);



/*     set parameters for segments   */
	len = sizeof(CELL);
	srows = rows/6 + 2;
	scols = cols/6 + 2;



/* create segment files for elevation & drainage direction */

	ideal_seg_fd = creat(ideal_file, 0666);
	segment_format(ideal_seg_fd, rows, cols, srows, scols, len);
	close(ideal_seg_fd);

	dir_seg_fd = creat(dir_file, 0666);
	segment_format(dir_seg_fd, rows, cols, srows, scols, len);
	close(dir_seg_fd);

	da_seg_fd = creat(da_file, 0666);
	segment_format(da_seg_fd, rows, cols, srows, scols, len);
	close(da_seg_fd);

/*  open iniitialize & segment files  */

      ideal_seg_fd = open (ideal_file,2);
	ret = segment_init(&ideal_seg, ideal_seg_fd,3);
	   if (ret == -1)
	      printf("could not initialize ideal_seg "), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

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


/***  read elevation file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (ideal_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&ideal_seg, cell, r);
	    segment_flush(&ideal_seg);
	    }

/********** call the functions that do the work  ***********/


       ret = dircontrol();


/*************  write to cell file  ********************/

	segment_flush(&da_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&da_seg, cell, r);
	   if(G_put_map_row(da_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  write to cell file  ********************/

	segment_flush(&dir_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&dir_seg, cell, r);
	   if(G_put_map_row(dir_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  release segments, close all files  ********************/

	segment_release(&ideal_seg);
	segment_release(&dir_seg);
	segment_release(&da_seg);

	close (ideal_seg_fd);
	close (dir_seg_fd);
	close (da_seg_fd);

	G_close_cell(ideal_map_fd);
	G_close_cell(dir_map_fd);
 	G_close_cell(da_map_fd);

        sprintf(command, "g.remove rast=cme");
        G_system(command);
        strcpy(da_map_name, hold_da);

       ret = setcolors(dir_map_name);

return(1);

}  /************************  end of main **************************/


/*********************************************************************
*
*  control filters the dem and calls the other functions
*
**********************************************************************/
dircontrol()
{
static int maxele, minele;
int r, c, ele, draining, newedge=1, holes=0;

minele = 30000;

 
for(r=0;r<rows;r++)            /*********** find max elev **********/
   {
   for(c=0;c<cols;c++)
      {
      segment_get(&ideal_seg, &ele, r, c);
      if(maxele < ele)
        maxele = ele;
      if( minele > ele && ele > 0 )
        minele = ele;
      }
   }
printf(" max elev = %d  min elev = %d \n",maxele,minele);

findout();

drainout(maxele, minele);

return(1);
}  /********************  end control  *****************************/


/*********************** findout **************************
*
*
*
**********************************************************/
findout()
{
int r,c, elev, dir,lastele,
     ret, drsinpoint, finding, inland=3;

int down = 0, up = 0, flat = 0, upcount = 0;

segment_get(&ideal_seg, &lastele, 0,0);

for(r=1;r<rows;r++)
    {
    segment_get(&ideal_seg, &elev, r,0);
    if(elev == 0)
        flat = 0, down=0, up=1;
    if(elev > 0)
        {
        if(elev<lastele)
           down = 1, up=0, upcount=0,flat=0;
        if(elev == lastele)
           flat++;
        if(elev > lastele)
           up = 1, upcount++;
        if(upcount > 1)
           down = 0;
        lastele = elev;
        if(down && up)
           {
           if(flat < r)   /* if flat = r wait till final pass */
              {
              rmin = r-(flat+2)/2, cmin=0;
              inland = checkin();
              if(inland==2)
                 ret = drainedge();
              }
           flat = 0, down=0, up=1;
           }
       }
    }

for(c=1;c<cols-1;c++)
    {
     segment_get(&ideal_seg, &elev, rows-1,c);
    if(elev == 0)
        flat = 0, down=0, up=1;
    if(elev > 0)
        {
     if(elev<lastele)
        down = 1, up=0, upcount=0, flat=0;
     if(elev == lastele)
        flat++;
     if(elev > lastele)
       up = 1, upcount++;
     if(upcount > 1)
        down = 0;
     lastele = elev;
     if(down && up)
        {
        if(.5*flat<=c)
           cmin = c-(flat+2)/2, rmin = rows-1;
        else 
           cmin = 0, rmin=rows-1-(.5*flat-c);
        inland = checkin();
        if(inland==2)
           ret = drainedge();
        flat = 0, down=0, up=1;
        } 
      }
     }

for(r=rows-2;r>=0;r--)
    {
    segment_get(&ideal_seg, &elev, r,cols-1);
    if(elev == 0)
        flat = 0, down=0, up=1;
    if(elev > 0)
        {
    if(elev<lastele)
       down = 1, up=0, upcount=0, flat=0;
    if(elev == lastele)
       flat++;
    if(elev > lastele)
       up = 1, upcount++;
    if(upcount > 1)
       down = 0;
    lastele = elev;
    if(down && up)
       {
       if(.5*flat<=rows-1-r)
           rmin = r+(flat+2)/2, cmin=cols-1;
        else 
           rmin = rows-1, cmin=rows-1-(.5*flat-(rows-1-r));
        inland = checkin();
    if(inland==2)
       ret = drainedge();
       flat = 0, down=0, up=1;
       }
     }
    }

for(c=cols-2;c>=0;c--)
    {
    segment_get(&ideal_seg, &elev, 0, c);
    if(elev == 0)
        flat = 0, down=0, up=1;
    if(elev > 0)
        {
    if(elev<lastele)
       down = 1, up=0, upcount=0, flat=0;
    if(elev == lastele)
       flat = flat + 1;
    if(elev > lastele)
       up = 1, upcount++;
    if(upcount > 1)
       down = 0;
    lastele = elev;
    if(down && up)
       {
       if(.5*flat<=cols-1-c)
          cmin = c+(flat+2)/2, rmin = 0;
        else 
          cmin = cols-1, rmin=.5*flat-(cols-1-c);
       inland = checkin();
       if(inland==2)
           ret = drainedge();
        flat = 0, down=0, up=1;
        } 
      }
      }

for(r=1;r<rows;r++)
    {
    segment_get(&ideal_seg, &elev, r,0);
    if(elev == 0)
        flat = 0, down=0, up=1;
    if(elev > 0)
        {
    if(elev<lastele)
       down = 1, up=0, upcount=0, flat=0;
    if(elev == lastele)
       flat++;
    if(elev > lastele)
       up = 1, upcount++;
    if(upcount > 1)
       down = 0;
    lastele = elev;
    if(down && up)
       {
       if(.5*flat <= r)
          rmin = r-(flat+2)/2, cmin=0;
       else
          rmin = 0, cmin=.5*flat-r;
       inland = checkin();
       if(inland==2)
          ret = drainedge();
       flat = 0, down=0, up=1;
       }
      }
     }
return(1);
} /***********************  end findout *****************/



/************************ drainout ***********************
*
*  drain out finds the watershed above an outlet
*
*************************************************************/

drainout(maxelev,curele)
int maxelev,curele;
{
int  draining=1, dir=0, cda=1, elev,
    r, c, da, minda, edge, ret;

ret = markedge();

while(curele <= maxelev)
     {
     while(draining)
       {
       draining = 0;
       for(r=0;r<rows;r++)
          {
          for(c=0;c<cols;c++)
             {
             segment_get(&ideal_seg, &elev, r, c);
             if(elev==curele)
                {
                segment_get(&dir_seg, &dir, r, c);
                if(dir==0)
                   {
                   minda = 9000000;

                  if(r>0)
                    {
                    segment_get(&da_seg, &da, r-1, c);
                    if(da < minda && da>0)
                       {
                       dir=3;
                       minda=da;
                       }
                    }

                   if(r>0 && c>0)
                     {
                     segment_get(&da_seg, &da, r-1, c-1);
                     if(da< minda && da>0)
                        {
                         dir=4;
                         minda=da;
                        }
                     }

                  if(r>0 && c<cols-1)
                    { 
                    segment_get(&da_seg, &da, r-1, c+1);
                    if(da < minda && da>0)
                       {
                       dir=2;
                       minda=da;
                       }
                    }

                  if(c>0)
                    {
                    segment_get(&da_seg, &da, r, c-1);
                    if(da < minda && da>0)
                       {
                       dir=5;
                       minda=da;
                       }
                    }

                  if(c<cols-1)
                    {
                    segment_get(&da_seg, &da, r, c+1);
                    if(da < minda && da>0)
                       {
                       dir=1;
                       minda=da;
                       }
                    }

                  if(r<rows-1 && c>0)
                    {
                    segment_get(&da_seg, &da, r+1, c-1);
                    if(da < minda && da>0)
                       {
                       dir=6;
                       minda=da;
                       }
                    }

                  if(r<rows-1)
                    {
                    segment_get(&da_seg, &da, r+1, c);
                    if(da < minda && da>0)
                       {
                       dir=7;
                       minda=da;
                       }
                    } 

                  if(r<rows-1 && c<cols-1)
                    {
                    segment_get(&da_seg, &da, r+1, c+1);
                    if(da < minda && da>0)
                       {
                       dir=8;
                       minda=da;
                       }
                    }

                 if(dir>0)
                   {
                   segment_put(&dir_seg, &dir, r, c);
                   segment_put(&da_seg, &cda, r, c);
                   cda++;
                   draining = 1;
                   } 

               }  /** end if !dir   **/
              }  /** end if ele    **/
           }   /*** end for c  ****/
         }    /***  end for r  ***/
        cda++;
        }    /*** end while draining ***/
      curele++, cda++, draining = 1;
fprintf(stderr,"draining cells of elev %d    maximum elevation %d  \r",curele, maxelev);
     fflush(stderr);
      }    /*** end while curele  ***/
fprintf(stderr,"\n");
return(1);
}


/***********************  drainedge *******************************/
drainedge()
{
int dir = 0;
if(rmin==0)
  {
  if(cmin==0)
    dir=4;
  if(cmin==cols-1)
    dir=2;
  if(cmin>0 && cmin<cols-1)
     dir=3;
  }

if(rmin==rows-1)
  {
  if(cmin==0)
    dir=6;
  if(cmin==cols-1)
    dir=8;
  if(cmin>0 && cmin<cols-1)
    dir=7;
  }

if(cmin==0 && (rmin>0 && rmin<rows-1))
   dir=5;
if(cmin==cols-1 && (rmin>0 && rmin<rows-1))
   dir=1;
segment_put(&dir_seg, &dir, rmin,cmin);
return(0);
}

/*********************  set search *****************************
*
*   set search sets the rx, ry, cx & cy that are used to limit
*   the part of the window searched for sells to be drained.
*
****************************************************************/

setdirsearch(s)
int s;
{
/** set search according to size around top of search **/

     if(rmin==0)
        rx=0, ry=s;
     if(rmin==rows-1)     
        rx=rows-1-s, ry=rows-1;
     if(rmin>0 && rmin<rows-1)
        rx=rmin-s, ry=rmin+s;
     if(cmin==0)
        cx=0, cy=s;
     if(cmin==cols-1)     
       cx=cols-1-s, cy=cols-1;
     if(cmin>0 && cmin<cols-1)
       cx=cmin-s, cy=cmin+s;
     if(cx<0)
        cx=0;
     if(cy>cols-1)
        cy=cols-1;
     if(rx<0)
        rx=0;
     if(ry==rows-1)
        ry=rows-1;
}


/************************  resetda ************************
*
* resets all values on da to 0 
*
**********************************************************/

resetda()
{
int r,c, da=0;
for(r=rx;r<=ry;r++)
   {
   for(c=cx;c<=cy;c++)
      {
      if(r>=0 && r<rows &&c>=0 && c<cols)
        segment_put(&da_seg, &da, r,c);
      }
   }
}

/*********************** checkin **************************
*
*
**********************************************************/

checkin()
{
int outele, nele,r,c;
int s=0, minelev, da, elev, lake =1;
segment_get(&ideal_seg, &outele, rmin,cmin);
resetda();
segment_put(&da_seg,&lake, rmin, cmin);
setdirsearch(s);
while(1)
   {
   minelev = 30000;
   for(r=rx;r<=ry;r++)        /* find minimum elevation of cells */
      {                       /* next to possible outlet         */
      for(c=cx;c<=cy;c++)
         {
         if(r>= 0 && r < rows && c >= 0 && c < cols)
           {
           segment_get(&da_seg, &da,r,c);
           if(da==1)
             {
                   if(r>0 && c>0)
                     {
                     segment_get(&da_seg, &da, r-1, c-1);
                     if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r-1, c-1);
                       if(elev < minelev)
                          minelev=elev;
                       }
                     }

                  if(r>0)
                    {
                    segment_get(&da_seg, &da, r-1, c);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r-1, c);
                       if(elev < minelev)
                          minelev=elev;
                       }
                     }

                  if(r>0 && c<cols-1)
                    { 
                    segment_get(&da_seg, &da, r-1, c+1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r-1, c+1);
                       if(elev < minelev)
                          minelev=elev;
                       }
                    }

                  if(c>0)
                    {
                    segment_get(&da_seg, &da, r, c-1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r, c-1);
                       if(elev < minelev)
                          minelev=elev;
                       }
                    }
                  if(c<cols-1)
                    {
                    segment_get(&da_seg, &da, r, c+1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r, c+1);
                        if(elev < minelev)
                            minelev=elev;
                        }
                    }

                  if(r<rows-1 && c>0)
                    {
                    segment_get(&da_seg, &da, r+1, c-1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r+1, c-1);
                       if(elev < minelev)
                          minelev=elev;
                       }
                    }

                  if(r<rows-1)
                    {
                    segment_get(&da_seg, &da, r+1, c);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r+1, c);
                       if(elev < minelev)
                          minelev=elev;
                       }
                    } 

                  if(r<rows-1 && c<cols-1)
                    {
                    segment_get(&da_seg, &da, r+1, c+1);
                    if(da ==10)
                       {
                       segment_get(&ideal_seg, &elev, r+1, c+1);
                       if(elev < minelev)
                          minelev=elev;
                       }
                     }

             } /*end if */
           } /* end bounds if */
         } /* end for c */
      } /* end for r */

if(rmin == 1 && cmin == 0)
   printlake(s+1);

     if(minelev < outele)      /* valley drains into window */
        {
        resetda();
        return(1);
        }

     if(minelev > outele)      /* valley drains out of window */
        {
        resetda();
        return(2);
        }

   /********* fill in da for neighbors with same elev ********/

   for(r=rx;r<=ry;r++)
      {
      for(c=cx;c<=cy;c++)
         {
         if(r>= 0 && r < rows && c >= 0 && c < cols)
            {
            segment_get(&da_seg, &da,r,c);
            if(da==1)
               {
                   if(r>0 && c>0)
                     {
                     segment_get(&da_seg, &da, r-1, c-1);
                     if(da == 0)
                        {
                        segment_get(&ideal_seg, &elev, r-1, c-1);
                        if(elev == minelev)
                          segment_put(&da_seg, &lake, r-1,c-1);
                        }
                     }

                  if(r>0)
                    {
                    segment_get(&da_seg, &da, r-1, c);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r-1, c);
                       if(elev == minelev)
                          segment_put(&da_seg, &lake, r-1,c);
                       }
                    }

                  if(r>0 && c<cols-1)
                    { 
                    segment_get(&da_seg, &da, r-1, c+1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r-1, c+1);
                       if(elev == minelev)
                         segment_put(&da_seg, &lake, r-1,c+1);
                       }
                    }

                  if(c>0)
                    {
                    segment_get(&da_seg, &da, r, c-1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r, c-1);
                       if(elev == minelev)
                         segment_put(&da_seg, &lake, r,c-1);
                       }
                    }

                  if(c<cols-1)
                    {
                    segment_get(&da_seg, &da, r, c+1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r, c+1);
                       if(elev == minelev)
                         segment_put(&da_seg, &lake, r,c+1);
                        }
                    }

                  if(r<rows-1 && c>0)
                    {
                    segment_get(&da_seg, &da, r+1, c-1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r+1, c-1);
                       if(elev == minelev)
                         segment_put(&da_seg, &lake, r+1,c-1);
                       }
                    }

                  if(r<rows-1)
                    {
                    segment_get(&da_seg, &da, r+1, c);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r+1, c);
                       if(elev == minelev)
                         segment_put(&da_seg, &lake, r+1, c);
                       }
                    } 

                  if(r<rows-1 && c<cols-1)
                    {
                    segment_get(&da_seg, &da, r+1, c+1);
                    if(da == 0)
                       {
                       segment_get(&ideal_seg, &elev, r+1, c+1);
                       if(elev == minelev)
                          segment_put(&da_seg, &lake, r+1,c+1);
                       }
                    }
                   
                }  /** end if **/
              } /** end bounds if **/
            }  /** end for c **/
         }  /** end for r **/
     s++;
     setdirsearch(s);
     } /********* end while ***************/

 } /***************** end checkin  *******************************/

printlake(s)
int s;
{
int r,c,da;
printf("\n");
   for(c=cmin-s;c<=cmin+s;c++)
      {
       if( c>=0 && c<cols)
      printf("%d ",c);
      }
   printf(" \n");
for(r=rmin-s;r<=rmin+s;r++)
   {
   if(r>=0  && r<rows )
   printf("%d   ",r);
   for(c=cmin-s;c<=cmin+s;c++)
       {
       if(r>=0 && c>=0 && r<rows && c<cols)
         {
         segment_get(&da_seg, &da, r,c);
         printf("%d ",da);
         }
       }
   printf("  ");
   for(c=cmin-s;c<=cmin+s;c++)
       {
       if(r>=0 && c>=0 && r<rows && c<cols)
         {
         segment_get(&ideal_seg, &da, r,c);
         printf("%d ",da);
         }
       }
   printf(" \n");
   }
printf("\n\n");
}


/**************** markedge *******************************
*
*  this function searches the edge for cells that have a
* drainage direction and markes them in the da layer.
*
*********************************************************/
markedge()
{
int r, c, dir, ele, da = 1;

  for(r=0;r<rows;r++)
     {
     for(c=0;c<cols;c++)
         {
         segment_get(&dir_seg, &dir, r,c);
         segment_get(&ideal_seg, &ele, r,c);
         if( dir > 0 || ele == 0 )
         segment_put(&da_seg, &da,r, c);
         }
     }
return(1);
}

setcolors(mapname)
char mapname[50];
{
G_init_colors(&colors);
G_set_color(0,0,0,0,&colors);
G_set_color(1,30,0,255,&colors);
G_set_color(2,0,255,255,&colors);
G_set_color(3,0,225,0,&colors);
G_set_color(4,245,150,150,&colors);
G_set_color(5,255,0,0,&colors);
G_set_color(6,255,165,0,&colors);
G_set_color(7,255,255,0,&colors);
G_set_color(8,145,155,255,&colors);

G_write_colors(mapname,G_mapset(),&colors);
G_free_colors(&colors);

return(1);

}
