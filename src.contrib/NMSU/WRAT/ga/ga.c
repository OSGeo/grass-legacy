/*************************** ga.c ******************************
*   This program uses a drainage direction to sum the values  
*   above a given point in a watershed.
*
*
******************************************************************/

# include "segment.h";

# include "gis.h";

struct Cell_head window;

SEGMENT  dir_seg, da_seg, in_seg, out_seg;

int rows, cols,
    dir_seg_fd, da_seg_fd, in_seg_fd, out_seg_fd,
    dir_map_fd, da_map_fd, in_map_fd, out_map_fd, 
    RR,CC;


main(argc, argv) 
    char *argv[];

{

int ret=0,                  /*  holds flags returned by functions  */

    srows, scols, len, r;

char dir_map_name[50], da_map_name[50], in_map_name[50], out_map_name[50];
	
char *dir_mapset, *da_mapset, *in_mapset, *out_mapset,
     *dir_file, *da_file, *in_file, *out_file;

CELL *cell;

G_gisinit(argv[0]); 

	dir_file = G_tempfile();
	da_file = G_tempfile();
	in_file = G_tempfile();
	out_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
	cell = G_allocate_cell_buf();

/*   if not in command line, ask for map names */

if(argc ==1)
   { 
 
   dir_mapset = G_ask_cell_old("Which drainage direction map should be used?", dir_map_name);
   if (dir_mapset == NULL)
      exit(0);

   da_mapset = G_ask_cell_old("Name of the drainage accumulatio map", da_map_name);
   if (da_mapset == NULL)
       exit(0);

   in_mapset = G_ask_cell_old("Name of the input map", in_map_name);
   if (in_mapset == NULL)
       exit(0);
   
   out_mapset = G_ask_cell_new("Name of the output map", out_map_name);
   if (out_mapset == NULL)
       exit(0);
    }

/* if there is a command line read it to map names  */

if(argc > 1)
    {
     sscanf(argv[1], "%s", dir_map_name);
     sscanf(argv[2], "%s", da_map_name);
     sscanf(argv[3], "%s", in_map_name);
     sscanf(argv[4], "%s", out_map_name);
                                         /* find mapsets */
     out_mapset = G_mapset();
     da_mapset = G_find_file("cell", da_map_name,"");
     in_mapset = G_find_file("cell", in_map_name,"");
     dir_mapset = G_find_file("cell", dir_map_name,"");
    }

        /*    open needed map layers    */

	dir_map_fd = G_open_cell_old(dir_map_name, dir_mapset);
           if (dir_map_fd == 0)
              printf("unable to open drainage direction map."), exit(0);

	da_map_fd = G_open_cell_old(da_map_name, da_mapset);
           if (da_map_fd == 0)
              printf("unable to open drainage accumulation map."), exit(0);

	in_map_fd = G_open_cell_old(in_map_name, in_mapset);
           if (in_map_fd == 0)
              printf("unable to open input map."), exit(0);

	out_map_fd = G_open_cell_new(out_map_name, out_mapset);
           if (out_map_fd == 0)
              printf("unable to open output map."), exit(0);



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

	in_seg_fd = creat(in_file, 0666);
	segment_format(in_seg_fd, rows, cols, srows, scols, len);
	close(in_seg_fd);

	out_seg_fd = creat(out_file, 0666);
	segment_format(out_seg_fd, rows, cols, srows, scols, len);
	close(out_seg_fd);

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

      in_seg_fd = open (in_file,2);
	ret = segment_init(&in_seg, in_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize n_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      out_seg_fd = open (out_file,2);
	ret = segment_init(&out_seg, out_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize out_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);



/***  read drainage direction file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (dir_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&dir_seg, cell, r);
	   
            G_get_map_row (da_map_fd, cell, r); 
	    segment_put_row(&da_seg, cell, r);
	    
            G_get_map_row (in_map_fd, cell, r); 
	    segment_put_row(&in_seg, cell, r);
	    }

/********** call the functions that do the work  ***********/
        

	gafind();


/***********  write da to cell file  *********************/

	segment_flush(&out_seg);

	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&out_seg, cell, r);
	   if(G_put_map_row(out_map_fd, cell, r)<0)
	      exit(1);
	   }

/*************  release segments, close all files  ********************/

	segment_release(&dir_seg);
	segment_release(&da_seg);
	segment_release(&in_seg);
	segment_release(&out_seg);

	close (dir_seg_fd);
	close (da_seg_fd);
	close (in_seg_fd);
	close (out_seg_fd);

	G_close_cell(dir_map_fd);
	G_close_cell(da_map_fd);
	G_close_cell(in_map_fd);
	G_close_cell(out_map_fd);


return(0);

}  /************************  end of main *****************************/





/****************************  gafind  *********************************/
/**  dafind() searches the da_map for cells marked 1 in findtops()    **/
/**  from each top dafind flows down hill according to dir_map adding **/
/**  up the drainage accumulation along the way.  Once the path       **/
/**  intersects an exisisting path the tributary area is added to     **/
/**  each cell in the existing "stream" until it exits the map.       **/
/***********************************************************************/


gafind()
{
	int r, c, da_value,
	   in_value,  out_value,
           next_out;

	  for (RR=1; RR<=rows-2; RR++)
	    {
            printf("processing row %d of %d\n",RR,rows-2);
	    for (CC=1; CC<=cols-2; CC++)
		{
	        segment_get(&da_seg, &da_value, RR, CC);
		if (da_value == 1)
		   {

		   r=RR, c=CC;
	           segment_get(&in_seg, &in_value, r, c);
		   segment_put(&out_seg, &in_value, r, c); 
                   next_out = in_value;
                   nextrc(&r, &c);

		   while (!cexit(r,c) && !stream(r,c) )
	               {
	                segment_get(&in_seg, &in_value, r , c);
		        next_out = in_value + next_out;
		        segment_put(&out_seg, &next_out, r, c); 
                        segment_flush(&out_seg);
                        nextrc(&r, &c);
		       }


		  while ( !cexit(r, c) )
		       {
		        segment_get(&out_seg, &out_value, r, c);
		        out_value = out_value + next_out;
	                segment_put(&out_seg, &out_value, r, c);
                        segment_flush(&out_seg);
                        nextrc(&r, &c);
		       }
		 }  /********** end if da[RR][CC] ==1 *****/
	    }  /***** end for C ********/
     }  /*****  end for R *****/
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


/***************************** cstream ****************************/
/**   cstream() checks for the intersection with an established  **/
/**   stream from the draining of a previous path                **/
/******************************************************************/

stream(r,c)
int r,c;
{
int out_value;

	segment_get(&out_seg, &out_value, r, c);
        if(out_value > 0 )
           return 1;
        else
   	   return 0;

}  /*********************   end cstreasm  ****************/



