/* %W% %G% */   
/*====================================================================*/
/* program to propogate the link label into the hillslope areas;      */
/* processes CELL files only and works on window derived from link    */
/* label map                                                          */
/*====================================================================*/
#include "gis.h"

#define NOMASK 1

static int nrows, ncols;

main() 
{
    int partfd;
    char buf[100];
    char drain_name[30], *drain_mapset;
    char ridge_name[30], *ridge_mapset;
    char part_name[30], *part_mapset;
    CELL *drain, *ridge, *part;
    CELL *read_map();
    struct Cell_head window;
    int row, col, npass, tpass;

    G_gisinit ("DUAL");


    do
      {
      do
       printf ("enter the number of passes through the dataset: ");
       while (!G_gets(buf));
      }
      while (sscanf(buf, "%d", &tpass) != 1);
      printf ("\n # of passes = %d",tpass);
	  
    drain_mapset = G_ask_cell_old ("\n enter coded stream network file name",drain_name);
    if (drain_mapset == NULL)
	exit(2);

    G_get_cellhd (drain_name, drain_mapset, &window);
    G_set_window (&window);
    nrows = G_window_rows();
    ncols = G_window_cols();

    ridge_mapset = G_ask_cell_old ("enter thinned ridge network file name",ridge_name);
    if (ridge_mapset == NULL)
	exit(2);

    part_mapset = G_ask_cell_new ("please name the resultant watershed partition file name",part_name);
    if (part_mapset == NULL)
	exit(2);

    drain = read_map (drain_name, drain_mapset, NOMASK, nrows, ncols);
    ridge = read_map (ridge_name, ridge_mapset, NOMASK, nrows, ncols);

    partfd = G_open_cell_new (part_name);
    if (partfd < 0)
    {
	char msg[100];
	sprintf (msg, "unable to create %s", part_name);
	G_fatal_error (msg);
    }

/* run through file and set streams to zero at locations where ridges exist*/ 
    for (row = 0; row < nrows; row++)
     {
     for (col = 0; col < ncols; col++)
      if (ridge[row*ncols + col] != 0) drain[row*ncols + col] = 0;
     }

 for (npass = 1; npass <= tpass; npass++)
 {
    for (row = 1; row < nrows-1; row++)
    {
     for (col = 1; col < ncols-1; col++)
      {
       if(drain[row*ncols+col] == 0 && ridge[row*ncols+col] == 0)  
        {
        if(drain[(row-1)*ncols+col] != 0 && ridge[(row-1)*ncols+col] == 0) 
         drain[row*ncols+col] = drain[(row-1)*ncols+col];  
        if(drain[row*ncols+(col-1)] !=0 && ridge[row*ncols+(col-1)] == 0 ) 
         drain[row*ncols+col] = drain[row*ncols+(col-1)]; 
        }
      }
    }
          printf ("forward sweep complete\n");

    for (row = nrows-3; row > 1; --row)
    {
     for (col = ncols-3; col > 1; --col)
      {
       if(drain[row*ncols+col] == 0 && ridge[row*ncols+col] == 0)  
        {
        if(drain[(row+1)*ncols+col] != 0 && ridge[(row+1)*ncols+col] == 0) 
         drain[row*ncols+col] = drain[(row+1)*ncols+col]; 
        if(drain[row*ncols+(col+1)] != 0 && ridge[row*ncols+(col+1)] == 0) 
         drain[row*ncols+col] = drain[row*ncols+(col+1)]; 
        }
       }
    }
          printf ("reverse sweep complete\n");    
 }

/* write out partitioned watershed map */
for (row = 0; row<nrows; row++)
 G_put_map_row (partfd, drain+(row*ncols), row);
      
    printf ("creating support files for %s\n", part_name);
    G_close_cell (partfd);

    exit(0);

}
