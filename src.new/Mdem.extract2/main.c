/* %W% %G% */
#define MAIN
#include "usgs.h"


main(argc,argv) char *argv[];
{
   int new,nrow;
   char newname[20];

   G_gisinit("DEM Tape Extraction Program");

/* check command line for accuracy */

   if (!getargs(argc,argv))
   {
       fprintf (stderr, "usage: [if=tapedev] of=cellfile bs=blocksize\n");
       exit(1);
   }
   
/* initialize variables and buffers */
    
    usgs_init();
/*  loop thru entire tape looking for relevant data */
/*  if any data found, call getgrid to extract it   */ 

    printf("Reading Elevation Tape...     \n");
    while(get_hdr())
    {
        hdr_list(stdout);
        if(!((n < (double)(file_south + y_res/2.))||
        (s > (double)(file_north - y_res/2.))||
        (w > (double)(file_east - x_res/2.))||
        (e < (double)(file_west + x_res/2.))))
        {
            if(!getgrid())
            {
                G_fatal_error("could not find data in correct file");
                exit(1);
            }
        }
        else
        {
            skip_file();
        }
    }
    close(fd);
    close(tapefile);

/* make system call to Mrot90 to rotate cell file 90 deg */

printf("making system call to Mrot90\n\n");

   sprintf(command,"Mrot90 if=%s of=%s bpc=%d rows=%d cols=%d",
   inf,of,sizeof(CELL),cellhd.cols,cellhd.rows);
    printf("%s\n",command);
   if(system(command))
   {
       G_fatal_error("can't rotate cell file");
       exit(1);
   }
   unlink(inf);

printf("copying rotated file to cell\n");
/* open new cell file */
   
   if((new = G_open_cell_new(outname)) < 0)
   {
      G_fatal_error("can't create new cell file ");
      exit(1);
   }

/* copy rotated file into rcell file */

   fd = open(of,0);
   for(nrow = 0; nrow < cellhd.rows; nrow++)
   {
       read(fd,profile_buf,cellhd.cols * sizeof(CELL));
       if(G_put_map_row(new,profile_buf,nrow) < 0)
       {
          G_fatal_error("error while writing to cell file");
              exit(1);
       }
   }

   close(fd);
   unlink(of);
   printf ("CREATING SUPPORT FILES FOR %s\n", outname);
   G_close_cell(new);
   exit(0);
}
