#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "global.h"

/********************************************
  file write.c - Bugfix by Luca Palmeri
                 lpalmeri@ux1.unipd.it
                           13 Aug 1999  
*********************************************/
/* Corrected   write_map(char *name) to correctly use
   the target window settings, needed by G_put_raster_row(...)  
   Pierre de Mouveaux (pmx@audiovu.com) 05 april 2000       
 */

int write_matrix (int row, int col)
{
    int n;
    long offset;

    select_target_env();
    if(!temp_fd) 
    {
        temp_name = G_tempfile();
        temp_fd = creat(temp_name,0660);
		if (temp_fd < 0)
				perror("temp_fd");
    }
    for (n=0; n < matrix_rows; n++)
    {
        offset = (row++ * target_window.cols + col) * G_raster_size(map_type);
        if(lseek(temp_fd,offset,0) == (off_t)-1 ) {
		G_warning("write_matrix: %s\n",strerror(errno));
		return -1;
	}

        if(write(temp_fd,cell_buf[n],G_raster_size(map_type)*matrix_cols) 
                                != G_raster_size(map_type)*matrix_cols)
        {
            close(temp_fd);
           unlink(temp_name);
           G_fatal_error("error while writing to temp file: %s\n",strerror(errno));
        }
        /*G_put_map_row_random (outfd, cell_buf[n], row++, col, matrix_cols);*/
    }
    select_current_env();

    return 0;
}

void show_window(struct Cell_head* w) {
		fprintf(stderr,"-----------------------------------------------\n");
		fprintf(stderr,"format: %d,cpmr: %d,rows: %d,cols: %d,proj: %d,zone %d\n",
			   w->format,w->compressed,w->rows,w->cols,
			   w->proj,w->zone);
		fprintf(stderr,"ew %.2f,ns %.2f, N:%.2f, S:%.2f,E:%.2f,W:%.2f\n",
			   w->ew_res, w->ns_res,w->north,w->south, w->east, w->west);
} 


int write_map(char *name)
{
   int fd, row;
   char buf[100];
   void *rast;
   struct Cell_head win;


   select_target_env();
   if (G_set_window(&target_window)<0)
		   fprintf(stderr,"Error setting window.\n"); 

   fprintf(stderr,"write_map(%s)\n",name);
   rast = G_allocate_raster_buf(map_type);
   
   fd = G_open_raster_new(name,map_type);

   close(temp_fd);
   if((temp_fd = open(temp_name,0)) <= 0)
        G_fatal_error("tempfile '%s' does not exist\n",temp_name);
 
   if(fd <=0)
   {
       sprintf(buf, "Can't open map %s\n", name);
       G_fatal_error(buf);
   }

   for(row = 0; row < target_window.rows; row++)
     {
         /* BUGFIX: Removed semi-colon at the end of line */
         if(read(temp_fd,rast,target_window.cols * G_raster_size(map_type))
                      != target_window.cols * G_raster_size(map_type)) 
         {
            close(temp_fd);
            unlink(temp_name);
            G_fatal_error("error reading row %d\n", row);
         }
         if(G_put_raster_row(fd,rast, map_type) < 0)
         {
            close(temp_fd);
            unlink(temp_name);
            G_fatal_error("error while writing to raster file");
         }
     }
   /*BUGFIX: next three lines moved after the } */
   close(temp_fd);
   unlink(temp_name);
   G_close_cell(fd);
   return 0;
}
