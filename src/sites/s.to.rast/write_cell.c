#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

int write_temp (int fd, struct Cell_head *window,
    double east, double north, int quad_size, void *rast,
    RASTER_MAP_TYPE map_type)
{
    long offset;
    int row;
    int col;
    int startrow, endrow;
    int startcol, endcol;

    row = (int) G_northing_to_row (north, window);
    col = (int) G_easting_to_col (east, window);
    startcol = col - quad_size ;
    endcol   = col + quad_size ;
    if (startcol < 0)
	startcol = 0;
    if (endcol >= window->cols)
        endcol = window->cols - 1;
    
    if (endcol < startcol)
	return 0;

    startrow = row - quad_size ;
    endrow   = row + quad_size ;
    if (startrow < 0)
	startrow = 0;
    if (endrow >= window->rows)
        endrow = window->rows - 1;
    
    if (endrow < startrow)
	return 0;

    for (row = startrow; row <= endrow; row++)
	for (col = startcol; col <= endcol; col++)
        {
            offset = (row * window->cols + col) * G_raster_size(map_type);
            lseek(fd,offset,0);
            if(write(fd, rast, G_raster_size(map_type))!=
			                     G_raster_size(map_type))
		   G_fatal_error("error while writing to temp file (disk full?)");
        }

	return 0;
}

int close_temp (int fd, char *name, char *temp_name,
   int nrows, int ncols, RASTER_MAP_TYPE map_type)
{
   int row, rast_fd;
   void *rast;
    
   if ((rast_fd = G_open_raster_new (name, map_type)) < 0)
   {
       fprintf(stderr, "\ncan't create raster file <%s>\n", name);
       exit(1);
   }
   rast = G_allocate_raster_buf(map_type);

   close(fd);
   fd = open(temp_name,0);
   if(fd >=0)
   {
       for (row = 0; row < nrows; row++)
       {
            if(read(fd, rast, ncols * G_raster_size(map_type))!=
			                     ncols * G_raster_size(map_type))
		   G_fatal_error("error while reading temp file (disk full?)");
            G_put_raster_row(rast_fd, rast, map_type);
       }
       unlink(temp_name);
   }
   else
       G_fatal_error("error while reading temp file");
   G_close_cell(rast_fd);

   return 0;
}
