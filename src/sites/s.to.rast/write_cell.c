#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

void check_overwrite(long *, long);

int write_temp (int fd, struct Cell_head *window,
    double east, double north, int quad_size, void *rast,
    RASTER_MAP_TYPE map_type)
{
    long offset;
    int row;
    int col;
    int startrow, endrow;
    int startcol, endcol;
    long i, num_cells, *cells_written;

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

    /* Add check for overwriting cells */
    num_cells = 0;
    num_cells = (endrow - startrow + 1) * (endcol - startcol + 1);
    if (NULL == (cells_written = (long *) G_malloc(sizeof(long) * (num_cells + 1)))) {
        G_fatal_error("Out of memory");
    }
    i = 0;
    
    for (row = startrow; row <= endrow; row++)
	for (col = startcol; col <= endcol; col++)
        {
            offset = (row * window->cols + col) * G_raster_size(map_type);
            lseek(fd,offset,0);
            /* For overwrite check */
            /* debugging */
            assert(i < num_cells);
            cells_written[i++] = offset;
            if(write(fd, rast, G_raster_size(map_type))!=
                                             G_raster_size(map_type))
            G_fatal_error("error while writing to temp file (disk full?)");
        }

    assert(i == num_cells);
    check_overwrite(cells_written, num_cells);
    G_free(cells_written);
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



void check_overwrite(long *cells, long count)
{
    static long *all_cells = NULL, all_size = 0;
    static int  said_it = 0;
    long i, j, *lptr;
    
    if (!said_it) {
        if (!all_cells) {
            all_cells = (long *) G_malloc(sizeof(long) * count);
            assert(all_cells != NULL);
        }
        else {
            lptr = (long *) G_realloc(all_cells, sizeof(long) * 
                            (all_size + count));
            assert(lptr != NULL);
            all_cells = lptr;
        }
        /* Check the new values against our list */
        for (i = 0; i < all_size; i++) {
            for (j = 0; j < count; j++) {
                if (cells[j] == all_cells[i] && !said_it) { /* Do warning */
                    G_warning("Writing same cells more than once!\n"
                            "Consider reducing your cell resolution or"
                            " reducing the 'size' parameter.");
                    said_it = 1;
                    goto break_out;
                }
            }
        }
break_out:
        /* If we didn't find any, let's add to the list */
        if (!said_it) {
            all_size += count;
            for (i = all_size - count, j = 0; 
                            i < all_size && j < count; i++, j++) {
                all_cells[i] = cells[j];
            }
        }
        else { /* Free memory and set all_cells == NULL */
            G_free(all_cells);
            all_cells = NULL;
        }
    } /* if (!said_it) */
} /* check_overwrite() */
/* vim: softtabstop=4 shiftwidth=4 expandtab */
