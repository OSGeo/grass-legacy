/*****************************************************************************/
/***                                                                       ***/
/***                             write_raster()                            ***/
/***   	            Writes out and closes  GRASS raster file.  		   ***/
/***                 Jo Wood, Project ASSIST, 29th May 1993.               ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"

write_raster()
{
    /* ------ Initialise ------ */

    int	   rast_row,rast_col;		/* Counts through each raster row.   */

    CELL  *row_buf;
    row_buf  = G_allocate_cell_buf();   /* Allocate enough memory to store   */
                                        /* one raster row.		     */

    /* ------ Copy raster to GRASS buffer and write out ------ */

    for (rast_row=0;rast_row<401; rast_row++)
    {
	row_buf = raster[rast_row];
	G_put_map_row(fd_out, row_buf);	/* Write the row buffer to  	     */
    					/* the output raster.       	     */
    }

    /* ------ Close GRASS raster ------ */

    G_close_cell(fd_out);
    G_init_cats((CELL)0,"Elevation",&cats);
    G_write_cats(file_out_name,&cats);

    /* ------ Update file name so new one may be opened ------ */

    strcat(file_out_name,"%");

    O_raster     = FALSE;		/* Flag to indicate file is now closed	*/
    num_rlines   = 0;

}
