/*
****************************************************************************
*
* MODULE:       d.rast.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a raster map in a map projection.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*******************************************************************************
Pre-Processed Disk Access 
structures & associated functions for d.rast.PROJ

Sharif Razzaque June 1995
*******************************************************************************
*/

#include <stdlib.h>
#include <string.h>	/* For memset */

#include "config.h"	/* For Grass 5.0 Bev Wallace */
#include "gis.h"
#include "display.h"	/* For D_* - Bev Wallace */
#include "raster.h"	/* For R_* - Bev Wallace */

#include "coord_systems.viewproj.h"

/* 
	FILE REQUEST STRUCTURE GLOBALS
*/

typedef struct temp_tag
{
        int col; /* the column in the rasterfile requested */
        screen_coord_type scrn_loc; /* where on the display the data goes*/
        struct temp_tag *next_ptr; /* pointer to next request*/
} col_request_type;

col_request_type **PPDA_filerows; /* dynamic array of pointers to start of
                                         linked list of column requests */

int PPDA_num_of_rows;

int PPDA_screen_buffer_x_size;
int PPDA_screen_buffer_y_size;
int PPDA_screen_buffer_x_disp;
int PPDA_screen_buffer_y_disp;


void * PPDA_init (int num_of_rows, int t,int b,int r, int l)
{
        int index;

        PPDA_num_of_rows=num_of_rows;

        /* allocate row array*/
        PPDA_filerows=
          (col_request_type **)G_malloc(
           sizeof(col_request_type*) * (num_of_rows+4) );

        /* initialize array to NULL pointers */
        for(index=0;index<(num_of_rows+4);index++)
        {
                PPDA_filerows[index]=NULL;
        }
        
        /* set-up screen buffer size globals , but don't allocate yet!*/
        PPDA_screen_buffer_x_size=r-l+1;
        PPDA_screen_buffer_y_size=b-t+1;
        PPDA_screen_buffer_x_disp=l;
        PPDA_screen_buffer_y_disp=t;

        return(PPDA_filerows);
}


void PPDA_request (map_coord_type map, screen_coord_type screen)
{
        col_request_type *col_request_ptr;

        /* check if request is valid */
        if (map.row>PPDA_num_of_rows)
        {
                fprintf(stderr, "disk request out of bounds - ignoring\n");
                return;
        }
        
        /* allocate new col_request */
        col_request_ptr=(col_request_type *)G_malloc(sizeof(col_request_type));
        
        /* make it point to old front of list */
        (*col_request_ptr).next_ptr=PPDA_filerows[map.row+3];

        /* stick it in front of list*/
        PPDA_filerows[map.row+3]=col_request_ptr;

        /* store the column number & the screen info */
        (*col_request_ptr).col=map.col;
        (*col_request_ptr).scrn_loc=screen;

	return;
}


void PPDA_process_row_file(int row_index, CELL *disk_row_buffer,
                         struct Colors *colors, short *disp_buffer) 
{
        col_request_type *col_request_ptr;
        col_request_type *old_col_request_ptr;

        int pixel_data;
        
        col_request_ptr=PPDA_filerows[row_index+3];
        while(col_request_ptr!=NULL) /* travel list */
        {
                pixel_data=disk_row_buffer[col_request_ptr->col];
                disp_buffer[ col_request_ptr->scrn_loc.x+
                                (col_request_ptr->scrn_loc.y)*
                                PPDA_screen_buffer_x_size]
                        = pixel_data;
     
                /* free this request & get the next */
                old_col_request_ptr=col_request_ptr;
                col_request_ptr=(*col_request_ptr).next_ptr;
                free(old_col_request_ptr);
        }

	return;
}


static void
draw_screen_row (short *start_ptr,  struct Colors *colors,int overlay)
{
        short *end_ptr=start_ptr+sizeof(short)*(PPDA_screen_buffer_x_size-1);
        short *x_ptr;
        short current_cat;
        int delta_x;

        x_ptr=start_ptr;
        while(x_ptr<end_ptr)
        {
                current_cat=*x_ptr;
                delta_x=0;
                        while( (current_cat==*x_ptr) && (x_ptr<end_ptr))
                        {
                                x_ptr++;
                                delta_x++;
                        }
                        if ((current_cat>-1) && (!overlay || current_cat) )
                        {
                                D_color(current_cat,colors);
                                R_cont_rel(delta_x,0);
                        }
                        else
                                R_move_rel(delta_x,0);
        }

	return;
}       

              
void PPDA_get_and_plot_data (int cellfile, struct Colors *colors,int overlay)
{
        int row_index;
        CELL *disk_row_buffer;
        short *disp_buffer;
        short *pixel_ptr;
        
        int scrn_y;

        /* allocate array to act as a buffer for display window */
        disp_buffer=(short *)G_malloc( sizeof(short)
           *PPDA_screen_buffer_x_size *PPDA_screen_buffer_y_size);
        /* clear out buffer to -1*/
        memset(disp_buffer,-1,( sizeof(short)
           *PPDA_screen_buffer_x_size *PPDA_screen_buffer_y_size) );

        disk_row_buffer=G_allocate_cell_buf();

        /* loop through all rows in array*/
        for (row_index=0;row_index<PPDA_num_of_rows;row_index++)
        {
                if (PPDA_filerows[row_index+3]!=NULL)
                {
                        /* get the row from disk */
                        G_get_c_raster_row(cellfile,disk_row_buffer,row_index);
                        
                        /* process the row*/
                        PPDA_process_row_file(row_index,disk_row_buffer
                                                ,colors,disp_buffer);
                 }
		/* Print out our progress */
		G_percent (row_index + 1, PPDA_num_of_rows, 1);
       }

        /* now display contents of display buffer onto screen */
        pixel_ptr=disp_buffer;
        for (scrn_y=0;scrn_y<PPDA_screen_buffer_y_size;scrn_y++)
        {
                R_move_abs(PPDA_screen_buffer_x_disp,
                     scrn_y+PPDA_screen_buffer_y_disp);

                draw_screen_row(pixel_ptr,colors,overlay);
                pixel_ptr=pixel_ptr+PPDA_screen_buffer_x_size;
        }

        free(disp_buffer);
        
        /* done with row array so free it*/
        free(PPDA_filerows);

	return;
}
