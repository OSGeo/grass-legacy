/*
****************************************************************************
*
* MODULE:       d.rast.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Tuan Tran, LMMS
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
******************************************************************************
*    Dcell.PROJ  modified version of Dcell, used by D.rast.viewproj      
*
*	Original code for Grass 4.1 by Sharif Razzaque June 1995.
*	Updated for Grass 5.0 by Bev Wallace 9/14/99.
*		Currently works for CELL_TYPE data only.
*
******************************************************************************
*/

#include "config.h"	/* For Grass 5.0 - Bev Wallace */
#include "gis.h"
#include "display.h"	/* For D_* - Bev Wallace */
#include "raster.h"	/* For R_* - Bev Wallace */
#include "projects.h"
#include "coord_systems.viewproj.h"

/* prototypes for D_PPDA.c functions (this is the only place they are used) */
extern void * PPDA_init (int num_of_rows, int t,int b,int r, int l);
extern void PPDA_request (map_coord_type map, screen_coord_type screen);
extern void PPDA_get_and_plot_data (int cellfile, struct Colors *colors,int overlay);
/*****************************************************************************/


static int cell_draw (char *name, char *mapset, struct Colors *colors, 
	int overlay, RASTER_MAP_TYPE data_type)
{
	char buff[128] ;
	int cellfile;
	int t, b, l, r ;
	int ncols;

        int t_in, b_in, l_in, r_in;
        int num_of_rows_in_filedata;
 
        D_color(0,colors); /* for grid drawing in next line */
        num_of_rows_in_filedata = setup_conversions_viewproj ((!overlay),
		&t_in, &b_in, &l_in, &r_in);  

	ncols = G_window_cols();

	/* Set up the screen, conversions, and graphics */
	D_get_screen_window(&t, &b, &l, &r) ;
	if (D_cell_draw_setup(t, b, l, r))
	{
		sprintf(buff,"Cannot use current window") ;
		G_fatal_error(buff) ;
	}
	D_set_overlay_mode(overlay);

	/* Make sure map is available */
	if ((cellfile = G_open_cell_old(name, mapset)) == -1)
	{
		sprintf(buff,"Not able to open cellfile for [%s]", name);
		G_fatal_error(buff) ;
	}

/*
        DRAW THE MAP!
*/
        {
        short x,y;
        screen_coord_type   scrn_pos;
        map_coord_type      mp_pos;
                
        fprintf (stdout,"calculating screen to world mapping...\n");
	fflush (stdout);

        PPDA_init(num_of_rows_in_filedata,t,b,r,l);

	/* Avoid display outside the region
	 * Change l,r,t,b to l_in, r_in, t_in, b_in  - Tuan Tran */
        for (y=t_in;y<=b_in;y=y+1)
        {
                for(x=l_in;x<=r_in;x=x+1)
                {
                        scrn_pos.x=x;
                        scrn_pos.y=y;
                        
                        mp_pos=screen_to_map(scrn_pos);
                        if (mp_pos.row>=0)
                                PPDA_request(mp_pos,scrn_pos);
                
                }
		/* Print out our progress */
		G_percent (y - t_in + 1, b_in - t_in + 1, 1);
        }

        fprintf (stdout, "drawing screen buffer...\n");
	fflush (stdout);

        PPDA_get_and_plot_data(cellfile,colors,overlay);

        fprintf (stdout, "done\n");
	fflush (stdout);
        }

	/* Wrap up and return */
	G_close_cell(cellfile) ;
	free_conversions_viewproj(); /* free all PROJ memory*/
	return(0);
}


int Dcell_viewproj (char *name, char *mapset, int overlay)
{
    struct Colors colors ;
    int offset ;
    char buff[128] ;
    int fp;
    RASTER_MAP_TYPE data_type;

    fp = G_raster_map_is_fp(name, mapset);
    if(fp) {
	data_type = DCELL_TYPE;
	/* Have not yet converted this code to handle DCELL_TYPE data - Bev */
	fprintf(stderr,"Unable to display DCELL_TYPE raster data\n") ;
	exit(1);
    }
    else {
        data_type = CELL_TYPE;
    }

    if (G_read_colors(name, mapset, &colors) == -1) {
	sprintf(buff,"Color file for [%s] not available", name);
        G_fatal_error(buff);
    }

    D_setup(0);

    /* cell maps wipe out a picture, so we clear info on the window too */
    if (!overlay)
	D_clear_window();

    /* Get color offset value for current window and pass to driver */
    D_offset_is(&offset) ;
    R_color_offset(offset) ;

    /* Set the colors for the display */
    D_set_colors (&colors);

    /* Go draw the cell file */
    /* Original d.rast.viewproj differed from d.rast within cell_draw. */
    cell_draw(name, mapset, &colors, overlay, data_type) ;

    /* release the colors now */
    G_free_colors (&colors);

    /* record the cell file */
    D_set_cell_name(G_fully_qualified_name(name, mapset));

    return(0);
}


