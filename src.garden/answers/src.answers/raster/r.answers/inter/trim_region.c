/* %W% %G% */
/* 
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. Permission to use, copy,    |
    |   modify, and distribute this software and its          |
    |   documentation for any purpose and without fee is      |
    |   hereby granted, provided that the above copyright     |
    |   notice appear in all copies.  This software is        |
    |   provided "as is" without express or implied warranty. |
    +---------------------------------------------------------+

   function: trim_region
   called by: step_1
   
   In the interest of running a frugal project, this program sets out
   to define a suggested project region that is no larger than necessary.
   Here's the scheme:
   read the cell head of the raster layer given as the project mask layer,
   and start with that as a base. then read the cell values in the mask.
   all non-zero values are considered "in the mask". while reading the values, 
   keep track of where the extreme n, s, e, and w cells are. then calculate
   the region n, s, e, and w limits, giving a 2 cell border (unless the
   original border created by the cellhd was less) around the "mask" area.
   
   this way, all the subsequent project work is using the smallest possible 
   region.
   
   but the user gets to approve the region settings when the edit_region 
   function is called...
*/

#include "answers.h"
trim_region()
{
    int fd;
    int row,
        col;
    int north_row,
        south_row,
        east_col,
        west_col;
double north,
       south,
       east,
       west;
    CELL *cell;
    struct Cell_head *tmp_window;
    char *G_align_window();

/* if complete[1] is 0 or 2, we need to re-define the region since
   the mask or resolution must be new or changed. Otherwise, if 1,
   then the user must be running this for a second time, without changing
   mask or resolution, perhaps to change the project region specifically */
   
    if (complete[1] == 1) 
        {
        G_clear_screen();
        printf("\n      ANSWERS on GRASS REGION Utility \n\n");
        printf("You have already defined a region for this project.\n");
        printf("If current project region is changed, all previously\n");
        printf("completed steps must be run again to resample ANSWERS\n");
        printf("inputs.\n\n");
        if(!G_yes("Do you wish to re-define the region at this time?",0)) 
            return(0);
        else
            alter_status();
        }
    
    G_clear_screen();


    printf("\n      ANSWERS on GRASS REGION Utility \n\n");
    printf("The purpose of this utility is to delineate the geographical\n");
    printf("region of the watershed. Once set, all ANSWERS on GRASS programs\n");
    printf("will automatically use the region for subsequent operations.\n");
    printf("Also, each time you work on this project, the MASK and REGION\n");
    printf("will be automatically set.\n");
    printf("\n");
    printf("This utility scans <%s> looking for all cells with\n", mask_layer);
    printf("non-zero values, while at the same time keeping track of the\n");
    printf("north-, east-, west-, and south-most cells. Thus an optimal\n");
    printf("region can be determined for the project.\n");
    printf("\n");
    printf("The next screen will display the region that has been derived\n");
    printf("(and allow you to modify the region coordinates, if desired).\n");
    printf("\n");
    printf("Note: this utility ignors and overrides any currently set mask\n");
    printf("or region.\n");
    printf("\n");
    printf("Scanning...\n");

/* set the region to the MASK layer's cell head listing */
 
    if (G_get_cellhd(mask_layer, mask_mapset, &window) < 0)
    {
	fprintf(stderr, "\n\7WARNING: Could not get cell head info for <%s>\n", 
	mask_layer);
	fprintf(stderr, "therefore, REGION could not be set.");
	complete[1] = 0;
	hit_return();
	return (1);
    }
    
/* copy current window to a temp, so that we can play our
   mumbo with the resolution. if proj_resolution is different
   from current, and we don't align, G_set_window will change
   resolution to fit window coords. we don't want that */

    tmp_window = &window;

/* this allows us to insist that the resolution select by the user
   is used... thus they can set the project resolution */

    tmp_window->ew_res = proj_resolution;
    tmp_window->ns_res = proj_resolution;

/* if need be, window dimensions will be modified so that
   resolutions, UTM coords, and rows and cols work out evenly,
   without changing resolutions. thus, UTM coords may be rounded
   (larger) to make this so  */

    G_align_window(&window, tmp_window);

    G_set_window(&window);

    fd = G_open_cell_old(mask_layer, mask_mapset);
    if (fd < 0)
    {
	fprintf(stderr, "\n\7WARNING: Could not open cell file <%s>", mask_layer);
	return (1);
    }
    cell = G_allocate_cell_buf();

/* initialize some variables */

    north_row = window.rows;
    west_col = window.cols;
    south_row = east_col = 0;
    cells_in_wshd =  0;
    
    fprintf(stderr, "\npercent complete: ");
    for (row = 0; row < window.rows; row++)
    {
	if (G_get_map_row_nomask(fd, cell, row) < 0)
	{
	    fprintf(stderr, "\n\7WARNING: Failure reading map row %d of <%s>\n",
	    row, mask_layer);
	    hit_return();
	    return (1);
	}
	percent(row, window.rows, 5);

/* scan thru cell values. if not 0 then consider this part of
   the mask. keep track of where the extreme edge cells are, so
   we know how big to make the region */

	for (col = 0; col < window.cols; col++)
	{
	    if (cell[col] > 0)
	    {
		cells_in_wshd++;
		if (row < north_row)
		    north_row = row;
		if (row > south_row)
		    south_row = row;
		if (col > east_col)
		    east_col = col;
		if (col < west_col)
		    west_col = col;
	    }
	}
    }
/*
    cells_in_wshd--;
*/
    G_close_cell(fd);

/* gather some possibly useless information about the watershed */

    rows_in_wshd = south_row - north_row;
    cols_in_wshd = east_col - west_col;

/* set it so that we get at least a cell border around the watershed's
   perimeter */

    north_row = north_row - 1;
    south_row = south_row + 2;
    east_col = east_col + 2;
    west_col = west_col - 1;
    
/* calc UTM coords for new region */

    north = window.north - ((north_row) * window.ns_res);
    south = window.north - ((south_row) * window.ns_res);
    east = window.west + ((east_col) * window.ew_res);
    west = window.west + ((west_col) * window.ew_res);

/* fill in the rest of the cellhd structure for our new region */

    window.cols = east_col - west_col;
    window.rows = north_row - south_row;
    window.north = north;
    window.south = south;
    window.east = east;
    window.west = west;

/* make it the current window */

    G_set_window(&window);
    printf("done   \n\n");
    hit_return();

/* call edit_region. if it is passing back a flag (from user backing out
   of an operation) we'll pass it back as well */

    return (0);
}
