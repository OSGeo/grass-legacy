/****************************************************************/
/*								*/
/*	plot_pattern.c	in	~/src/i_range			*/
/*								*/
/*	This function draws the pattern inside the display	*/
/*	window according to the line coordinate information	*/
/*	maintained in arrays of doubles.			*/
/*								*/
/****************************************************************/

#include "dlg.h"

plot_pattern()
{
	int i;
	extern double dlg_U_south,dlg_U_north,
			dlg_U_west,dlg_U_east;
	extern struct dlg dlg;
	extern int *n_coors_info;
	extern double **line_coor_info,*line_bounds_info;
	
	/*	loop over lines					*/
	for (i=1; i<=dlg.max_lines; i++)
        {
	/*  if any part of line falls outside map edges, do	*/
	/*  not draw that line					*/
	
                if (*(line_bounds_info+4*i-4) < dlg_U_south)
                        continue ;
                if (*(line_bounds_info+4*i-3) > dlg_U_north)
                        continue ;
                if (*(line_bounds_info+4*i-1) < dlg_U_west)
                        continue ;
                if (*(line_bounds_info+4*i-2) > dlg_U_east)
                        continue ;

	/*	draw a line passing through the coordinates 	*/
	dlg_plot_all_coors(*(n_coors_info+i-1),
			   *(line_coor_info+i-1));
        }
}

/************* END OF FUNCTION "PLOT_PATTERN" *******************/
