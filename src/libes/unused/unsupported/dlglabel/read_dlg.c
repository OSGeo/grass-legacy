/*  @(#)read_dlg.c	2.1  6/26/87  */
#include "dlg.h"

/*
 * read_dlg   reads the body of a dlg file in "optional" binary format.
 *
 *   returns:   0  on a successful read
 *             -1  on error
 */

read_dlg()
{
	long ftell() ;
	int num ;
	int undef ;
	char type ;

	orig_nodes = 0 ;
	orig_areas = 0 ;
	orig_lines = 0 ;
	undef = 0 ;

	while( fread(&type, sizeof(type), 1, dlg) )
	{
		switch(type)
		{
		case 'N':
			fread(&num,    sizeof(num),    1, dlg) ;
			alloc_nodes(num) ;
			if (orig_nodes < num)
				orig_nodes = num ;
			fread(&node[num].x,      sizeof(node[num].x),      1, dlg) ;
			fread(&node[num].y,      sizeof(node[num].y),      1, dlg) ;
			fread(&node[num].n_lines,sizeof(node[num].n_lines),1, dlg) ;
			fread(&node[num].n_atts, sizeof(node[num].n_atts), 1, dlg) ;

			if (node[num].n_lines)
				read_int(dlg, node[num].n_lines, &(node[num].lines)) ;

			if (node[num].n_atts)
				read_int(dlg, 2 * node[num].n_atts, &(node[num].atts)) ;

			break ;

		case 'A':
			fread(&num,    sizeof(num),    1, dlg) ;
			alloc_areas(num) ;
			if (orig_areas < num)
				orig_areas = num ;

			fread(&area[num].x,      sizeof(area[num].x),      1, dlg) ;
			fread(&area[num].y,      sizeof(area[num].y),      1, dlg) ;
			fread(&area[num].n_lines,sizeof(area[num].n_lines),1, dlg) ;
			fread(&area[num].n_atts, sizeof(area[num].n_atts), 1, dlg) ;
			fread(&area[num].n_isles,sizeof(area[num].n_isles),1, dlg) ;

			if (area[num].n_lines)
				read_int(dlg, area[num].n_lines, &(area[num].lines)) ;

			if (area[num].n_atts)
				read_int(dlg, 2 * area[num].n_atts, &(area[num].atts)) ;

			break ;

		case 'L':
			fread (&num, sizeof(num), 1, dlg) ;
			alloc_lines(num) ;
			if (orig_lines < num)
				orig_lines = num ;
			fread (&line[num].start_node,sizeof(line[num].start_node),1, dlg) ;
			fread (&line[num].end_node,  sizeof(line[num].end_node),  1, dlg) ;
			fread (&line[num].left_area, sizeof(line[num].left_area), 1, dlg) ;
			fread (&line[num].right_area,sizeof(line[num].right_area),1, dlg) ;
			fread (&line[num].n_coors,   sizeof(line[num].n_coors),   1, dlg) ;
			fread (&line[num].n_atts,    sizeof(line[num].n_atts),    1, dlg) ;
			fread (&line[num].N,         sizeof(line[num].N),         1, dlg) ;
			fread (&line[num].S,         sizeof(line[num].S),         1, dlg) ;
			fread (&line[num].E,         sizeof(line[num].E),         1, dlg) ;
			fread (&line[num].W,         sizeof(line[num].W),         1, dlg) ;

			line[num].file = dlg ;
			line[num].offset = ftell(dlg) ;

			fseek(dlg, (long)(line[num].n_coors * 2 * sizeof(double)), 1) ;

			if (line[num].n_coors < 2)   /* A line must have two endpoints */
				line[num].n_coors = 2 ;

			if (line[num].n_atts)
				read_int(dlg, line[num].n_atts * 2, &(line[num].atts)) ;

			break ;

		default:
			return(-1) ;
		}
	}
	return(0) ;
}
