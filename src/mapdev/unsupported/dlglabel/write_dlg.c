/*  @(#)write_dlg.c	2.1  6/26/87  */
#include "dlg.h"
#include "externs.h"
#include <signal.h>

write_dlg()
{
	int stat ;
	int num ;
	int newper, oldper ;
	int n_records, n_read ;
	int record ;
	int coor ;
	char buffer[90] ;

	printf("\n\nWriting out labeled DLG file.\n") ;
	printf("  Hit DEL/RUB to abort\n") ;
	set_signals() ;

/* Write out dlg header */
	rewind(dlg_new) ;
	printf("  1. Writing header\n") ;
	write_dlg_header() ;

/* Write out node info */
	printf("\n  2. Writing nodes to # %5d: ", tot_nodes) ;
	printf("%3d%% ", oldper=0) ;
	for (num=1; num<=tot_nodes; num++)
	{
		if (signalflag.interrupt)
			return(-1) ;
		if ( (node[num].x == 0.0) || (node[num].y == 0.0) )
			continue ;
		if (oldper != (newper = num * 100 / tot_nodes))
		{
			printf("%3d%%", newper) ;
			oldper = newper ;
		}
		fwrite("N",               sizeof(char),             1, dlg_new) ;
		fwrite(&num,              sizeof(num),              1, dlg_new) ;
		fwrite(&node[num].x,      sizeof(node[num].x),      1, dlg_new) ;
		fwrite(&node[num].y,      sizeof(node[num].y),      1, dlg_new) ;
		fwrite(&node[num].n_lines,sizeof(node[num].n_lines),1, dlg_new) ;
		fwrite(&node[num].n_atts, sizeof(node[num].n_atts), 1, dlg_new) ;

		if (node[num].n_lines)
			fwrite (node[num].lines, sizeof(*node[num].lines), node[num].n_lines, dlg_new) ;
		if (node[num].n_atts)
			fwrite (node[num].atts, sizeof(*node[num].atts), node[num].n_atts * 2, dlg_new) ;
	}

/* Write out area information */
	printf("\n\n  3. Writing areas to # %5d: ", tot_areas) ;
	printf("%3d%% ", oldper=0) ;
	for (num=1; num<=tot_areas; num++)
	{
		if (signalflag.interrupt)
			return(-1) ;
		if ( (area[num].x == 0.0) || (area[num].y == 0.0) )
			continue ;
		if (oldper != (newper = num * 100 / tot_areas))
		{
			printf("%3d%%", newper) ;
			oldper = newper ;
		}

		fwrite("A",               sizeof(char),             1, dlg_new) ;
		fwrite(&num,              sizeof(num),              1, dlg_new) ;
		fwrite(&area[num].x,      sizeof(area[num].x),      1, dlg_new) ;
		fwrite(&area[num].y,      sizeof(area[num].y),      1, dlg_new) ;
		fwrite(&area[num].n_lines,sizeof(area[num].n_lines),1, dlg_new) ;
		fwrite(&area[num].n_atts, sizeof(area[num].n_atts), 1, dlg_new) ;
		fwrite(&area[num].n_isles,sizeof(area[num].n_isles),1, dlg_new) ;

		if (area[num].n_lines)
			fwrite (area[num].lines, sizeof(*area[num].lines), area[num].n_lines, dlg_new) ;
		if (area[num].n_atts)
			fwrite (area[num].atts, sizeof(*area[num].atts), area[num].n_atts * 2, dlg_new) ;
	}

/* Write out line information */
	printf("\n\n  4. Writing lines to # %5d: ", tot_lines) ;
	printf("%3d%% ", oldper=0) ;
	for (num=1; num<=tot_lines; num++)
	{
		if (signalflag.interrupt)
			return(-1) ;
		if ( (line[num].start_node == 0) || (line[num].end_node == 0) )
			continue ;
		if (oldper != (newper = num * 100 / tot_lines))
		{
			printf("%3d%%", newper) ;
			oldper = newper ;
		}

		fwrite("L",                   sizeof(char),                1, dlg_new) ;
		fwrite (&num,                 sizeof(num),                 1, dlg_new) ;
		fwrite (&line[num].start_node,sizeof(line[num].start_node),1, dlg_new) ;
		fwrite (&line[num].end_node,  sizeof(line[num].end_node),  1, dlg_new) ;
		fwrite (&line[num].left_area, sizeof(line[num].left_area), 1, dlg_new) ;
		fwrite (&line[num].right_area,sizeof(line[num].right_area),1, dlg_new) ;
		fwrite (&line[num].n_coors,   sizeof(line[num].n_coors),   1, dlg_new) ;
		fwrite (&line[num].n_atts,    sizeof(line[num].n_atts),    1, dlg_new) ;
		fwrite (&line[num].N,         sizeof(line[num].N),         1, dlg_new) ;
		fwrite (&line[num].S,         sizeof(line[num].S),         1, dlg_new) ;
		fwrite (&line[num].E,         sizeof(line[num].E),         1, dlg_new) ;
		fwrite (&line[num].W,         sizeof(line[num].W),         1, dlg_new) ;

		if (line[num].n_coors)
		{
			if (read_coors(num) < 0)
				printf("Error: Read for line %d\n", num) ;

			fwrite (coors, sizeof(*coors), line[num].n_coors * 2, dlg_new) ;
		}
		if (line[num].n_atts)
			fwrite (line[num].atts, sizeof(*line[num].atts), line[num].n_atts * 2, dlg_new) ;
	}

	printf("\n\n    Hit RETURN to continue\n") ;
	gets(buffer) ;
}
