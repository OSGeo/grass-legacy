/*  @(#)write_areas.c	1.2  6/24/87  */
#include "structures.h"
#include <stdio.h>

write_areas(f_dlg)
	FILE *f_dlg ;
{
	int i ;
	int n_atts ;
	int maj_att ;
	int isle ;
	int zero ;
	int num_line_ents ;

	maj_att = 999 ;
	zero = 0 ;

	for(i=3; i<=n_areas; i++)
	{
		if (areas[i].category > 0)
			n_atts = 1 ;
		else
			n_atts = 0 ;

	/* Calculate total # of line entries */
		num_line_ents = areas[i].num_lines ;
		if (areas[i].n_islands)
		{
			for (isle=0; isle<areas[i].n_islands; isle++)
					num_line_ents += 1 + islands[areas[i].island_list[isle]].num_lines ;
		}

		/* Write entry out to DLG file */
		fwrite("A",        sizeof(char),      1, f_dlg) ;
		fwrite(&i,   sizeof(int),    1, f_dlg) ;
		fwrite(&areas[i].cent_x,    sizeof(double), 1, f_dlg) ;
		fwrite(&areas[i].cent_y,    sizeof(double), 1, f_dlg) ;
		fwrite(&num_line_ents,      sizeof(int),    1, f_dlg) ;
		fwrite(&n_atts,             sizeof(int),    1, f_dlg) ;
		fwrite(&areas[i].n_islands, sizeof(int),    1, f_dlg) ;
		fwrite(areas[i].line_list, sizeof(int), areas[i].num_lines, f_dlg) ;
		if (areas[i].n_islands)
		{
			for (isle=0; isle<areas[i].n_islands; isle++)
			{
				fwrite(&zero,      sizeof(int),    1, f_dlg) ;
				fwrite(islands[areas[i].island_list[isle]].line_list,
						sizeof(int),
						islands[areas[i].island_list[isle]].num_lines,
						f_dlg) ;
			}
		}
		if (areas[i].category > 0)
		{
			fwrite(&maj_att,           sizeof(int),    1, f_dlg) ;
			fwrite(&areas[i].category, sizeof(int),    1, f_dlg) ;
		}
	}
}
