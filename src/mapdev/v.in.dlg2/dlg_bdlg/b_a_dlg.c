/*  @(#)b_a_dlg.c	2.1  6/26/87  */

#include <stdio.h>
#include "format.h"

#define		COOR_MAX	5000

/*
 * b_d_dlg  reads the body of a dlg file in binary format
 *    and writes it out in "optional" ascii format.   The binary form
 *    is exactly equivalent to the "optional" format.
 */

b_d_dlg(bin, dlg)
	FILE *bin ;
	FILE *dlg ;
{
	double x ;
	double y ;
	double N ;
	double S ;
	double E ;
	double W ;
	int n_lines ;
	int n_atts ;
	int n_isles ;
	int start_node ;
	int end_node ;
	int left_area ;
	int right_area ;
	int n_coors ;
	int num, old_num ;
	int n_read ;

	int num_nodes ;
	int num_areas ;
	int num_lines ;

	int line_buf[COOR_MAX] ;
	int att_buff[COOR_MAX] ;
	double coor_buff[COOR_MAX * 2] ;
	char type, old_type ;

	extern	int	new_format ;

	num_nodes = 0 ;
	num_areas = 0 ;
	num_lines = 0 ;

	old_num = 0 ;  old_type = ' ' ;

	while( fread(&type, sizeof(type), 1, bin) )
	{
		switch(type)
		{
		case 'N':
			num_nodes++ ;
			fread(&num,    sizeof(num),    1, bin) ;
			fread(&x,      sizeof(x),      1, bin) ;
			fread(&y,      sizeof(y),      1, bin) ;
			fread(&n_lines,sizeof(n_lines),1, bin) ;
			fread(&n_atts, sizeof(n_atts), 1, bin) ;

			if (n_lines)
				fread (line_buf, sizeof(*line_buf), n_lines, bin) ;
			if (n_atts)
				fread (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;

			fprintf(dlg, "%1c%5d%12.2lf%12.2lf%6d%6d%6d%6d%6d%6d\n",
				'N', num, x, y, 0, n_lines, 0, n_atts, 0, 0) ;

			if (n_lines)
				write_int(dlg, n_lines, line_buf) ;
			if (n_atts)
				write_int(dlg, n_atts * 2, att_buff) ;

			break ;

		case 'A':
			num_areas++ ;
			fread(&num,    sizeof(num),    1, bin) ;
			fread(&x,      sizeof(x),      1, bin) ;
			fread(&y,      sizeof(y),      1, bin) ;
			fread(&n_lines,sizeof(n_lines),1, bin) ;
			fread(&n_atts, sizeof(n_atts), 1, bin) ;
			fread(&n_isles,sizeof(n_isles),1, bin) ;

			if (n_lines)
				fread (line_buf, sizeof(*line_buf), n_lines, bin) ;
			if (n_atts)
				fread (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;

			fprintf(dlg, "%1c%5d%12.2lf%12.2lf%6d%6d%6d%6d%6d%6d\n",
				'A', num, x, y, 0, n_lines, 0, n_atts, 0, n_isles) ;

			if (n_lines)
				write_int(dlg, n_lines, line_buf) ;
			if (n_atts)
				write_int(dlg, n_atts * 2, att_buff) ;

			break ;

		case 'L':
			num_lines++ ;
			fread (&num,       sizeof(num),       1, bin) ;
			fread (&start_node,sizeof(start_node),1, bin) ;
			fread (&end_node,  sizeof(end_node),  1, bin) ;
			fread (&left_area, sizeof(left_area), 1, bin) ;
			fread (&right_area,sizeof(right_area),1, bin) ;
			fread (&n_coors,   sizeof(n_coors),   1, bin) ;
			fread (&n_atts,    sizeof(n_atts),    1, bin) ;
			if (new_format)
			 {
				fread (&N,         sizeof(N),         1, bin) ;
				fread (&S,         sizeof(S),         1, bin) ;
				fread (&E,         sizeof(E),         1, bin) ;
				fread (&W,         sizeof(W),         1, bin) ;
			 }

			if (n_coors)
			{
				if (n_coors > COOR_MAX)
				{
					printf("Request for %d coors\n", n_coors) ;
					exit(-1) ;
				}
				fread (coor_buff, sizeof(*coor_buff), n_coors * 2, bin) ;
			}

			if (n_atts)
			{
				fread (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;
			}

			fprintf(dlg, "%1c%5d%6d%6d%6d%6d            %6d%6d\n",
				'L', num, start_node, end_node,
				left_area, right_area, n_coors, n_atts ) ;

			if (n_coors)
				write_double(dlg, n_coors * 2, coor_buff) ;
			if (n_atts)
				write_int(dlg, n_atts * 2, att_buff) ;
			break ;

		default:
			printf("Out of sync:  old_type = %c, old_num = %d\n",
				old_type, old_num) ;
			break ;
		}

		old_type = type ;	old_num = num ;
	}

	printf("\n    nodes: %d\n", num_nodes) ;
	printf("    areas: %d\n", num_areas) ;
	printf("    lines: %d\n", num_lines) ;

}
