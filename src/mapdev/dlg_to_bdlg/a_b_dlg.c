/*  @(#)a_b_dlg.c	2.1  6/26/87  */
#include <stdio.h>
#define COOR_MAX		5000
#define MAXLINE		90
#define CHAR_NULL	'\0'
#define FGET 		if (fgets(buff,MAXLINE,dlg) == NULL) strcpy(buff,"E")

/*
 * a_b_dlg  reads the body of a dlg file in "optional" ascii format
 *    and writes it bdlg in CERL binary format.   This binary form
 *    is exactly equivalent to the "optional" format.
 *
 *  May 24, 89;
 *  To many cases where the sscanf failed for Area and Line records.
 *  Created scan_node_record() and scan_area_record() to get rid of
 *  the problems.  -mh
 */

#include "dlghead.h"
#include "format.h"

a_b_dlg(dlg, bin)
	FILE *dlg ;
	FILE *bin ;
{
	char buff[128] ;
	double x ;
	double y ;
	double xx ;
	double yy ;
	double N ;
	double S ;
	double E ;
	double W ;

	int n_lines ;
	int n_area_lines ;
	int n_atts ;
	int n_isles ;

	int start_node ;
	int end_node ;
	int left_area ;
	int right_area ;
	int n_coors ;
	int num ;
	int n_read ;
	static int line_buf[COOR_MAX] ;
	static int att_buff[COOR_MAX] ;
	static double coor_buff[COOR_MAX * 2] ;

	int num_nodes ;
	int num_areas ;
	int num_lines ;
	int num_undef ;

	int n ;
	int rem ;
	int area_lines ;

	extern	int	new_format ;

	num_nodes = 0 ;
	num_areas = 0 ;
	num_lines = 0 ;
	num_undef = 0 ;


	for(;;)
	{
		FGET ;
		switch (*buff)
		{
		case 'N':
			num_nodes++ ;
			/* dpg */
			scan_node_record( buff, &num, &x, &y, &n_lines, &n_atts) ;

			if (n_lines)
				if (n_read = read_int(dlg, n_lines, line_buf) )
				{
					printf("Error: Missing %d lines for node %d\n",
						n_read, num) ;
					n_lines -= n_read ;
				}
			if (n_atts)
				if (n_read = read_int(dlg, n_atts * 2, att_buff) )
				{
					printf("Error: Missing %d attributes for area %d\n",
						n_read, num) ;
					n_atts -= n_read / 2 ;
				}
			
			/*  dpg
			n_atts = 0 ;
			*/

			/**  take care of different parameters  */
			xx = int_params[0] * x
				+ int_params[1] * y
				+ int_params[2] ;

			yy = int_params[0] * y
				- int_params[1] * x
				+ int_params[3] ;

			fwrite("N",     sizeof(char),   1, bin) ;
			fwrite(&num,    sizeof(num),    1, bin) ;
			fwrite(&xx,      sizeof(xx),      1, bin) ;
			fwrite(&yy,      sizeof(yy),      1, bin) ;
			fwrite(&n_lines,sizeof(n_lines),1, bin) ;
			fwrite(&n_atts, sizeof(n_atts), 1, bin) ;
			if (n_lines)
				fwrite (line_buf, sizeof(*line_buf), n_lines, bin) ;
			if (n_atts)
				fwrite (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;
			break ;

		case 'A':
			num_areas++ ;
			n_atts = 0 ;	/* in case there are no values  */
			n_isles = 0 ;
			n_area_lines = 0 ;
			scan_area_record( buff, &num, &x, &y,
				&n_lines, &n_area_lines, &n_atts, &n_isles ) ;

			if (n_lines)
				if (n_read = read_int(dlg, n_lines, line_buf) )
				{
					printf("Error: Missing %d lines for area %d\n",
						n_read, num) ;
					n_lines -= n_read ;
				}


/*
* Calculate number of lines of area-line coordinates
* and skip over that number of lines.
*/
			if (n_area_lines)
			{
				n = n_area_lines / 3 ;
				rem = n_area_lines % 3 ;
				area_lines =  rem ? ++n : n ;
				for ( n = 0 ; n < area_lines ; ++n)
					FGET ;
			}



			if (n_atts)
				if (n_read = read_int(dlg, n_atts * 2, att_buff) )
				{
					printf("Error: Missing %d attributes for area %d\n",
						n_read, num) ;
					n_atts -= n_read / 2 ;
				}

			/**  take care of different parameters  */
			xx = int_params[0] * x
				+ int_params[1] * y
				+ int_params[2] ;

			yy = int_params[0] * y
				- int_params[1] * x
				+ int_params[3] ;


			fwrite("A",     sizeof(char),   1, bin) ;
			fwrite(&num,    sizeof(num),    1, bin) ;
			fwrite(&xx,      sizeof(xx),      1, bin) ;
			fwrite(&yy,      sizeof(yy),      1, bin) ;
			fwrite(&n_lines,sizeof(n_lines),1, bin) ;
			fwrite(&n_atts, sizeof(n_atts), 1, bin) ;
			fwrite(&n_isles,sizeof(n_isles),1, bin) ;
			if (n_lines)
				fwrite (line_buf, sizeof(*line_buf), n_lines, bin) ;
			if (n_atts)
				fwrite (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;
			break ;

	case 'L':
			num_lines++ ;
			n_atts = 0 ;
			sscanf(buff, "%*1c%5d%6d%6d%6d%6d%*12c%6d%6d",
				&num, &start_node, &end_node,
				&left_area, &right_area, &n_coors, &n_atts ) ;



			if (n_coors > COOR_MAX)
			{
				fprintf(stderr, "ERROR: Too many coordinates for a single line.   L %d\n", num) ;
				exit(-1) ;
			}

			if (n_coors)
			{
				if (n_read = read_doubles(dlg, n_coors * 2, coor_buff) )
				{
					printf("Error: Missing %d coordinates for line %d\n",
						n_read, num) ;
					n_coors -= n_read / 2 ;
				}

				if (new_format)
					bound_box(coor_buff, n_coors, &N, &S, &E, &W) ;
			}
			else
			{
				N = 0.0 ;
				S = 0.0 ;
				E = 0.0 ;
				W = 0.0 ;
			}

			if (n_atts)
				if (n_read = read_int(dlg, n_atts * 2, att_buff) )
				{
					printf("Error: Missing %d attributes for line %d\n",
						n_read, num) ;
					n_atts -= n_read / 2 ;
				}

			fwrite("L",         sizeof(char),      1, bin) ;
			fwrite (&num,       sizeof(num),       1, bin) ;
			fwrite (&start_node,sizeof(start_node),1, bin) ;
			fwrite (&end_node,  sizeof(end_node),  1, bin) ;
			fwrite (&left_area, sizeof(left_area), 1, bin) ;
			fwrite (&right_area,sizeof(right_area),1, bin) ;
			fwrite (&n_coors,   sizeof(n_coors),   1, bin) ;
			fwrite (&n_atts,    sizeof(n_atts),    1, bin) ;

			if (new_format)
			 {
				fwrite (&N,         sizeof(N),         1, bin) ;
				fwrite (&S,         sizeof(S),         1, bin) ;
				fwrite (&E,         sizeof(E),         1, bin) ;
				fwrite (&W,         sizeof(W),         1, bin) ;
			 }

			if (n_coors)
				fwrite (coor_buff, sizeof(*coor_buff), n_coors * 2, bin) ;
			if (n_atts)
				fwrite (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;
			break ;
		
		case 'E':
			printf("\n    nodes: %d\n", num_nodes) ;
			printf("    areas: %d\n", num_areas) ;
			printf("    lines: %d\n", num_lines) ;
			printf("  unknown: %d\n", num_undef) ;
			printf("\n") ;
			fflush(bin) ;
			return(0) ;

		default:
			printf(" unknown line: '%s'\n", buff) ;
			num_undef++ ;
		}
	}
}

/*  This strips out the Area or Line linkage record from a string  */

static  
scan_node_record( str, num, x, y, n_lines, n_atts)
	char *str ;
	int  *num ;
	double  *x, *y ;
	int  *n_lines ;
	int  *n_atts ;

{

	char  char_num[6] ;
	char  char_x[13] ;
	char  char_y[13] ;
	char  char_lines[7] ;
	char  char_atts[7] ;

	int  atoi() ;
	double  atof() ;


	sscanf(str, "%*1c%5c%12c%12c%*6c%6c%*6c%6c",
			char_num, char_x, char_y, char_lines, char_atts) ;

	/*  make sure there null terminated */
	char_num[5] = CHAR_NULL ;
	char_x[12] = CHAR_NULL ;
	char_y[12] = CHAR_NULL ;
	char_lines[6] = CHAR_NULL ;
	char_atts[6] = CHAR_NULL ;


	*num = atoi(char_num) ;
	*x = atof(char_x) ;
	*y = atof(char_y) ;
	*n_lines = atoi(char_lines) ;
	*n_atts = atoi(char_atts) ;
}

static  scan_area_record( str, num, x, y, n_lines, n_area_lines,
		  n_atts, n_isles)
	char *str ;
	int  *num ;
	double  *x, *y ;
	int  *n_area_lines, *n_lines, *n_atts, *n_isles ;

{

	char  char_a_lines[7] ;
	char  char_atts[7] ;
	char  char_isles[7] ;
	int junk;

	int  atoi() ;

/*  the area and node records are the same up to a point */
	scan_node_record( str, num, x, y, n_lines, n_atts) ;

/*  skip the info the scan_node_record() had gotten  */
	sscanf(str, "%*42c%6c%6c%*6c%6c",
			 char_a_lines, char_atts, char_isles) ;

	/*  make sure there null terminated */
	char_a_lines[6] = CHAR_NULL ;
	char_atts[6] = CHAR_NULL ;
	char_isles[6] = CHAR_NULL ;

	*n_area_lines = atoi(char_a_lines) ;
	*n_atts = atoi(char_atts) ;
	*n_isles = atoi(char_isles) ;
}

