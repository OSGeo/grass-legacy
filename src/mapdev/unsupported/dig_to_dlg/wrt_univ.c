/*  @(#)write_univ.c	2.1  6/26/87  */

/*  file includes  write_univ_node(),  write_univ_lines(), write_univ_areas()
*/

#include	<stdio.h>
#include	"structures.h"
#include	"universe.h"


static	int univ_node,	univ_line, neg_univ_line ;

write_univ_node(f_dlg)
	FILE *f_dlg ;
{
	int at_node ;
	int n_atts ;
	int endpoint ;
	int no_lines ;

	univ_node = n_nodes + 1 ;
	univ_line = n_lines + 1 ;
	neg_univ_line = -univ_line ;
	no_lines = 2 ;

	n_atts = 0 ;


		fwrite("N",     sizeof(char),   1, f_dlg) ;

		fwrite(&univ_node,    sizeof(univ_node),    1, f_dlg) ;
		fwrite(univ_xarray, sizeof(univ_xarray[0]), 1, f_dlg) ;
		fwrite(univ_yarray, sizeof(univ_yarray[0]), 1, f_dlg) ;
		fwrite(&no_lines, sizeof(no_lines), 1, f_dlg) ;

		fwrite(&n_atts, sizeof(n_atts), 1, f_dlg) ;

		fwrite (&univ_line,  sizeof(univ_line), 1, f_dlg) ;
		fwrite (&neg_univ_line,  sizeof(neg_univ_line), 1, f_dlg) ;


}	/*  write_univ_node()  */



write_univ_areas(f_dlg)
	FILE *f_dlg ;
{
	int n_atts ;
	int i ;
	int zero, one ;
	int	area_num ;
	int num_line_ents ;

	char	buf[80] ;

	zero = 0 ;
	one = 1 ;
	n_atts = 1 ;
	num_line_ents = 1 ;

	area_num = one ;

		/* Write entry 'A 1' outside map to DLG file */

	fwrite("A",        sizeof(char),      1, f_dlg) ;
	fwrite(&area_num,  			 sizeof(int),	    1, f_dlg) ;
	fwrite(&areas[area_num].cent_x,    sizeof(double),	 	1, f_dlg) ;
	fwrite(&areas[area_num].cent_y,    sizeof(double),		1, f_dlg) ;
	fwrite(&num_line_ents,      sizeof(int),    1, f_dlg) ;
	fwrite(&n_atts,             sizeof(int),    1, f_dlg) ;

	/*  number of islands  */
	fwrite(&zero,			 sizeof(int),    1, f_dlg) ;
	/*  line list  */
	fwrite(&neg_univ_line,		 sizeof(int),	 1, f_dlg) ;

		/*  categories,  major and minor  */
	fwrite(&zero,           sizeof(int),    1, f_dlg) ;
	fwrite(&zero,			sizeof(int),    1, f_dlg) ;


		/* Write entry 'A 2' inside map to DLG file */

	area_num = 2 ;
	n_atts = 0 ;

	/*  calculate total # of line entries;  start off with 1 to include the
	*	universe bounding box line.
	*/

	num_line_ents = 1 ;
	for ( i = 0;  i < areas[area_num].n_islands ; i++)
		num_line_ents += 1 + islands[areas[area_num].island_list[i]].num_lines ;


	fwrite("A",        sizeof(char),      1, f_dlg) ;
	fwrite(&area_num,  			 sizeof(int),	    1, f_dlg) ;
	fwrite(&areas[area_num].cent_x,    sizeof(double), 	1, f_dlg) ;
	fwrite(&areas[area_num].cent_y,    sizeof(double),		1, f_dlg) ;
	fwrite(&num_line_ents,      sizeof(int),    1, f_dlg) ;
	fwrite(&n_atts,             sizeof(int),    1, f_dlg) ;

	/*  number of islands  */
	fwrite(&areas[area_num].n_islands,			 sizeof(int),    1, f_dlg) ;
	/*  line list  */
	fwrite(&univ_line,		 sizeof(int),	 1, f_dlg) ;


	/*  island list  */
	for ( i = 0;  i < areas[area_num].n_islands ; i++)
	{
		fwrite(&zero,		 sizeof(int),	 1, f_dlg) ;
		fwrite( islands[areas[area_num].island_list[i]].line_list,
				sizeof(int),
				islands[areas[area_num].island_list[i]].num_lines,
				f_dlg) ;
	}


}
			/*  write_univ_areas()  */




write_univ_lines( f_dlg)
	FILE *f_dlg ;
{
	int n_atts ;
	int n_points ;
	int i ;
	int num ;
	int one, two ;
	char buff[128] ;
	double N, S, E, W ;

	n_points =  UNIV_N_POINTS ;
	n_atts = 0 ;
	one = 1 ;
	two = 2 ;

		bound_box(univ_xarray, univ_yarray, n_points, &N, &S, &E, &W) ;

		fwrite("L",         sizeof(char),      1, f_dlg) ;
		fwrite (&univ_line,       sizeof(univ_line),       1, f_dlg) ;

		/*  beginning node  */
		fwrite (&univ_node,       sizeof(univ_node),       1, f_dlg) ;
		/*  ending node  */
		fwrite (&univ_node,       sizeof(univ_node),       1, f_dlg) ;

		/*  left area, right area  */
		fwrite (&one,		sizeof(one),      1, f_dlg) ;
		fwrite (&two,	     sizeof(two),     1, f_dlg) ;

		fwrite (&n_points,   sizeof(n_points),   1, f_dlg) ;
		fwrite (&n_atts,     sizeof(n_atts),     1, f_dlg) ;
		fwrite (&N,          sizeof(N),          1, f_dlg) ;
		fwrite (&S,          sizeof(S),          1, f_dlg) ;
		fwrite (&E,          sizeof(E),          1, f_dlg) ;
		fwrite (&W,          sizeof(W),          1, f_dlg) ;

		/*  x,y coordinates  */
		for (i=0; i<n_points; i++)
		{
			fwrite (univ_xarray+i, sizeof(*univ_xarray), 1, f_dlg) ;
			fwrite (univ_yarray+i, sizeof(*univ_yarray), 1, f_dlg) ;
		}

}

