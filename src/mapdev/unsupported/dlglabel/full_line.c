/*  @(#)full_line.c	2.1  6/26/87  */
#include "dlg.h"

static int last_n ;
static double *coors ;

full_plot_line(choice, node_color1, node_color2, line_color1, line_color2)
	int choice ;
	char	*node_color1, *node_color2 ;
	char	*line_color1, *line_color2 ;
{
	double x, y ;
	int n_coors ;

	if (! choice)
		return(-1) ;

	last_n = choice ;
	if (read_coors(choice) < 0)
	{
		printf("full_plot_line:  Error: Read for line %d\n", choice) ;
		return(-1)  ;
	}

	if (line[choice].n_coors > 2)
	/* Do half lines in one color, other half in other */
	{
		n_coors = line[choice].n_coors / 2 ;

		R_standard_color( D_translate_color(line_color1)) ;
		plot_all_coors(n_coors+1, coors) ;

		R_standard_color( D_translate_color(line_color2)) ;
		plot_all_coors(line[choice].n_coors - n_coors ,
				   coors + 2 * n_coors) ;
	}
	else if (line[choice].n_coors == 2)   /* Then split the single line */
	{
		R_standard_color( D_translate_color(line_color1)) ;
		First(coors+0, coors+1) ;
		x = (coors[0] + coors[2]) / 2.0 ;
		y = (coors[1] + coors[3]) / 2.0 ;
		Next(&x, &y) ;
		R_standard_color( D_translate_color(line_color2)) ;
		Next(coors+2, coors+3) ;
	}

	R_standard_color( D_translate_color(node_color1)) ;
	Blot(node[line[choice].start_node].x, node[line[choice].start_node].y);
	R_standard_color( D_translate_color(node_color2)) ;
	Blot(node[line[choice].end_node].x, node[line[choice].end_node].y);
}

re_full_plot_line(node_color1, node_color2, line_color1, line_color2)
	char	*node_color1, *node_color2 ;
	char	*line_color1, *line_color2 ;
{
	int n_coors ;
	double x, y ;

	if (line[last_n].n_coors > 2)
	/* Do half lines in one color, other half in other */
	{
		n_coors = line[last_n].n_coors / 2 ;

		R_standard_color( D_translate_color(line_color1)) ;
		plot_all_coors(n_coors+1, coors) ;

		R_standard_color( D_translate_color(line_color2)) ;
		plot_all_coors(line[last_n].n_coors - n_coors, coors + 2 * n_coors) ;
	}
	else if (n_coors == 2)   /* Then split the single line */
	{
		R_standard_color( D_translate_color(line_color1)) ;
		First(coors+0, coors+1) ;
		x = (coors[0] + coors[2]) / 2.0 ;
		y = (coors[1] + coors[3]) / 2.0 ;
		Next(&x, &y) ;
		R_standard_color( D_translate_color(line_color2)) ;
		Next(coors+2, coors+3) ;
	}

	R_standard_color( D_translate_color(node_color1)) ;
	Blot(node[line[last_n].start_node].x, node[line[last_n].start_node].y);
	R_standard_color( D_translate_color(node_color2)) ;
	Blot(node[line[last_n].end_node].x, node[line[last_n].end_node].y);
}
