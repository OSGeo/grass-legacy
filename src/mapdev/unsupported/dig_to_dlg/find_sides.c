/*  @(#)find_sides.c	2.1  6/26/87  */
#include "structures.h"
/*  Finds the areas that bound on right and left of a line.
 *  If the number found on right or left differes from 1,
 *  -1 is returned, otherwise, return 0.
 */

find_sides(at_line, right, left)
	int at_line ;
	int *right ;
	int *left ;
{
	int pos_line ;
	int neg_line ;
	int n_left, n_right ;
	int a, l ;

	pos_line = at_line ;
	neg_line = -at_line ;
	n_right = 0 ;
	n_left = 0 ;
	*right = 0 ;
	*left = 0 ;

	for(a=1; a<=n_areas; a++)
	{
		if (areas[a].num_lines == 0)
			continue ;
		for(l=0; l<areas[a].num_lines; l++)
		{
			if (areas[a].line_list[l] == 0)  /* into islands */
			{
				pos_line = -at_line ;
				neg_line = at_line ;
				continue ;
			}
			if (areas[a].line_list[l] == pos_line)
			{
				*right = a ;
				n_right++ ;
				continue ;
			}
			if (areas[a].line_list[l] == neg_line)
			{
				*left = a ;
				n_left++ ;
				continue ;
			}
		}
	}

	if ( (n_left != 1) || (n_right != 1) )
		return(-1) ;
	return(0) ;
}
