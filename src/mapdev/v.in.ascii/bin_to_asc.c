/*  @(#)bin_to_asc.c    2.1  6/26/87  */

#include "Vect.h"
#include "Vect.h"
#include "gis.h"

int bin_to_asc(
    FILE *ascii,
    struct Map_info *Map)
{
	int type;
	double *xptr, *yptr;
	static struct line_pnts *Points;
	char buf1[100], buf2[100];

	Points = Vect_new_line_struct ();	/* init line_pnts struct */



	/* by default, read_next_line will NOT read Dead lines */
	/*  but we can override that (in Level I only) by specifying */
	/*  the type  -1, which means match all line types */
	Vect_set_constraint_type (Map, -1);


	while(1)
	{
		if (-1 == (type = Vect_read_next_line (Map, Points)))
			return (-1);
		if (type == -2)	/* EOF */
			goto done;

		switch(type)
		{
		case AREA:
			fprintf(ascii, "A  %d\n", Points->n_points);
			break;
		case LINE:
			fprintf(ascii, "L  %d\n", Points->n_points);
			break;
		case DOT:
			fprintf(ascii, "P  %d\n", Points->n_points);
			break;
		case DEAD_AREA:
			fprintf(ascii, "a  %d\n", Points->n_points);
			break;
		case DEAD_LINE:
			fprintf(ascii, "l  %d\n", Points->n_points);
			break;
		case DEAD_DOT:
			fprintf(ascii, "p  %d\n", Points->n_points);
			break;
		default:
			fprintf(ascii, "X  %d\n", Points->n_points);
			/*DEBUG*/	    fprintf (stderr, "got type %d\n", (int) type);
			break;
		}

		xptr = Points->x;
		yptr = Points->y;
		while (Points->n_points--)
		{
			G_format_northing (*yptr++, buf1, -1);
			G_format_easting (*xptr++, buf2, -1);
			fprintf(ascii, " %-12s %-12s\n", buf1, buf2);

/*			 fprintf(ascii, " %12lf %12lf\n", *yptr++, *xptr++); */
		}
	}

done:
	return (0);
}
