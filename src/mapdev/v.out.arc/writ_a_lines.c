#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "Vect.h"
#include "gtoa.h"

int write_area_lines (struct Map_info *Map, FILE *ascii)
{
    int count=0;
    int type;
    double *xptr, *yptr;
    static int alloc_points;
    struct line_pnts *Points;
    long offset;


	Points = Vect_new_line_struct ();

#ifdef OLD_LIB
    /* set up for alloc_points()  called by Read_line() */
    if (alloc_points == 0)
    {
	alloc_points = -1;	/* will never get here again */
	Points->alloc_points = 0;
    }
#endif /*OLD_LIB*/


#ifdef OLD_LIB
    while(1)
    {
	offset = ftell (binary);
	if (-1 == (type = dig__Read_line (&Points, binary, offset)))
	    return (-1);
	if (type == -2)	/* EOF */
	    goto done;
        else if (type==AREA)
           {
		   fprintf(ascii,"           %d\n",++count);
           xptr = Points.x;
		   yptr = Points.y;
		   while (Points.n_points--)
			   fprintf(ascii, " %12.2f%c%12.2f\n", *xptr++, separator, *yptr++);
		   fprintf(ascii,"END\n");
           }
    }
#endif /*OLD_LIB*/

    while(1)
    {
        if (-1 == (type = Vect_read_next_line (Map, Points)))
		{
           G_fatal_error ("Failure on reading vector input file.\n");
            return (-1);
		}
        if (type == -2) /* EOF */
            goto done;

        if (type==AREA)
	    {
		   fprintf(ascii,"           %d\n",++count);
           xptr = Points->x;
		   yptr = Points->y;
		   while (Points->n_points--)
			   fprintf(ascii, " %12.2f %12.2f\n", *xptr++, *yptr++);
		   fprintf(ascii,"END\n");
	    }
    }

done:
    {
		fprintf(ascii,"END\n");
		Vect_destroy_line_struct (Points);
        return (0);
    }
}
