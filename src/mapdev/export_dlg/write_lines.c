/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "export_dlg.h"

#define LINE_FMT "L%5d%6d%6d%6d%6d            %6d%6d%6d\n"

write_dlg_lines (map, fp)
    struct Map_info *map;
    FILE *fp;
{
    P_LINE *Line;
    P_AREA *Area;
    int line, left, right;
    int n_points, n_atts;
    static struct line_pnts Gpoints;

    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line = &(map->Line[line]);

	/* if reach the DOTs, then we are done */
	if (Line->type == DOT)
	    break;

	if (Line->left < 0)
	{
	    left = abs(map->Isle[abs (Line->left)].area);
	}
	else
	    left = Line->left;

	if (Line->right < 0)
	{
	    right = abs(map->Isle[abs (Line->right)].area);
	}
	else
	    right = Line->right;

	if (line == 1)
	{
	    Area = &(map->Area[1]);
	    Gpoints.alloc_points = 0;
	    dig_alloc_points (&Gpoints, 5);
	    Gpoints.n_points = n_points = 5;
	    Gpoints.x[0] = Area->W; Gpoints.y[0] = Area->N;
	    Gpoints.x[1] = Area->E; Gpoints.y[1] = Area->N;
	    Gpoints.x[2] = Area->E; Gpoints.y[2] = Area->S;
	    Gpoints.x[3] = Area->W; Gpoints.y[3] = Area->S;
	    Gpoints.x[4] = Area->W; Gpoints.y[4] = Area->N;
	}
	else
	{
	    if (0 > dig__Read_line (&Gpoints, map->digit, Line->offset))
	    {
		fprintf (stderr, "ERROR reading line %d from file\n", line);
		exit (-1);
	    }
	}
	
	if (Line->att)
	    n_atts = 1;
	else 
	    n_atts = 0;


	fprintf (fp, LINE_FMT, 
		line,				/* index of element */
		Line->N1,			/* start node */
		Line->N2,			/* end node */
		left,				/* left area */
		right,				/* right area */
		Gpoints.n_points,		/* # of coords */
		n_atts,				/* # of atts */
		0);				/* unused */
	start_coords ();
	write_coords (fp, Gpoints.n_points, Gpoints.x, Gpoints.y);
	end_coords (fp);

	if (n_atts)
	{
	    start_att ();
	    write_att (fp, DEF_MAJOR, map->Att[Line->att].cat);
	    end_att (fp);
	}
    }
}
