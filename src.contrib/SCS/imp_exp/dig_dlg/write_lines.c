/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
/*#include "digit.h"*/
#include "gis.h"
#include "digit.h"
#include "export_dlg.h"

#define LINE_FMT "L%5d%6d%6d%6d%6d            %6d%6d%6d\n"
#ifdef SCS_MODS
#define FLAT_FMT "%5d\t%s\t%6d \n"
#endif

write_dlg_lines (map, fp, ftmp, ctmp)
    struct Map_info *map;
    FILE *fp, *ftmp, *ctmp;
{
    P_LINE *Line;
    P_AREA *Area;
int ii;
    int line, left, right;
    int n_points, n_atts;
    static struct line_pnts *Gpoints;
    static int first = 1;

#ifdef SCS_MODS
    register int i;
    char label[50];
    char *X, *Y;
    double N, S, E, W;
    P_NODE *Node;

    /* release some memory */
    if (map->Node != NULL)
       {
	   for (i = 1 ; i <= map->n_nodes ; i++)
	       if (map->Node[i].alloc_lines > 0)
	          {
		   if (map->Node[i].lines != NULL)
		       free (map->Node[i].lines);
		   if (map->Node[i].angles != NULL)
		       free (map->Node[i].angles);
	          }
	   free (map->Node);
	   map->Node = 0;
       }
#endif

    /*
    **  SEt up so that we can call this repetitively in the future
    */
    if (first)
    {
        first = 0;
        Gpoints = Vect_new_line_struct ();
    }

    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line = &(map->Line[line]);

#ifndef SCS_MODS
        /* if reach the DOTs, then we are done */
        if (Line->type == DOT)
               break;
#else
    /*  SCS wants sites as degenerate lines in dlg file */
#endif

	if (Line->left < 0)
	{
	    left = -(map->Isle[abs (Line->left)].area);
	}
	else
	    left = Line->left;

	if (Line->right < 0)
	{
	    right = -(map->Isle[abs (Line->right)].area);
	}
	else
	    right = Line->right;

	/* Note ++
	**   This should more correctly be done by creating an Xarray/Yarray
        **   and then calling Vect_copy_xy_to_points ()  As dig_alloc_points
        **   is not currently a documented function.
        */
	if (line == 1)
	{
            Area = &(map->Area[1]);
	    dig_alloc_points (Gpoints, 5);    /*Note ++ */
	    Gpoints->n_points = n_points = 5;
	    Gpoints->x[0] = Area->W; Gpoints->y[0] = Area->N;
	    Gpoints->x[1] = Area->E; Gpoints->y[1] = Area->N;
	    Gpoints->x[2] = Area->E; Gpoints->y[2] = Area->S;
	    Gpoints->x[3] = Area->W; Gpoints->y[3] = Area->S;
	    Gpoints->x[4] = Area->W; Gpoints->y[4] = Area->N;
	}
	else
	{
  	    if (0 > Vect__Read_line (map, Gpoints, Line->offset)) 
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
	        Gpoints->n_points,               /* # of coords */
		n_atts,				/* # of atts */
		0);				/* unused */
  	start_coords ();
	write_coords (fp, Gpoints->n_points, Gpoints->x, Gpoints->y);
	end_coords (fp); 

  	if (n_atts)
	{
	    start_att ();
	    write_dlg_att (fp, DEF_MAJOR, map->Att[Line->att].cat);
	    end_att (fp);
#ifdef SCS_MODS
	    if (get_label(label,map->Att[Line->att].cat,ctmp) == 0)
	       fprintf(stderr,"\n* WARNING * no label for line %d, cat# %d\n",
		   line,map->Att[Line->att].cat);
	    fprintf (ftmp, FLAT_FMT, 
		line,			    /* line number */
		label, 			    /* category label */
		map->Att[Line->att].cat);   /* category code value */
#endif
	} 
    }
}
