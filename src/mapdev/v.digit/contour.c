/*
**  Written by Dave Gerdes  12/1989
**  US Army Construction Engineering Research Lab
angle*/

#include <stdlib.h>
#include <math.h>
#include "digit.h"
#include "display_line.h"
#include "dig_curses.h"
#include "debug.h"
#include "Map_proto.h"
#include "local_proto.h"

/* Label contours
**   Contour interval is specified previously
**   User selects starting line
**   If it is not currently labelled, user is asked for its elevation 
**   User selects Ending line (with rubber line)
**   If it is not currently labelled, user is asked for its elevation
**   each line crossed by the rubber line is checked, and if
**   The number of intervals * the contour interval == the difference
**      between the elevations of the two selected lines, all the intermediate
**	lines will be labelled appropriately
*/

#define PI 3.141592

#define LIST_DUPLINE 1
#define LIST_DUPCONT 2
#define LIST_MEMORY  3

struct intersect {
    plus_t line;
    double x, y;
};
int cmp_x(struct intersect *, struct intersect *);
int cmp_y(struct intersect *, struct intersect *);
int line_in_bbox (P_LINE *,double,double,double,double);
static int check_list (struct Map_info *, struct intersect *,int);

int label_contour (struct Map_info *map, int interval)
{
    P_LINE *Line;
    static struct line_pnts Points;
    static int first_time = 1;
    double N, S, E, W;
#define LIST_SIZE 50
    struct intersect list[LIST_SIZE];
    char buf[100];
    int cnt = 0;
    plus_t line;
    plus_t line1, line2;
    int label1, label2;
    int label_diff;
    int incr;
    int start;

    double x1, y1;
    double x2, y2;
    double xtmp, ytmp;
    int i;
    int ret;
    int n_interval;
    double xdiff, ydiff;
    int dir;
    int n_interv, int_cnt;
    plus_t **lines;
    int label;
    int n, j;
    int next_line, cat;

    if (first_time)
    {
	first_time = 0;
	Points.alloc_points = 0;
	dig_alloc_points (&Points, 2);
    }

    Clear_info();
    if (0 >= (line1 = find_line_with_mouse (LINE | AREA, "Select first line and point:", tell_line_label)))
    {
	return (0);
    }
    Points.x[0] = Point_X; Points.y[0] = Point_Y;
    if (!LINE_LABELED (&(map->Line[line1])))
    {
	
	label1 = 0;
	while (label1 == 0)
	{
	    Clear_base ();
	    _Clear_info ();
	    Write_info (2, "Enter elevation for this line: ");
	    Get_curses_text(buf);
	    label1 = atoi (buf);
	    Clear_info ();
	}
    }
    else
	label1 =  (map->Att[map->Line[line1].att].cat);

    x1 = Point_X;
    y1 = Point_Y;
    if (0 >= (line2 = _find_line_with_mouse (LINE | AREA, "Select ending line and point:", tell_line_label, USE_LINE, x1, y1)))
    {
	V1_read_line (map, &Gpoints, map->Line[line1].offset);
	display_line (map->Line[line1].type, &Gpoints, line1, map);
	return (-1);
    }
    x2 = Point_X;
    y2 = Point_Y;
    if (!LINE_LABELED (&(map->Line[line2])))
    {
	
	label2 = 0.0;
	while (label2 == 0.0)
	{
	    Clear_base ();
	    _Clear_info ();
	    Write_info (2, "Enter elevation for this line: ");
	    Get_curses_text(buf);
	    label2 = atoi (buf);
	    Clear_info ();
	}
    }
    else
	label2 = (map->Att[map->Line[line2].att].cat);

    Points.x[1] = Point_X; Points.y[1] = Point_Y;
    Points.n_points = 2;
    dig_bound_box2 (&Points, &N, &S, &E, &W, map->head.orig_scale); /*4.0*/

    for (line = 1 ; line < map->n_lines ; line++)
    {
	Line = &(map->Line[line]);
	if (!LINE_ALIVE(Line))
	    continue;
	if (line_in_bbox (Line, N, S, E, W))
	{

	    V1_read_line (map, &Gpoints, Line->offset);
	    for (i = 1 ; i < Gpoints.n_points ; i++)
	    {
		if ((Gpoints.y[i-1] < S && Gpoints.y[i] < S) ||
		    (Gpoints.y[i-1] > N && Gpoints.y[i] > N) ||
		    (Gpoints.x[i-1] > E && Gpoints.x[i] > E) ||
		    (Gpoints.x[i-1] < W && Gpoints.x[i] < W))
		    continue;

		if (0 >= dig_find_intersection (
		    Points.x[0], Points.y[0], Points.x[1], Points.y[1],
		    Gpoints.x[i-1], Gpoints.y[i-1], Gpoints.x[i], Gpoints.y[i],
		    &xtmp, &ytmp))
		    continue;

/*DEBUG*/ debugf ("Crosses line %d  Cnt %d\n", line, cnt+1);
		
		if (cnt >= LIST_SIZE)
		    return -1; /* too many lines */

		list[cnt].x = xtmp;
		list[cnt].y = ytmp;
		list[cnt].line = line;
		cnt ++;
	    }
	}
    }

    if ((ret = check_list (map, list, cnt)) != 0)
    {
	Curses_error ("ERROR: Crosses same line more than once");
    }

    /* get greatest variance */
    /* and use that axis to to comparisons */

    ydiff = fabs (Points.y[1] - Points.y[0]);
    xdiff = fabs (Points.x[1] - Points.x[0]);

    label_diff = abs (label1 - label2);

    if (xdiff > ydiff)
    {
	qsort (list, cnt, sizeof (struct intersect), cmp_x);
	if (x1 < x2)
	    dir = 1;
	else 
	    dir = -1;
    }
    else
    {
	qsort (list, cnt, sizeof (struct intersect), cmp_y);
	if (y1 < y2)
	    dir = 1;
	else 
	    dir = -1;
    }
    

/* n_interve = 1 + number of internal lines */
    n_interv = cnt + 1;

    /* adjust it in case we have crossed external lines */
    if (list[0].line == line1 || list[0].line == line2)
	n_interv--;

    if (list[cnt-1].line == line1 || list[cnt-1].line == line2)
	n_interv--;

    if ( (n_interv * interval) != label_diff)
    {
	Curses_error ("Not correct number of intervals");
	goto ending;
    }

/************************************************************************/
    /* check for conflict with existing labels */
    if (dir > 0)
    {
	start = label1;
	if (label1 < label2)
	    incr =  interval;
	else
	    incr =  -interval;
    }
    else 
    {
	start = label2;
	if (label2 < label1)
	    incr =  interval;
	else
	    incr =  -interval;
    }


    int_cnt = 1;
    for (i = 0 ; i < cnt ; i++)
    {
	if (list[i].line == line1 || list[i].line == line2)
	    continue;

	Line = &(map->Line[list[i].line]) ;
	if (LINE_LABELED (Line))
	{
	    if (map->Att[Line->att].cat != (int) (start + int_cnt * incr))
	    {
		Curses_error ("Labelled line does not agree");
		goto ending;
	    }
	}
	int_cnt++;
    }
/************************************************************************/

    /*
    ** make sure we label the 1st and last lines if they are not already
    */
    V1_read_line (map, &Gpoints, map->Line[line1].offset);
    if (!LINE_LABELED (&(map->Line[line1])))
    {
	label_line (map, line1, (int)label1, &Gpoints);
    }
    else
	display_line (map->Line[line1].type, &Gpoints, line1, map);

    V1_read_line (map, &Gpoints, map->Line[line2].offset);
    if (!LINE_LABELED (&(map->Line[line2])))
    {
	label_line (map, line2, (int)label2, &Gpoints);
    }
    else
	display_line (map->Line[line2].type, &Gpoints, line2, map);

    Changes_Made = 1;

    /* and finally, just do it */
    int_cnt = 1;
    for (i = 0 ; i < cnt ; i++)
    {
	if (list[i].line == line1 || list[i].line == line2)
	    continue;

	Line = &(map->Line[list[i].line]) ;
	if (LINE_LABELED (Line))  /* already tested above */
	{
	    int_cnt++;
	    continue;
	}

	label = (start + int_cnt * incr);

	lines = dig_get_cont_lines (map, list[i].line, PI/8., 1);

	for (n = 0 ; n < 2 ; n++)	/* do both directions */
	{
	    for (j = 0 ; (next_line = lines[n][j]) ; j++)
	    {
		if (!next_line)	/* no more lines */
		    break;

		Line = &(map->Line[abs(next_line)]);

		if (LINE_LABELED (Line) && map->Att[Line->att].cat != label)
		{
		    /* TODO find a better way to handle this */
		    Curses_error ("Ran into a line of different value");
		    if(0 > V1_read_line(map, &Gpoints, Line->offset))
			return (-1);
		    highlight_line (Line->type, &Gpoints, next_line, map);
		    break;
		}


		if(0 > V1_read_line(map, &Gpoints, Line->offset))
		    return (-1);

		if (0 > label_line (map, abs(next_line), label, &Gpoints))
		    return (-1);
	    }
	}

	int_cnt++;
    }
    return (1);

ending:
    /* clean up highlit lines */
    V1_read_line (map, &Gpoints, map->Line[line1].offset);
    display_line (map->Line[line1].type, &Gpoints, line1, map);

    V1_read_line (map, &Gpoints, map->Line[line2].offset);
    display_line (map->Line[line2].type, &Gpoints, line2, map);

    return (-1);
    
}

int cmp_x (struct intersect *a, struct intersect *b)
{
    if (a->x < b->x)
	return (-1);
    else if (a->x > b->x)
	return (1);
    else
	return (0);
}

int cmp_y (struct intersect *a, struct intersect *b)
{
    if (a->y < b->y)
	return (-1);
    else if (a->y > b->y)
	return (1);
    else
	return (0);
}

static int check_list (struct Map_info *map, struct intersect *list, int cnt)
{
    plus_t **clines;
    int i, j, k, l;
    char buf[100];

    /*DEBUG*/ 
    debugf ("LIST:\n");
    for (i = 0 ; i < cnt ; i++)
    {
	sprintf (buf, "%3d (%f,%f)\n", list[i].line, list[i].x, list[i].y);
	debugf (buf);
    }

    /* check for crossing same line more than once */
/*DEBUG*/ debugf ("In CHECK_LIST \n");
    for (i = 0 ; i < cnt ; i++)
	for (j = i+1 ; j < cnt ; j++)
	{
	    if (list[i].line == list[j].line)
		return (LIST_DUPLINE);
	}

    /* check for crossing same 'continuous' line more than once */
    for (i = 0 ; i < cnt ; i++)
    {
	clines = dig_get_cont_lines (map, list[i].line, PI/8., 1);
	if (NULL == clines)
	    return (LIST_MEMORY);
	for (j = 0 ; j < cnt ; j++)
	{
	    if (j == i)
		continue;

	    for (k = 0 ; k < 2 ; k++)	/* fore/aft */
		for (l = 0 ; clines[k][l] ; l++)	/* cont lines */
		{
		    if (clines[k][l] == list[j].line)
			return (LIST_DUPCONT);
		}
	}
    }
    return (0);
}

int line_in_bbox ( P_LINE *Line, double N,double S,double E,double W)
{
    if (Line->N < S)
	return 0;
    if (Line->S > N)
	return 0;
    if (Line->E < W)
	return 0;
    if (Line->W > E)
	return 0;

    return 1;
}
