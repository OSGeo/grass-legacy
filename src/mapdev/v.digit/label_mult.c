#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "digit.h"
#include "dig_curses.h"
#include "display_line.h"
#include "debug.h"
#include "Map_proto.h"
#include "local_proto.h"
/*
**  Written by Dave Gerdes  11/1989
**  US Army Construction Engineering Research Lab
*/

#ifndef PI
#define PI 3.14159265
#endif

static double ANGLE_VARIANCE = (PI/16.);
static int unlabel_line (struct Map_info *,plus_t,struct line_pnts *);

/*
**  after labelling chosen line, continue on labelling all other
** lines that appear to be continuations.  I.E. they are the same
**  type, they continue in the same direction, +/- a given range, and
**  they are not already labelled with a different value.
*
**  returns -1 on error or 0
*/
int label_mlines (struct Map_info *map, int cat)
{
    plus_t line, prev_line, next_line;
    P_LINE *Line;
    int type, prev_node;
    plus_t **lines;
    int doit, pass, i, j;
/*DEBUG*/    char *p;

/*DEBUG*/    if (NULL != (p = getenv ("ANGLE")))
/*DEBUG*/	ANGLE_VARIANCE = PI / atof (p);

/*DEBUG*/ debugf ("Tolerance = %lf\n", ANGLE_VARIANCE); while (1)
    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose line:", tell_line_label)))
	{
	    return (-1);
	}

/*
	if (cat)
	{
	    if (0 > label_line (map, line, cat, &Gpoints))
		return (-1);
	}
	else
	    unlabel_line (map, line, &Gpoints);
*/

	type = map->Line[line].type;


	lines = dig_get_cont_lines (map, line, ANGLE_VARIANCE, 1);

	/* go through it all twice,  first to highlight, then
	** to either act or unhighlight, based on user response
	*/
	for (pass = 1 ; pass <= 2 ; pass++)
	{
	    for (i = 0 ; i < 2 ; i++)	/* do both directions */
	    {
		for (j = 0 ; next_line = lines[i][j] ; j++)
		{
		    if (!next_line)	/* no more lines */
			break;

		    Line = &(map->Line[abs(next_line)]);

		    if (pass == 1)
		    {
		    /* throw away those lines we're not interested in */
			/* if different line types, continue */
			if (type != Line->type) 
			{
			    lines[i][j] = 0;
			    break;
			}
			
			/* if line is labelled a different value then continue */
			/* this needs to be cleaned up to allow RE-labelling of cont lines */

/*
#define RELABEL
*/
#ifndef RELABEL
			if (cat)	/* else UNLABELLING */
			    if (LINE_LABELED (Line) && cat != map->Att[Line->att].cat)
			    {
				lines[i][j] = 0;
				break;
			    }
#endif
		    }


		    if(0 > V1_read_line(map, &Gpoints, Line->offset))
			return (-1);

		    if (pass == 1)
			_highlight_line(Line->type, &Gpoints, next_line, map);
		    else	/* pass 2 */
		    {
			if (!doit)
			{
			    _display_line(Line->type, &Gpoints, next_line, map);
			}
			else
			{
			    Changes_Made = 1;
			    if (cat)
			    {
				if (0 > label_line (map, abs(next_line), cat, &Gpoints))
				    return (-1);
			    }
			    else
				unlabel_line (map, abs(next_line), &Gpoints);
			}
		    }
		}
	    }
	    V_flush ();
/*
#define CONFIRM
*/
#ifdef CONFIRM
	    if (pass == 1)
	    {
		char *str;
		if (cat)
		    str = "Label these lines?";
		else
		    str = "Un-label these lines?";
		doit = mouse_yes_no (str);
	    }
#else
	    doit = 1;
#endif
	}
    }
}

static int unlabel_line (
    struct Map_info *map,
    plus_t line,
    struct line_pnts *Points)
{
    if (map->Line[line].att)
    {
	erase_line (map->Line[line].type, Points, line, map);
	dig_del_att (map, map->Line[line].att);
	map->Line[line].att = 0;
	display_line (map->Line[line].type, Points, line, map);
	return (1);
    }
    return (0);
}
