/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#include "digit.h"

long ftell();

/* allow user to change type of line from AREA to LINE or back again */
/* if it affects any existing areas, they are unlabeled */
retype_line (map)
    struct Map_info *map;
{
    int line;
    P_LINE *Line;
    int area;
    long addr2;

    G_clear_screen();
    while (1)
    {
	if (0 >= (line = find_line_with_mouse (AREA | LINE," Re-type a line:", NULL)))
	{
	    return (0);
	}

	Line = &(map->Line[line]);

	switch (Line->type) {
	    case AREA:
		Changes_Made = 1;
		/* remove any bounded areas */
		if (Line->right > 0)
		    Del_area (map, Line->right);
		if (Line->right < 0)		/* ISLE */
		    Del_isle (map, ABS(Line->right));
		if (Line->left > 0)
		    Del_area (map, Line->left);
		if (Line->left < 0)		/* ISLE */
		    Del_isle (map, ABS(Line->left));

		Line->type = LINE;
		map->n_llines++;
		map->n_alines--;

		break;
	    case LINE:
		Changes_Made = 1;
		/* check any posible connecting lines to see */
		/* we area disturbing an existing area */

		if (area = check_next (map, line, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, line, LEFT))
		    Del_area (map, area);
		if (area = check_next (map, -line, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, -line, LEFT))
		    Del_area (map, area);

		Line->type = AREA;
		map->n_llines--;
		map->n_alines++;

		break;
	    default:
		continue;
		break;
	}
	if (Disp_llines)
	{
	    Disp_llines = 0;
/*-->  added SCS, RL Glenn, 9/20/91
      wanted the the labels to disappear with the re-type */
	    if (Disp_llabels)
	      {
              erase_line (Line->type, &Gpoints, line, map);
	      Disp_llabels = 0;
	      display_line (Line->type, &Gpoints, line, map);
	      Disp_llabels = 1;
	      }
	    else
	      display_line (Line->type, &Gpoints, line, map);
	    Disp_llines = 1;
	}
	else
	    display_line (Line->type, &Gpoints, line, map);

    /* Rewind, write out dead-code, then wind to next read location */
	addr2 = ftell(map->dig_fp);
	Vect__Rewrite_line(map, map->Line[line].offset, Line->type, &Gpoints);
	fseek(map->dig_fp, addr2, 0);
    }
}

/*
** this is designed to be called by retype_line when changing
**  a Line-line into and Area-line. I want to 
** check if I have interupted an existing area.
**  if so return the area number so it can be unlabeled
**  else  return 0
*/
/*
   ISLE   This looks ok for islands.
*/
check_next (map, line, direction)
    struct Map_info *map;
    int line;
    int direction;
{
    int next_line;
    int area;
    P_LINE *Line;

    next_line = dig_angle_next_line (map, line, direction);
    if (line == next_line)
	return (0);
    Line = &(map->Line[ABS(next_line)]);

    if (Line->type == AREA)
	switch (direction) {
	    case LEFT:
		if (next_line < 0)
		    area = Line->right;
		else
		    area = Line->left;
		break;
	    case RIGHT:
		if (next_line < 0)
		    area = Line->left;
		else
		    area = Line->right;
		break;
	    default:
		break;
	}
    return (area);
}
