#include <stdlib.h>
#include "Vect.h"
#include "vbuildlib.h"
#include "gis.h"

#ifdef LABEL
static int label_area (struct Map_info *,int);
#endif
/* for every area line, try to build an area */

int build_all_areas (struct Map_info *map, struct Map_info *Err)
{
    register int line, cnt;
    P_LINE *Line;
    int ret, type;
    int *errlines;  
    struct line_pnts *Points;

    /* init array */
    errlines = (int *)G_malloc( (map->n_lines + 1 ) * sizeof(int) ); 
    for (line = 1 ; line <= map->n_lines ; line++) {
	errlines[line] = 0;
    }

    Points = Vect_new_line_struct();
    
    cnt = 0;
    for (line = 1 ; line <= map->n_lines ; line++)
    {
        G_percent(line, map->n_lines, 2);
	Line = &(map->Line[line]);
	if (!LINE_ALIVE (Line) || Line->type != AREA)
	    continue;
	
	if (Line->right == 0) {
	    ret = build_area(map, line);
	    if ( ret  == 0) 
		cnt++;
	    else {
		errlines[abs(ret)] = 1;
	    }
	}

	if (Line->left == 0) {
	    ret = build_area(map, -line);
	    if ( ret  == 0) 
		cnt++;
	    else {
		errlines[abs(ret)] = 1;
	    }
	}
    }

    /* Report error lines and write to error file */
    for (line = 1 ; line <= map->n_lines ; line++) {
	if ( errlines[line] == 0 )
            continue;

	fprintf ( stderr, "Unclosed area, free end or edge inside area: line %d\n", line);
	
	if (Err != NULL ) { 
	    type = V2_read_line( map, Points, line);
	    Vect_write_line (Err, type, Points);
	}
    }
    free (errlines);
    
    return (cnt);
}

/* build area 
**
**  call build_area_w_line with correct args 
*/ 
int 
build_area (struct Map_info *map, int line)
{
    /* dont make this static, is not like struct line_pnts */
    register int ret, area, isle;
    P_AREA Area;
    int errline;

    area = 0;
    isle = 0;
    errline = 0;
    Area.n_lines = Area.alloc_lines = 0;

    if ((ret = dig_build_area_with_line (map, line, &Area, &errline)) > 0)
    {
	area = dig_new_area (map, &Area, 0);	/* give dummy att info*/
	free (Area.lines);
    }
    else 
    if (ret == -2)  /* found an island */
    {
	isle = dig_new_isle (map, &Area, 0);	/*ISLE*/  /* give dummy area info */
	free (Area.lines);
    }

    if (area || isle) {
	return (0);
    } else { 
	return (errline);
    }
}


#ifdef LABEL
static double local_x;	/* filled by label_area  */
static double local_y;	/* used by tell_area_label */
static int local_area;
static int local_prev;
static P_AREA local_struct;

/* ask user to select area to label and create  new area and label */
/* returns 0 OK  or -1 no area created */
static int label_area (struct Map_info *map, int cat)
{
    int line, area, att;
    double x, y;
    int ret;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");
	if (x == 0.0 && y == 0.0)
	    return (-1);

	/* change color so they know something happend */
	R_standard_color (dcolors[AQUA]); 
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	local_prev = 0;	/* reset static flag */
	if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_label)))
	{
	    unset_dot (x, y);
	    continue;
	}
	if (make_area_label (map, line) >= 0)	/* completed an area? */
	{

	  if (1==(ret=mouse_yes_no ("Accept this area? ")) || ret==3) /* yes? */
	  {
	    /* if this far, then an area is selected, either old or new */
	    /*  if local_area, then is old, else  Garea holds area info */
	    if (local_area)
	    {
		P_ATT *AP;
		char buf[100];

		if (!map->Area[local_area].att)
		{
#ifdef DEBUG
debugf ("Error: existing area had 0 att field\n");
#endif
		    map->Area[local_area].att = 
			new_att (map, local_x, local_y, AREA, local_area, cat);
		}

		area = local_area;
	    }
	  }
	  else	/* cleanup and leave */
	  {
	    display_line (map->Line[line].type, &Gpoints, line, map);
	    R_standard_color (dcolors[BLACK]);
	    Blot (&local_x, &local_y);
	    if (local_area)
		display_area (local_area, map);
	    else
		_reset_area (&Garea, map);
	  }
	}
	else  /* area not made */
	{
	    R_standard_color (dcolors[BLACK]);
	    Blot (&local_x, &local_y);
	}
    }
}
#endif

#ifdef CHECK_AREA
/* given x, y  and line number,  check if x, y is within a predefined 
**  area bounded by  line
*/
int 
check_area (struct Map_info *map, int line, double x, double y)
{
    line = abs (line);
#ifdef DEBUG
debugf ("Check_area: line %d R %d L %d (%lf, %lf)\n", line, map->Line[line].right, map->Line[line].left, x, y);
#endif
    if (map->Line[line].right)
	if (dig_point_in_area (map, x, y, &(map->Area[map->Line[line].right])))
	{
#ifdef DEBUG
debugf ("Check_area:  POINT IN AREA(right)  returned TRUE\n");
#endif
	    return (map->Line[line].right);
	}
    if (map->Line[line].left)
	if (dig_point_in_area (map, x, y, &(map->Area[map->Line[line].left])))
	{
#ifdef DEBUG
debugf ("Check_area:  POINT IN AREA(left) returned TRUE\n");
#endif
	    return (map->Line[line].left);
	}
#ifdef DEBUG
debugf ("Check_area:  POINT IN AREA returned FALSE\n");
#endif
    return (0);
}
#endif
