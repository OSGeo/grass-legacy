/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "wind.h"

/* build new area 
** returns -1 on error    0 if no area built  1 on area built
**
**  If area built, Area will contain that area info
**
** take given line  create one area to left  if is area return it
**  else create one area to right if is area return it else return 0
*/ 
build_area (map, x, y, line, Area)
    struct Map_info *map;
    double x, y;
    int line;
    P_AREA *Area;
{

    if (dig_build_area_with_line (map, line, Area) > 0)
	if (dig_point_in_area (map, x, y, Area) > 0.)
	    return (1);
    if (dig_build_area_with_line (map, -line, Area) > 0)
	if (dig_point_in_area (map, x, y, Area) > 0.)
	return (1);
    return (0);
}

Del_area (map, area)
    struct Map_info *map;
    int area;
{
    P_ATT *Att;
    P_AREA *Area;
    char buf[100];


    /* Instead of changing all the code out there, just have 
    ** this routine call Del_isle if we get an isle
    */
    if (area == 0)
	return (-1);

    if (area <= 0)	/* ISLE */
	return Del_isle (map, area);

/* this needs to be modified to only happen IF Area label is on screen... */
    Area = &(map->Area[area]);
    if (!AREA_ALIVE (Area)) return (-1);


    /* remove cat on screen */
    if (AREA_LABELED (Area))
    {
	Att = &(map->Att[Area->att]);	
	R_standard_color (dcolors[CLR_ERASE]);
	_Blot (&(Att->x), &(Att->y));
	sprintf (buf, "%d", Att->cat);
	Adot (&(Att->x), &(Att->y), buf);
	_reset_area (Area, map);
    }

/*DEBUG*/ debugf ("Calling del_area\n");
    dig_del_area (map, area);
}

/* same as Del_area(), but doesn't update the screen */

_Del_area (map, area)
    struct Map_info *map;
    int area;
{
    P_ATT *Att;
    P_AREA *Area;
    char buf[100];


    /* Instead of changing all the code out there, just have 
    ** this routine call Del_isle if we get an isle
    */
    if (area == 0)
	return (-1);

    if (area <= 0)	/* ISLE */
	return Del_isle (map, area);

/* this needs to be modified to only happen IF Area label is on screen... */
    Area = &(map->Area[area]);
    if (!AREA_ALIVE (Area)) return (-1);

/*DEBUG*/ debugf ("Calling del_area\n");
    dig_del_area (map, area);
}
