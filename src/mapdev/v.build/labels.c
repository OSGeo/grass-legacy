#include "gis.h"
#include "Vect.h"
#include "vbuildlib.h"

static int label_line(struct Map_info *,P_ATT *);
static int label_area ( struct Map_info *, P_ATT *);

int read_atts (struct Map_info *map, char *file)
{
    register int cnt;
    FILE *fp;
    P_ATT Att;
    int ret;
    char type;

    /*
    ** must insure that all lines in dig_att file are 50 chars long
    ** this is done at beginning of build.vect 
    */
    if ((fp = fopen (file, "r")) == NULL)
    {
	fprintf (stderr, "No attribute file exists. Creating an empty one.\n");
	G__make_mapset_element ("dig_att");
	if ((fp = fopen (file, "w")) != NULL)
	    fclose (fp);
	return (0);
    }

    for (cnt = 0 ;  ; cnt++)
    {
	if ((ret=read_att(fp, &type, &Att.x, &Att.y, &Att.cat, &Att.offset))==1)
	{
	    /*EOF */
	    goto leave;
	}
	if (ret < 0)	/* dead att */
	{
	    cnt--;
	    continue;
	}
	if (0 > dig_alloc_att (map, 1))	/* out of memory */
	{
	    fprintf (stderr, "Out of memory error while reading attributes\n");
	    cnt = -1;
	    goto leave;
	}
	type = dig_old_to_new_type (type);
	Att.type = type;

	switch (type) {
	    case LINE:
	    case DOT:
		label_line (map, &Att);
		break;
	    case AREA:
		label_area (map, &Att);
		break;
	    default:
	    fprintf (stderr, "ERROR: READ_ATTS  GOT TYPE %x\n", (int) Att.type);
		break;
	}
    }
    /*NOTREACHED*/
leave:

    fclose  (fp);
    return (cnt);
}

/* label_line ()
** for each line  if ALIVE(line) and Att.coor w/in line.bbox
**   alloc_space in array for 1 more line and record it.
** for each line in saved array
**	if line is closer than closest line to att, note it.
**  note:::  all of the above and then some is done by dig_point_to_line () !!
** 
** return closest line or 0 on no match
** return -1 on error
*/

/*  
**  I will assume for now that the  LAST, read it, LAST! attribute that
**  matches a feature is the one that will stick
*/
static int label_line (
    struct Map_info *map,
    P_ATT *Att)
{
    plus_t line;
    P_LINE *Line;
    int tmp;
    char type;

    tmp = 0;
    type = Att->type;
/* if LINE, then catch ALL lines, not just line-lines */
    if (type == LINE)
	type = LINE | AREA;
    line = dig_point_to_line (map, Att->x, Att->y, type);
    if (line <= 0)
    {
/*DEBUG*/ fprintf (stderr, "Failed to attach an attribute (category %d) to a line.\n", Att->cat);
	return (line);
    }
    else
    {
	Line = &(map->Line[line]);
	if (Line->att)
	{
	    fprintf (stderr, "WARNING: line %d label: %d matched another label: %d.\n", line, map->Att[Line->att].cat, Att->cat);
	    /* delete old att and take the new one */
	    dig__del_att (map, Line->att);
	    Line->att = 0;
	}
	/*
	else
	*/
	{
	    Att->index = line;
	    tmp = dig__new_att (map, Att->x, Att->y, (char) Att->type, Att->index, Att->cat, Att->offset);
	    if (tmp < 0)
	    {
		return (-1);
	    }
	    Line->att = tmp;
	}
    }
    return (tmp);
}


/*  label_area ()
** for each area  if ALIVE(area) and Att.coor w/in area.bbox
**   alloc_space in array for 1 more area and record it.
** for each area in saved array
**	if point is inside area, note it and distance from nearest area on area
**
**  if array == 1
**	return array
**  else
**      for each area in new saved array
**	    if nearest area is closer than closest area to att, note it.
**
**  return closest area or 0 on no match
**  return -1 on error
**    
*/ 
static int label_area (
    struct Map_info *map,
    P_ATT *Att)
{
    P_AREA *Area;
    plus_t area;
    int tmp;

    tmp = 0;
    area = dig_point_to_area (map, Att->x, Att->y);
    if (area <= 0)
    {
/*DEBUG*/ fprintf (stderr, "PNT_TO_AREA failed: (%f, %f) (Category %d)\n", Att->x, Att->y, Att->cat);
	return (area);
    }
    else
    {
	Area = &(map->Area[area]);
	if (Area->att)
	{
	    fprintf (stderr, "WARNING: area %d label: %d matched another label: %d.\n", area, map->Att[Area->att].cat, Att->cat);
	    dig__del_att (map, Area->att);
	    Area->att = 0;
	}
	/*
	else
	*/
	{
	    Att->index = area;
	    /*
	    tmp = dig__new_att (map, Att->x, Att->y, (char) dig_new_to_old_type (Att->type), Att->index, Att->cat, Att->offset);
	    */
	    tmp = dig__new_att (map, Att->x, Att->y, (char) Att->type, Att->index, Att->cat, Att->offset);
	    if (tmp < 0)
		return (-1);
	    Area->att = tmp;
	}
    }
    return (tmp);
}
