#include "Vect.h"
#include "dig_atts.h"
/*
**  Written by: Dave Gerdes 5 1988
**  US Army Construction Engineering Research Lab
*/

/* _del_att ()
**
**
**   same as del_att  except it does not update the dig_att file

**    changes type to DEAD_type  and re-writes structure to Att file
**
** 	returns 0 or -1 on error
*/
int dig__del_att ( struct Map_info *map, int att)
{
    int type;
    P_ATT *Att;

#ifdef FOOLISH
    /* see defines.h */
    map->Att[att].type <<= 4;
#endif

    Att = &(map->Att[att]);
    switch (Att->type) {
	case LINE:
	    type = DEAD_LINE;
	    break;
	case AREA:
	    type = DEAD_AREA;
	    break;
	case DOT:
	    type = DEAD_DOT;
	    break;
	default:
#ifdef DEBUG
debugf ("del_att:  Att->type is bad %x\n", (int) Att->type);
#endif
	     type = DEAD_LINE;
	    break;
    }
#ifdef DEBUG
debugf ("DEL_ATT: old type %02x new type %02x\n", (int)Att->type, (int)type);
#endif
    Att->type = type;
/*
    map->n_atts--;
*/
    return (0);
}

/* del_att ()
**
**    changes type to DEAD_type  and re-writes structure to Att file
**
** 	returns 0 or -1 on error
*/
int dig_del_att ( struct Map_info *map, int att)
{
    int type;
    P_ATT *Att;

#ifdef FOOLISH
    /* see defines.h */
    map->Att[att].type <<= 4;
#endif

    Att = &(map->Att[att]);
    switch (Att->type) {
	case LINE:
	    type = DEAD_LINE;
	    break;
	case AREA:
	    type = DEAD_AREA;
	    break;
	case DOT:
	    type = DEAD_DOT;
	    break;
	default:
#ifdef DEBUG
debugf ("del_att:  Att->type is bad %x\n", (int) Att->type);
#endif
	     type = DEAD_LINE;
	    break;
    }
#ifdef DEBUG
debugf ("DEL_ATT: old type %02x new type %02x\n", (int)Att->type, (int)type);
#endif
    Att->type = type;
    /*
    map->n_atts--;
    */
    fseek (map->att_fp, Att->offset, 0);
    if (0>write_att (map->att_fp, (char) dig_new_to_old_type (Att->type), Att->x, Att->y, Att->cat))
	return (-1);
    return (0);
}

/* returns new att index */
/* adds new attribute to P_ATTs but does not creat an entry in att file
**  this if for import_dig where we are going thru the att file attatching
**  existing atts to existing features
*/
int dig__new_att (	/* create new att  */
    struct Map_info *map,
    double x,double y,
    char type,
    int index,
    int cat,
    long offset)
{
    int att;
    P_ATT *AP;

    if (0 > dig_alloc_att (map, 1))
	return (-1);

    att = ++(map->n_atts);
    AP = &(map->Att[att]);

    AP->offset = offset;
    AP->cat = cat;
    AP->type = type;
    AP->index = index;
    AP->x = x;
    AP->y = y;
#ifdef DEBUG
debugf ("NEW_ATT: type %d  index %d cat %d att %d\n",
	type, index, cat, att);
#endif

    return (att);
}


/* adds att info to map.Att array, and appends new att info to */
/*  att file	*/
/* returns new att index */
int dig_new_att (	/* create new att  */
    struct Map_info *map,
    double x,double y,
    char type,
    int index,
    int cat)
{
    long offset;
    int att;

    fseek (map->att_fp, 0L, 2);
    offset = ftell (map->att_fp);
    write_att (map->att_fp, (char) dig_new_to_old_type (type), x, y, cat);

    att = dig__new_att (map, x, y, type, index, cat, offset);

    return (att);
}

/* re-write a given attribute to att file */
int dig_update_att ( struct Map_info *map, int att)
{
    P_ATT *Att;

    Att = &(map->Att[att]);
    fseek (map->att_fp, Att->offset, 0);
    if (0>write_att (map->att_fp, (char) dig_new_to_old_type (Att->type), Att->x, Att->y, Att->cat))
	return (-1);
    return 0;
}
