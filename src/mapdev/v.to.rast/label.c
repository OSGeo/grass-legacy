#include "Vect.h"
#include "gis.h"
#include "null.h"


int is_labeled (CELL cat)
{
    static CELL tmp;
    tmp = cat;
    return !ISNULL(&tmp);
}

int mark_unlabeled (CELL *cat)
{
    SETNULL(cat);

    return 0;
}

int get_area_label (struct Map_info *Map, int index, CELL *cat)
{
    int att;

    att = Map->Area[index].att;
    if (att == 0)
	mark_unlabeled(cat);
    else
	*cat = Map->Att[att].cat;

    return 0;
}

int get_line_label (struct Map_info *Map, int index, CELL *cat)
{
    int att;

    att = Map->Line[index].att;
    if (att == 0)
	mark_unlabeled(cat);
    else
	*cat = Map->Att[att].cat;

    return 0;
}

int get_site_label (struct Map_info *Map, int index, CELL *cat)
{
    get_line_label (Map, index, cat);

    return 0;
}
