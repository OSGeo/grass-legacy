#include "Vect.h"
#include "gis.h"
#include "null.h"


is_labeled (cat)
    CELL cat;
{
    static CELL tmp;
    tmp = cat;
    return !ISNULL(&tmp);
}

mark_unlabeled (cat)
    CELL *cat;
{
    SETNULL(cat);
}

get_area_label (Map, index, cat)
    struct Map_info *Map;
    int index;
    CELL *cat;
{
    int att;

    att = Map->Area[index].att;
    if (att == 0)
	mark_unlabeled(cat);
    else
	*cat = Map->Att[att].cat;
}

get_line_label (Map, index, cat)
    struct Map_info *Map;
    int index;
    CELL *cat;
{
    int att;

    att = Map->Line[index].att;
    if (att == 0)
	mark_unlabeled(cat);
    else
	*cat = Map->Att[att].cat;
}

get_site_label (Map, index, cat)
    struct Map_info *Map;
    int index;
    CELL *cat;
{
    get_line_label (Map, index, cat);
}
