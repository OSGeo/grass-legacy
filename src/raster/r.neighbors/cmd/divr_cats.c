#include "gis.h"
#include "ncb.h"
divr_cats()
{
    G_set_cats_fmt ("$1 $?different categories$category$", 1.0, 0.0, 0.0, 0.0, &ncb.cats);
}
