#include "gis.h"
#include "ncb.h"
intr_cats()
{
    G_set_cats_fmt ("$1% dispersion", 1.0, -1.0, 0.0, 0.0, &ncb.cats);
}
