/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

int 
datetime_is_positive (DateTime *dt)
{
    return dt->positive != 0;
}

int 
datetime_is_negative (DateTime *dt)
{
    return dt->positive == 0;
}

void 
datetime_set_positive (DateTime *dt)
{
    dt->positive = 1;
}

void 
datetime_set_negative (DateTime *dt)
{
    dt->positive = 0;
}

void 
datetime_invert_sign (DateTime *dt)
{
    dt->positive = !dt->positive;
}
