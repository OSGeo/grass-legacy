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
