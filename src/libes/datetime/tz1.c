/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

static int have(int x, DateTime *dt)
{
    return datetime_is_between(x, dt->from, dt->to);
}

int datetime_check_timezone (
    DateTime *dt,
    int minutes)
{
    if (!datetime_is_absolute(dt))
	return datetime_error(-1, "datetime not absolute");
    if (!have(DATETIME_MINUTE, dt))
	return datetime_error(-2, "datetime has no minute");
    if (!datetime_is_valid_timezone(minutes))
	return datetime_error(-3, "invalid datetime timezone");
    return 0;
}

int 
datetime_get_timezone (DateTime *dt, int *minutes)
{
    int stat;
    stat = datetime_check_timezone(dt, dt->tz);
    if (stat == 0)
	*minutes = dt->tz;
    return stat;
}

int 
datetime_set_timezone (DateTime *dt, int minutes)
{
    int stat;
    stat = datetime_check_timezone(dt, minutes);
    if (stat == 0)
	dt->tz = minutes;
    return stat;
}

int 
datetime_unset_timezone (DateTime *dt)
{
    dt->tz = -9999;

    return 0;
}

int 
datetime_is_valid_timezone (int minutes)
{
    return (minutes >= -720 && minutes <= 780);
}
