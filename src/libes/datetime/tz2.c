/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

int datetime_change_timezone (
    DateTime *dt,
    int minutes) /* new timezone in minutes */
{
    int stat;
    int old_minutes, diff_minutes;
    DateTime incr;

    stat = datetime_get_timezone(dt, &old_minutes);
    if (stat != 0)
	return stat;
    if (!datetime_is_valid_timezone (minutes))
	return datetime_error (-4, "invalid datetime timezone");

/* create a relative minute increment */
    datetime_set_type (&incr, DATETIME_RELATIVE, DATETIME_MINUTE, DATETIME_MINUTE, 0);

    /* BB - needed to set SIGN here */
    diff_minutes = minutes-old_minutes;
    if(diff_minutes >= 0){
	datetime_set_minute (&incr, diff_minutes);
    }
    else{
	datetime_invert_sign(&incr);
	datetime_set_minute (&incr, -diff_minutes);
    }

    return datetime_increment (dt, &incr);
}

int datetime_change_to_utc (DateTime *dt)
{
    return datetime_change_timezone (dt, 0);
}

void datetime_decompose_timezone (int tz, int *hours,int *minutes)
{
    if (tz < 0)
	tz = -tz;
    *hours = tz/60;
    *minutes = tz%60;
}
