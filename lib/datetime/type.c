/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

int 
datetime_set_type (DateTime *dt, int mode, int from, int to, int fracsec)
{
    dt->mode   = mode;
    dt->from   = from;
    dt->to     = to;
    dt->fracsec = fracsec;

    dt->year   = 0;
    dt->month  = 0;
    dt->day    = 0;
    dt->hour   = 0;
    dt->minute = 0;
    dt->second = 0.0;
    datetime_unset_timezone(dt);

    dt->positive = 1;

    return datetime_check_type (dt);
}

int 
datetime_get_type (DateTime *dt, int *mode, int *from, int *to, int *fracsec)
{
    *mode = dt->mode;
    *to   = dt->to;
    *from = dt->from;
    *fracsec = dt->fracsec;
    return datetime_check_type (dt);
}

int 
datetime_is_valid_type (DateTime *dt)
{
   /* Returns 0 if DateTime structure is not valid. */
    return datetime_check_type (dt) == 0;
}

int 
datetime_check_type (DateTime *dt)
{
   /* Returns 0 for a valid DateTime structure.
      Sets the error code and error message if the structure is not
         valid.  Returns error code. */
    switch (dt->mode)
    {
    case DATETIME_ABSOLUTE:
    case DATETIME_RELATIVE:
	break;
    default: return datetime_error(-1, "invalid datetime 'mode'");
    }

    if (!datetime_is_between (dt->from, DATETIME_YEAR, DATETIME_SECOND))
	return datetime_error(-2, "invalid datetime 'from'");
    if (!datetime_is_between (dt->to, DATETIME_YEAR, DATETIME_SECOND))
	return datetime_error(-3, "invalid datetime 'to'");
    if (dt->from > dt->to)
	return datetime_error(-4, "invalid datetime 'from-to'");
    if (dt->mode == DATETIME_RELATIVE)
    {
	if (datetime_in_interval_year_month (dt->from)
	&& !datetime_in_interval_year_month (dt->to))
	    return datetime_error(-5, "invalid relative datetime 'from-to'");
	if (datetime_in_interval_day_second (dt->from)
	&& !datetime_in_interval_day_second (dt->to))
	    return datetime_error(-5, "invalid relative datetime 'from-to'");
    }
    if (dt->mode == DATETIME_ABSOLUTE && dt->from != DATETIME_YEAR)
	return datetime_error(-6, "invalid absolute datetime 'from'");
    if (dt->to == DATETIME_SECOND && dt->fracsec < 0)
	return datetime_error(-7, "invalid datetime 'fracsec'");

    return 0;
}

int 
datetime_in_interval_year_month (int x)
{
    return datetime_is_between (x, DATETIME_YEAR, DATETIME_MONTH);
}

int 
datetime_in_interval_day_second (int x)
{
    return datetime_is_between (x, DATETIME_DAY, DATETIME_SECOND);
}

int 
datetime_is_absolute (DateTime *dt)
{
    return (dt->mode == DATETIME_ABSOLUTE);
}

int 
datetime_is_relative (DateTime *dt)
{
    return (dt->mode == DATETIME_RELATIVE);
}
