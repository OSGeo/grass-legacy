/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

int 
datetime_get_increment_type (DateTime *dt, int *mode, int *from, int *to, int *fracsec)
{
    if (!datetime_is_valid_type(dt))
	return datetime_error_code();

    *mode = DATETIME_RELATIVE;
    *to   = dt->to;
    *fracsec = dt->fracsec;

    if (datetime_is_absolute(dt))
    {
	if(datetime_in_interval_year_month(dt->to))
	    *from = DATETIME_YEAR;
	else
	    *from = DATETIME_DAY;
    }
    else
    {
	*from = dt->from;
    }
    return 0;
}

int 
datetime_set_increment_type (DateTime *src, DateTime *incr)
{
    int mode, from, to, fracsec;

    if(datetime_get_increment_type (src, &mode, &from, &to, &fracsec) != 0)
	return datetime_error_code();
    return datetime_set_type(incr, mode, from, to, fracsec);
}
