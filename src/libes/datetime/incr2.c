/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

int 
datetime_is_valid_increment (DateTime *src, DateTime *incr)
{
    return datetime_check_increment (src, incr) == 0;
}


int 
datetime_check_increment (DateTime *src, DateTime *incr)
{
    if (!datetime_is_valid_type(src))
	return 1;
    if (!datetime_is_valid_type(incr))
	return 2;

    if (!datetime_is_relative(incr))
	return datetime_error (-1, "datetime increment mode not relative");
    if (incr->to > src->to)
	return datetime_error (-2, "datetime increment too precise");

    if (datetime_in_interval_year_month (src->to) && !datetime_in_interval_year_month (incr->to))
	return datetime_error (-3, "illegal datetime increment interval");

    if (datetime_in_interval_day_second (src->to) && !datetime_in_interval_day_second (incr->to))
	return datetime_error (-4, "illegal datetime increment interval");

    return 0;
}
