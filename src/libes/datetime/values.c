/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include "datetime.h"

static int have( int x, DateTime *dt)
{
    return datetime_is_between(x, dt->from, dt->to);
}

int datetime_check_year (
    DateTime *dt,
    int year)
{
    if (!have(DATETIME_YEAR, dt))
	return datetime_error(-2, "datetime has no year");
    if (year < 0)
	return datetime_error(-1, "invalid datetime year");
    if (datetime_is_absolute(dt) && year <= 0)
	return datetime_error(-1, "invalid datetime year");
    return 0;
}

int datetime_check_month (
    DateTime *dt,
    int month)
{
    if (!have(DATETIME_MONTH, dt))
	return datetime_error(-2, "datetime has no month");
    if (month < 0)
	return datetime_error(-1,"invalid datetime month");
    if (datetime_is_absolute(dt) && (month < 1 || month > 12))
	return datetime_error(-1,"invalid datetime month");
/*
    if (dt->from != DATETIME_MONTH && month > 11)
	return datetime_error(-1,"invalid datetime month");
BILL CHANGED TO: */

    if (datetime_is_relative(dt) &&
	dt->from != DATETIME_MONTH && month > 11)
	return datetime_error(-1,"invalid datetime month");
    return 0;
}

int datetime_check_day( DateTime *dt, int day)
{
    int month, year, ad;
    int stat;

    if (!have(DATETIME_DAY, dt))
	return datetime_error(-2, "datetime has no day");
    if (day < 0)
	return datetime_error(-1,"invalid datetime day");
    if (datetime_is_absolute(dt))
    {
	stat = datetime_get_month (dt, &month);
	if (stat != 0) return stat;
	stat = datetime_get_year (dt, &year);
	if (stat != 0) return stat;
	ad = datetime_is_positive(dt);
	if (day < 1 || day > datetime_days_in_month(year, month, ad))
	    return datetime_error(-1,"invalid datetime day");
    }
    return 0;
}

int datetime_check_hour (
    DateTime *dt,
    int hour)
{
    if (!have(DATETIME_HOUR, dt))
	return datetime_error(-2, "datetime has no hour");
    if (hour < 0)
	return datetime_error(-1,"invalid datetime hour");
    if (dt->from != DATETIME_HOUR && hour > 23)
	return datetime_error(-1,"invalid datetime hour");
    return 0;
}

int datetime_check_minute (
    DateTime *dt,
    int minute)
{
    if (!have(DATETIME_MINUTE, dt))
	return datetime_error(-2, "datetime has no minute");
    if (minute < 0)
	return datetime_error(-1,"invalid datetime minute");
    if (dt->from != DATETIME_MINUTE && minute > 59)
	return datetime_error(-1,"invalid datetime minute");
    return 0;
}

int datetime_check_second ( DateTime *dt, double second)
{
    if (!have(DATETIME_SECOND, dt))
	return datetime_error(-2, "datetime has no second");
    if (second < 0)
	return datetime_error(-1,"invalid datetime second");
    if (dt->from != DATETIME_SECOND && second >= 60.0)
	return datetime_error(-1,"invalid datetime second");
    return 0;
}

int datetime_check_fracsec (DateTime *dt, int fracsec)
{
    if (!have(DATETIME_SECOND, dt))
	return datetime_error(-2, "datetime has no fracsec");
    if (fracsec < 0)
	return datetime_error(-1,"invalid datetime fracsec");
    return 0;
}

int datetime_get_year( DateTime *dt, int *year)
{
    int stat;
    stat = datetime_check_year(dt, dt->year);
    if (stat == 0)
	*year = dt->year;
    return stat;
}

int datetime_set_year ( DateTime *dt, int year)
{
    int stat;
    stat = datetime_check_year(dt, year);
    if (stat == 0)
    {
	dt->year = year;
	if(datetime_is_absolute(dt))
	    dt->day = 0;
    }
    return stat;
}

int datetime_get_month( DateTime *dt, int *month)
{
    int stat;
    stat = datetime_check_month(dt, dt->month);
    if (stat == 0)
	*month = dt->month;
    return stat;
}

int datetime_set_month ( DateTime *dt, int month)
{
    int stat;
    stat = datetime_check_month(dt, month);
    if (stat == 0)
    {
	dt->month = month;
	if(datetime_is_absolute(dt))
	    dt->day = 0;
    }
    return stat;
}

int datetime_get_day( DateTime *dt, int *day)
{
    int stat;
    stat = datetime_check_day(dt, dt->day);
    if (stat == 0)
	*day = dt->day;
    return stat;
}

int datetime_set_day ( DateTime *dt, int day)
{
    int stat;
    stat = datetime_check_day(dt, day);
    if (stat == 0)
	dt->day = day;
    return stat;
}

int datetime_get_hour( DateTime *dt, int *hour)
{
    int stat;
    stat = datetime_check_hour(dt, dt->hour);
    if (stat == 0)
	*hour = dt->hour;
    return stat;
}

int datetime_set_hour( DateTime *dt, int hour)
{
    int stat;
    stat = datetime_check_hour(dt, hour);
    if (stat == 0)
	dt->hour = hour;
    return stat;
}

int datetime_get_minute( DateTime *dt, int *minute)
{
    int stat;
    stat = datetime_check_minute(dt, dt->minute);
    if (stat == 0)
	*minute = dt->minute;
    return stat;
}

int datetime_set_minute( DateTime *dt, int minute)
{
    int stat;
    stat = datetime_check_minute(dt, minute);
    if (stat == 0)
	dt->minute = minute;
    return stat;
}

int datetime_get_second(DateTime *dt, double *second)
{
    int stat;
    stat = datetime_check_second(dt, dt->second);
    if (stat == 0)
	*second = dt->second;
    return stat;
}

int datetime_set_second( DateTime *dt, double second)
{
    int stat;
    stat = datetime_check_second(dt, second);
    if (stat == 0)
	dt->second = second;
    return stat;
}

int datetime_get_fracsec( DateTime *dt, int *fracsec)
{
    int stat;
    stat = datetime_check_fracsec(dt, dt->fracsec);
    if (stat == 0)
	*fracsec = dt->fracsec;
    return stat;
}

int datetime_set_fracsec(DateTime *dt, int fracsec)
{
    int stat;
    stat = datetime_check_fracsec(dt, fracsec);
    if (stat == 0)
	dt->fracsec = fracsec;
    return stat;
}
