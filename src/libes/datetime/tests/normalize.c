#include "datetime.h"

static void normalize_month_year();

_datetime_normalize (dt)
    DateTime *dt;
{
    int pos, ndays;
    int ad;

    if(!datetime_is_valid_type(dt))
	return datetime_error_code();

/* process the carry in this order
 *  SECONDS -> MINUTES
 *  MINUTES -> HOURS
 *  HOURS   -> DAYS
 *  DAYS    -> MONTHS
 *  MONTHS  -> YEARS
 *
 * note that DAYS->MONTHS only occurs for mode==ABSOLUTE
 */
    for (pos = dt->to; pos > dt->from; pos--) /* don't do the from */
    {
	switch (pos)
	{
	case DATETIME_MONTH:
		normalize_month_year(dt);
		break;

	case DATETIME_DAY: /* this case can only happen in ABSOLUTE mode */
		normalize_month_year(dt);
		ad = datetime_is_positive(dt);
		ndays = datetime_days_in_month (dt->year, dt->month, ad);
		while (dt->day < 1)
		{
		    dt->month--;
		    dt->day += ndays;
		    normalize_month_year(dt);
		    ad = datetime_is_positive(dt);
		    ndays = datetime_days_in_month (dt->year, dt->month, ad);
		}
		while (dt->day > ndays)
		{
		    dt->day -= ndays;
		    dt->month++;
		    normalize_month_year(dt);
		    ad = datetime_is_positive(dt);
		    ndays = datetime_days_in_month (dt->year, dt->month, ad);
		}
		break;

	case DATETIME_HOUR:
		while (dt->hour < 0)
		{
		    dt->hour += 24;
		    dt->day--;
		}
		while (dt->hour >= 24)
		{
		    dt->hour -= 24;
		    dt->day++;
		}
		break;

	case DATETIME_MINUTE:
		while (dt->minute < 0)
		{
		    dt->minute += 60;
		    dt->hour--;
		}
		while (dt->minute >= 60)
		{
		    dt->minute -= 60;
		    dt->hour++;
		}
		break;

	case DATETIME_SECOND:
		while (dt->second < 0.0)
		{
		    dt->second += 60.0;
		    dt->minute--;
		}
		while (dt->second >= 60.0)
		{
		    dt->second -= 60.0;
		    dt->minute++;
		}
		break;
	}
    }

/* if the dt->from is negative, set it postive and invert the datetime sign */
    switch (dt->from)
    {
    case DATETIME_YEAR:
	if (dt->year < 0)
	{
	    dt->year = -dt->year;
	    datetime_invert_sign(dt);
	}
	break;

    case DATETIME_MONTH:
	if (dt->month < 0)
	{
	    dt->month = -dt->month;
	    datetime_invert_sign(dt);
	}
	break;

    case DATETIME_DAY:
	if (dt->month < 0)
	{
	    dt->month = -dt->month;
	    datetime_invert_sign(dt);
	}
	break;

    case DATETIME_HOUR:
	if (dt->hour < 0)
	{
	    dt->hour = -dt->hour;
	    datetime_invert_sign(dt);
	}
	break;

    case DATETIME_MINUTE:
	if (dt->minute < 0)
	{
	    dt->minute = -dt->minute;
	    datetime_invert_sign(dt);
	}
	break;

    case DATETIME_SECOND:
	if (dt->second < 0.0)
	{
	    dt->second = -dt->second;
	    datetime_invert_sign(dt);
	}
	break;
    }

    return 0;
}

static
void
normalize_month_year(dt)
    DateTime *dt;
{
    int is_absolute;

    if (dt->from != DATETIME_YEAR) return;

    is_absolute = datetime_is_absolute(dt);

    /* force rounding into the future (1bc->1ad) */
    if (is_absolute && !datetime_is_positive(dt))
    {
	dt->year = -dt->year;
	datetime_set_positive(dt);
    }

    while (dt->month < 1)
    {
	dt->year--;
	dt->month += 12;
	if (dt->year == 0 && is_absolute)
	    dt->year = -1;
    }
    while (dt->month > 12)
    {
	dt->year++;
	dt->month -= 12;
	if (dt->year == 0 && is_absolute)
	    dt->year = 1;
    }
    if (dt->year < 0)
    {
	dt->year = -dt->year;
	datetime_set_negative(dt);
    }
}
