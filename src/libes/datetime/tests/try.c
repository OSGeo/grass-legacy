#include "datetime.h"
#include <stdio.h>

main(argc, argv) char *argv[];
{
    DateTime dt;
    int year, month, day, hour, minute;
    int have_year = 0, have_month = 0, have_day = 0;
    int have_hour = 0, have_minute = 0, have_second = 0;
    int to, from;
    int mode;
    int have_mode = 0;
    double second;
    int i;
    char buf[1024];

    from = DATETIME_SECOND + 1;
    to   = DATETIME_YEAR - 1;
    mode = DATETIME_RELATIVE;

    for (i = 1; i < argc; i++)
    {
	if (strcmp (argv[i], "-r")==0)
	{
	    mode = DATETIME_RELATIVE;
	    have_mode = 1;
	}

	if (sscanf (argv[i],"year=%d", &year) == 1)
	{
	    if (!have_mode)
		mode = DATETIME_ABSOLUTE;
	    have_year = 1;
	    if (from > DATETIME_YEAR) from = DATETIME_YEAR;
	    if (to   < DATETIME_YEAR) to   = DATETIME_YEAR;
	}

	if (sscanf (argv[i],"month=%d", &month) == 1)
	{
	    have_month = 1;
	    if (from > DATETIME_MONTH) from = DATETIME_MONTH;
	    if (to   < DATETIME_MONTH) to   = DATETIME_MONTH;
	}

	if (sscanf (argv[i],"day=%d", &day) == 1)
	{
	    have_day = 1;
	    if (from > DATETIME_DAY) from = DATETIME_DAY;
	    if (to   < DATETIME_DAY) to   = DATETIME_DAY;
	}

	if (sscanf (argv[i],"hour=%d", &hour) == 1)
	{
	    have_hour = 1;
	    if (from > DATETIME_HOUR) from = DATETIME_HOUR;
	    if (to   < DATETIME_HOUR) to   = DATETIME_HOUR;
	}

	if (sscanf (argv[i],"minute=%d", &minute) == 1)
	{
	    have_minute = 1;
	    if (from > DATETIME_MINUTE) from = DATETIME_MINUTE;
	    if (to   < DATETIME_MINUTE) to   = DATETIME_MINUTE;
	}

	if (sscanf (argv[i],"second=%lf", &second) == 1)
	{
	    have_second = 1;
	    if (from > DATETIME_SECOND) from = DATETIME_SECOND;
	    if (to   < DATETIME_SECOND) to   = DATETIME_SECOND;
	}

    }
	
    if (datetime_set_type (&dt, mode, from, to, 6) != 0)
    {
	fprintf (stderr, "ERROR(%d): %s\n",
	    datetime_error_code(), datetime_error_msg());
	exit(1);
    }

    if (have_year)
    {
	if (mode == DATETIME_ABSOLUTE && year < 0)
	{
	    datetime_set_negative(&dt);
	    year = -year;
	}
	dt.year = year;
    }
    if (have_month)
	dt.month = month;
    if (have_day)
	dt.day = day;
    if (have_hour)
	dt.hour = hour;
    if (have_minute)
	dt.minute = minute;
    if (have_second)
	dt.second = second;

    datetime_format (&dt, buf);
    fprintf (stdout,"%s\n", buf);
    _datetime_normalize(&dt);
    datetime_format (&dt, buf);
    fprintf (stdout,"%s\n", buf);
    exit(0);
}
