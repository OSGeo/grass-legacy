#include "datetime.h"
#include <stdio.h>

main(argc, argv) char *argv[];
{
    DateTime dt;
    int year, month, day, hour, minute;
    int have_year = 0, have_month = 0, have_day = 0;
    int have_hour = 0, have_minute = 0, have_second = 0;
    int new_to = 0, new_from = 0, round = 0;
    int have_reset = 0;
    int to, from;
    int mode;
    int have_mode = 0;
    double second;
    int i;
    char buf[1024];
    char word1[100], word2[100];

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

	if (sscanf (argv[i],"reset=%[^,],%[^,],%d", word1, word2, &round) == 3)
	{
	    have_reset = 1;

	    if (strcmp(word1,"year")==0) new_from = DATETIME_YEAR;
	    if (strcmp(word1,"month")==0) new_from = DATETIME_MONTH;
	    if (strcmp(word1,"day")==0) new_from = DATETIME_DAY;
	    if (strcmp(word1,"hour")==0) new_from = DATETIME_HOUR;
	    if (strcmp(word1,"minute")==0) new_from = DATETIME_MINUTE;
	    if (strcmp(word1,"second")==0) new_from = DATETIME_SECOND;

	    if (strcmp(word2,"year")==0) new_to = DATETIME_YEAR;
	    if (strcmp(word2,"month")==0) new_to = DATETIME_MONTH;
	    if (strcmp(word2,"day")==0) new_to = DATETIME_DAY;
	    if (strcmp(word2,"hour")==0) new_to = DATETIME_HOUR;
	    if (strcmp(word2,"minute")==0) new_to = DATETIME_MINUTE;
	    if (strcmp(word2,"second")==0) new_to = DATETIME_SECOND;
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
	if (year < 0)
	{
	    datetime_set_negative(&dt);
	    year = -year;
	}
	dt.year = year;
    }
    if (have_month)
    {
	if (month < 0)
	{
	    datetime_set_negative(&dt);
	    month = -month;
	}
	dt.month = month;
    }
    if (have_day)
    {
	if (day < 0)
	{
	    datetime_set_negative(&dt);
	    day = -day;
	}
	dt.day = day;
    }
    if (have_hour)
    {
	if (hour < 0)
	{
	    datetime_set_negative(&dt);
	    hour = -hour;
	}
	dt.hour = hour;
    }
    if (have_minute)
    {
	if (minute < 0)
	{
	    datetime_set_negative(&dt);
	    minute = -minute;
	}
	dt.minute = minute;
    }
    if (have_second)
    {
	if (second < 0)
	{
	    datetime_set_negative(&dt);
	    second = -second;
	}
	dt.second = second;
    }

    datetime_format (&dt, buf);
    fprintf (stdout,"%s\n", buf);
    if (have_reset)
    {
	if (datetime_change_from_to (&dt, new_from, new_to, round))
	{
	    fprintf (stderr, "ERROR(%d): %s\n",
		datetime_error_code(), datetime_error_msg());
	}
	datetime_format (&dt, buf);
	fprintf (stdout,"%s\n", buf);
    }
    exit(0);
}
