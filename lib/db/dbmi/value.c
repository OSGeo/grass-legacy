#include <stdlib.h>
#include "dbmi.h"

int
db_test_value_isnull(value)
    dbValue *value;
{
    return (value->isNull != 0);
}

int
db_get_value_int(value)
    dbValue *value;
{
    return (value->i);
}

double
db_get_value_double(value)
    dbValue *value;
{
    return (value->d);
}
/* for given value and C type of value returns double representation */
double
db_get_value_as_double(value, ctype)
    dbValue *value;
    int ctype;
{
    double val;
    
    switch ( ctype )
      {
	case ( DB_C_TYPE_INT ):
	    val = (double) db_get_value_int(value);
	    break;
	case ( DB_C_TYPE_STRING ):
	    val = atof ( db_get_value_string(value) );
	    break;
	case ( DB_C_TYPE_DOUBLE ):
	    val = db_get_value_double(value);
	    break;
	default:
	    val = 0;
      }
    return val;
}

char *
db_get_value_string(value)
    dbValue *value;
{
    return (db_get_string(&value->s));
}

int
db_get_value_year(value)
    dbValue *value;
{
    return (value->t.year);
}

int
db_get_value_month(value)
    dbValue *value;
{
    return (value->t.month);
}

int
db_get_value_day(value)
    dbValue *value;
{
    return (value->t.day);
}

int
db_get_value_hour(value)
    dbValue *value;
{
    return (value->t.hour);
}

int
db_get_value_minute(value)
    dbValue *value;
{
    return (value->t.minute);
}

double
db_get_value_seconds(value)
    dbValue *value;
{
    return (value->t.seconds);
}

void
db_set_value_null(value)
    dbValue *value;
{
    value->isNull = 1;
}

void
db_set_value_not_null(value)
    dbValue *value;
{
    value->isNull = 0;
}

void
db_set_value_int(value, i)
    dbValue *value;
    int i;
{
    value->i = i;
    db_set_value_not_null(value);
}

void
db_set_value_double(value, d)
    dbValue *value;
    double d;
{
    value->d = d;
    db_set_value_not_null(value);
}

db_set_value_string(value, s)
    dbValue *value;
    char *s;
{
    db_set_value_not_null(value);
    return db_set_string(&value->s, s);
}

void
db_set_value_year(value, year)
    dbValue *value;
    int year;
{
    value->t.year = year;
    db_set_value_datetime_not_current(value);
}

void
db_set_value_month(value, month)
    dbValue *value;
    int month;
{
    value->t.month = month;
    db_set_value_datetime_not_current(value);
}

void
db_set_value_day(value, day)
    dbValue *value;
    int day;
{
    value->t.day = day;
    db_set_value_datetime_not_current(value);
}

void
db_set_value_hour(value, hour)
    dbValue *value;
    int hour;
{
    value->t.hour = hour;
    db_set_value_datetime_not_current(value);
}

void
db_set_value_minute(value, minute)
    dbValue *value;
    int minute;
{
    value->t.minute = minute;
    db_set_value_datetime_not_current(value);
}

void
db_set_value_seconds(value, seconds)
    dbValue *value;
    double seconds;
{
    value->t.seconds = seconds;
    db_set_value_datetime_not_current (value);
}

int
db_test_value_datetime_current (value)
    dbValue *value;
{
    return (value->t.current != 0);
}

void
db_set_value_datetime_current (value)
    dbValue *value;
{
    value->t.current = 1;
    db_set_value_not_null(value);
}

void
db_set_value_datetime_not_current (value)
    dbValue *value;
{
    value->t.current = 0;
    db_set_value_not_null(value);
}

/* copy value from src to destination */
void
db_copy_value ( dst, src )
    dbValue *dst;
    dbValue *src;
{
    dst->isNull = src->isNull;
    dst->i = src->i;
    dst->d = src->d;
    if ( src->s.nalloc > 0 )
        db_copy_string ( &(dst->s), &(src->s) );
    dst->t.current = src->t.current;
    dst->t.year = src->t.year;
    dst->t.month = src->t.month;
    dst->t.day = src->t.day;
    dst->t.hour = src->t.hour;
    dst->t.minute = src->t.minute;
    dst->t.seconds = src->t.seconds;
}
