#include "dbmi.h"
#include "macros.h"

db__send_datetime (t)
    dbDateTime *t;
{
    DB_SEND_CHAR(t->current);
    if (!t->current)
    {
	DB_SEND_INT(t->year);
	DB_SEND_INT(t->month);
	DB_SEND_INT(t->day);
	DB_SEND_INT(t->hour);
	DB_SEND_INT(t->minute);
	DB_SEND_DOUBLE(t->seconds);
    }

    return DB_OK;
}

db__recv_datetime (t)
    dbDateTime *t;
{
    DB_RECV_CHAR(&t->current);
    if (!t->current)
    {
	DB_RECV_INT(&t->year);
	DB_RECV_INT(&t->month);
	DB_RECV_INT(&t->day);
	DB_RECV_INT(&t->hour);
	DB_RECV_INT(&t->minute);
	DB_RECV_DOUBLE(&t->seconds);
    }

    return DB_OK;
}
