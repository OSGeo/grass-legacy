#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <dbmi.h>

$include sqltypes;

#include "globals.h"

db_driver_list_tables (list, count, system)
    dbString **list;
    int *count;
    int system;
{
    $string name[MAX_STRING_SIZE];
    $string owner[MAX_STRING_SIZE];
    $int num;
    $char *cur = "c";
    int err;
    int i;
    dbString *s;

/* user tables have tabid > 99 */
/* get the number of tables first */

    if (system)
	$DECLARE $cur cursor for select count(*) from systables where tabid < 99;
    else
	$DECLARE $cur cursor for select count(*) from systables where tabid > 99;
    if (sql_error(NULL))
	return DB_FAILED;
    $OPEN $cur;
    if (sql_error(NULL))
	return DB_FAILED;
    $FETCH $cur into $num;
    err = sql_error(NULL);
    $CLOSE $cur;
    $FREE $cur;
    if (err)
	return DB_FAILED;

/* now get the table names */
    if (system)
	$DECLARE $cur cursor for select tabname, owner from systables where tabid < 99;
    else
	$DECLARE $cur cursor for select tabname, owner from systables where tabid > 99;
    if (sql_error(NULL))
	return DB_FAILED;
    $OPEN $cur;
    if (sql_error(NULL))
	return DB_FAILED;
    s = db_alloc_string_array (num);
    if (s == NULL)
    {
	$CLOSE $cur;
	$FREE $cur;
	return DB_FAILED;
    }
    for (i = 0; i < num; i++)
    {
	$FETCH $cur into $name, $owner;
	if (sql_eof())
	    break;
	if (sql_error(NULL))
	    break;
	db_set_string (&s[i], "");
	if (*owner)
	{
	    db_append_string (&s[i], owner);
	    db_append_string (&s[i], ".");
	}
	db_append_string (&s[i], name);
    }
    $CLOSE $cur;
    $FREE $cur;

    if (i < num)
    {
	db_free_string_array (s, num);
	return DB_FAILED;
    }

    *list = s;
    *count = num;
    return DB_OK;
}
