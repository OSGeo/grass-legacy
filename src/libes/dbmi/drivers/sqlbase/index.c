#include "globals.h"
db_driver_create_index (index)
    dbIndex *index;
{
    db_set_index_name (index, NULL);
    return create_index (index);
}

db_driver_drop_index(name)
    dbString *name;
{
    char *s;

    s = db_get_string(name);
    return drop_index (s);
}

create_index (index)
    dbIndex *index;
{
    dbString cmd;
    int stat;

    db_init_string (&cmd);
    db_append_string (&cmd, "CREATE ");
    if (db_test_index_type_unique(index))
	db_append_string (&cmd, "UNIQUE ");
    db_append_string (&cmd, "INDEX ");
    db_append_string (&cmd, get_index_name(index));
    db_append_string (&cmd, " ON ");
    db_append_string (&cmd, db_get_index_table_name (index));
    db_append_string (&cmd, " (");
    build_column_list (index, &cmd,
	db_get_index_number_of_columns(index),
	db_get_index_column_name);

    db_append_string (&cmd, ")");

    stat = execute_immediate (&cmd);
    db_free_string (&cmd);
    return stat;
}

drop_index (name)
    char *name;
{
    int stat;
    dbString cmd;

    db_init_string (&cmd);
    db_append_string (&cmd, "DROP INDEX ");
    db_append_string (&cmd, name);

    stat = execute_immediate (&cmd);
    db_free_string (&cmd);
    return stat;
}

char *
get_index_name(index)
    dbIndex *index;
{
    char dummy[2];
    char *name;

    name = db_get_index_name(index);
    if (sscanf(name, "%1s", dummy) == 1)
	    return name;
    return make_index_name();
}

/* create a new index name. To do this make up sequential names
 * and see if they already exist in the sysindexes table
 * NULL is returned if there is an error
 */
char *
make_index_name()
{
    int count;
    SQLTRCD rcd; /* return code */
    int i;
    static char name[20];
    char cmd[256];

    i = 0;
    do
    {
	sprintf (name, "IDX%d", ++i);
	sprintf (cmd, "select count(*) from sysadm.sysindexes where name = '%s'", name);
	if(rcd = sqlcom (database_cursor, cmd, NULL_TERMINATED))
	{
	    report_error (rcd, "can't create an new index name");
	    return NULL;
	}
	if(bind_int_for_fetch (database_cursor,
		1,		/* column number of count(*) */
		&count,		/* program buffer to hold results */
		SQLNPTR		/* fetch code status */
	))
	{
	    return NULL;
	}
	if(rcd = sqlexe (database_cursor))
	{
	    report_error (rcd,"make_index_name: sqlexe failure");
	    return NULL;
	}
	if (rcd = sqlfet (database_cursor))
	{
	    report_error (rcd,"make_index_name: fetch failure");
	    return NULL;
	}
    }while(count>0);
    return name;
}
