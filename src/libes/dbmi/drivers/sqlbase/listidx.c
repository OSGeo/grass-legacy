#include "globals.h"


db_driver_list_indexes (tableName, list, count)
    dbString *tableName;
    dbIndex **list;
    int *count;
{
    return list_indexes (db_get_string (tableName), list, count);
}

static int get_number_of_indexes();
static int get_index_basic_info();
static int get_index_column_names();

list_indexes (tableName, idxlist, idxcount)
    char *tableName;
    dbIndex **idxlist;
    int *idxcount;
{
    char tbname[256], tbcreator[256];
    char fullname[256];
    dbIndex *list;
    int n, count;

    *idxlist = NULL;
    *idxcount = 0;

    decompose_tablename (tableName, tbcreator, tbname);
    compose_tablename (fullname, tbcreator, tbname);


    if(get_number_of_indexes (tbcreator, tbname, &count) != DB_OK)
	return DB_FAILED;
    if (count <= 0)
	return DB_OK;
    list = db_alloc_index_array (count);
    if (list == NULL)
	return DB_FAILED;
    if(get_index_basic_info (tbcreator, tbname, list, &count) != DB_OK)
	goto fail;

    for (n = 0; n < count; n++)
    {
	if(db_set_index_table_name (&list[n], fullname) != DB_OK)
	    goto fail;
	if(get_index_column_names (&list[n]) != DB_OK)
	    goto fail;
    }

    *idxlist = list;
    *idxcount = count;
    return DB_OK;
fail:
    db_free_index_array (list, count);
    return DB_FAILED;
}


static int
get_number_of_indexes (tbcreator, tbname, count)
    char *tbcreator;
    char *tbname;
    int *count;
{
    dbIndex *list = NULL;
    return get_index_basic_info (tbcreator, tbname, list, count);
}

static int
get_index_basic_info (tbcreator, tbname, list, count)
    char *tbcreator;
    char *tbname;
    dbIndex *list;
    int *count;
{
    char name[256];
    int ncols;
    char creator[256];
    char uniquerule[50];
    char cmd[256];
    int eof;
    SQLTRCD rcd;
    int n;
    dbIndex *idx;

    *count = 0;

    sprintf (cmd, "select distinct name,creator,colcount,uniquerule from sysadm.sysindexes where tbcreator = '%s' and tbname = '%s'", tbcreator, tbname);

    if (rcd = sqlcom (database_cursor, cmd, NULL_TERMINATED))
    {
	report_error_with_carot (database_cursor, rcd, cmd);
	return DB_FAILED;
    }

    if (bind_string_for_fetch (database_cursor,
	    1,		/* column number */
	    name,		/* program buffer */
	    sizeof(name),
	    SQLNPTR		/* fetch code status */
	))
    {
	return DB_FAILED;
    }

    if (bind_string_for_fetch (database_cursor,
	    2,		/* column number */
	    creator,		/* program buffer */
	    sizeof(creator),
	    SQLNPTR		/* fetch code status */
	))
    {
	return DB_FAILED;
    }

    if (bind_int_for_fetch (database_cursor,
            3,                  /* column number */
            &ncols,             /* program buffer */
            SQLNPTR             /* fetch code status */
        ))
    {
        return DB_FAILED;
    }

    if (bind_string_for_fetch (database_cursor,
	    4,		/* column number */
	    uniquerule,		/* program buffer */
	    sizeof(uniquerule),
	    SQLNPTR		/* fetch code status */
	))
    {
	return DB_FAILED;
    }

    if (rcd = sqlexe (database_cursor))
    {
	report_error (rcd, "list_indexes sqlexe() failure");
	return DB_FAILED;
    }

    for (n = 0; ; n++)
    {
	db_zero(name, sizeof(name));
	db_zero(creator, sizeof(creator));
	db_zero(uniquerule, sizeof(uniquerule));
	if (fetch (database_cursor, &eof))
	    return DB_FAILED;
	if (eof) break;
	if (list != NULL)
	{
	    idx = &list[n];
	    if(db_set_index_name (idx, name) != DB_OK)
		return DB_FAILED;
	    if(db_alloc_index_columns (idx, ncols) != DB_OK)
		return DB_FAILED;
	    if (*uniquerule == 'U')
		db_set_index_type_unique (idx);
	    else
		db_set_index_type_non_unique (idx);
	}
    }

    *count = n;
    return DB_OK;
}

static int
get_index_column_names (index)
    dbIndex *index;
{
    char cmd[1024];
    char name[256];
    int n, ncols;
    int eof;
    SQLTRCD rcd;

    sprintf (cmd,
       "select colname from sysadm.syskeys where ixname = '%s' order by colseq",
	db_get_index_name (index));

    if (rcd = sqlcom (database_cursor, cmd, NULL_TERMINATED))
    {
	report_error_with_carot (database_cursor, rcd, cmd);
	return DB_FAILED;
    }

    if (bind_string_for_fetch (database_cursor,
	    1,		/* column number */
	    name,		/* program buffer */
	    sizeof(name),
	    SQLNPTR		/* fetch code status */
	))
    {
	return DB_FAILED;
    }

    if (rcd = sqlexe (database_cursor))
    {
	report_error (rcd, "list_indexes sqlexe() failure");
	return DB_FAILED;
    }

    ncols = db_get_index_number_of_columns(index);

    eof = 0;
    for (n = 0; n < ncols; n++)
    {
	db_zero(name, sizeof(name));
	if (fetch (database_cursor, &eof))
	    return DB_FAILED;
	if (eof) break;
	db_set_index_column_name (index, n, name);
    }
    /* if !eof then there is a problem
     * I don't know what is the right thing to do
     * I am silently ignoring this case for now
     */

    return DB_OK;
}
