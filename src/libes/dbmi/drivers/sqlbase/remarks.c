#include "globals.h"

#define MAX_REMARK 254


static int
add_remarks (type, name, remarks)
    char *type;
    char *name;
    char *remarks;
{
    int stat;
    char rem1[MAX_REMARK+1];
    char rem2[2*MAX_REMARK+1];
    dbString cmd;

    strncpy (rem1, remarks, MAX_REMARK);
    rem1[MAX_REMARK] = 0;
    dup_single_quotes (rem1, rem2);
    db_init_string (&cmd);
    db_append_string (&cmd, "COMMENT ON ");
    db_append_string (&cmd, type);
    db_append_string (&cmd, " ");
    db_append_string (&cmd, name);
    db_append_string (&cmd, " IS '");
    db_append_string (&cmd, rem2);
    db_append_string (&cmd, "'");

    stat = execute_immediate (&cmd);
    db_free_string (&cmd);
    return stat;
}

int
add_remarks_to_table (tableName, remarks)
    char *tableName;
    char *remarks;
{
    return add_remarks ("TABLE", tableName, remarks);
}

int
add_remarks_to_column (tableName, columnName, remarks)
    char *tableName;
    char *columnName;
    char *remarks;
{
    char name[1024];

    sprintf (name, "%s.%s", tableName, columnName);
    return add_remarks ("COLUMN", name, remarks);
}

static int
fetch_one_string (cmd, result, len)
    char *cmd;
    char *result;
    int len;
{
    SQLTRCD rcd; /* return code */

    if(sqlcom (database_cursor, cmd, NULL_TERMINATED))
	return 0;

    db_zero(result, len);
    if(bind_string_for_fetch (database_cursor,
	    1,		/* column number */
	    result,
	    len,
	    SQLNPTR	/* fetch code status */
    ))
    {
	return 0;
    };
    if(sqlexe (database_cursor))
	return 0;
    rcd = sqlfet (database_cursor);
    if(fetch_error(rcd) || fetch_eof(rcd))
	return 0;
    return 1;
}

char *
get_remarks_for_table (tableName)
    char *tableName;
{
    char cmd[1024];
    char tbname[256], tbcreator[256];
    static char remarks[2*MAX_REMARK];

    decompose_tablename(tableName, tbcreator, tbname);

    sprintf (cmd,
      "select remarks from sysadm.systables where name ='%s' and creator = '%s'",
       tbname, tbcreator);

    if(!fetch_one_string (cmd, remarks, sizeof(remarks)))
	*remarks = 0;
    return remarks;
}

char *
get_remarks_for_column (tableName, columnName)
    char *tableName;
    char *columnName;
{
    char cmd[1024];
    char tbname[256], tbcreator[256];
    static char remarks[2*MAX_REMARK];

    decompose_tablename(tableName, tbcreator, tbname);
    sprintf (cmd,
      "select remarks from sysadm.syscolumns where tbname ='%s' and tbcreator = '%s' and name = '%s'",
       tbname, tbcreator, columnName);

    if(!fetch_one_string (cmd, remarks, sizeof(remarks)))
	*remarks = 0;
    return remarks;
}
