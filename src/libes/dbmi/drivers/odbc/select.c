#include <dbmi.h>
#include "odbc.h"
#include "globals.h"
#include "proto.h"
#include <stdio.h>

int
db_driver_open_select_cursor(sel, dbc, mode)
    dbString *sel;
    dbCursor *dbc;
    int mode;
{
    cursor      *c;
    SQLRETURN   ret;
    SQLINTEGER  err;
    char        *sql, msg[OD_MSG], emsg[DB_MSG];      
    dbTable     *table;   

    /* allocate cursor */
    c = alloc_cursor();
    if (c == NULL)
	return DB_FAILED;

    db_set_cursor_mode(dbc,mode);
    db_set_cursor_type_readonly(dbc);

    sql = db_get_string(sel);

    ret = SQLExecDirect(c->stmt,sql,SQL_NTS);
    if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
    {
	SQLGetDiagRec(SQL_HANDLE_STMT, c->stmt,1, NULL, &err, msg, sizeof(msg), NULL);
	snprintf(emsg, sizeof(emsg),"SQLExecDirect():\n%s\n%s (%d)\n", sql, msg, err);
	report_error(emsg);  
	return DB_FAILED;
    }  

    describe_table(c->stmt, &table);
    
    db_set_cursor_table(dbc, table);
   
    /* record table with dbCursor */
    db_set_cursor_table(dbc, table);         

    /* set dbCursor's token for my cursor */
    db_set_cursor_token(dbc, c->token);

    return DB_OK;
}
