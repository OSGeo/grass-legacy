#include <dbmi.h>
#include "odbc.h"
#include "globals.h"
#include "proto.h"

db_driver_execute_immediate (sql)
    dbString *sql;
{
    char *s, msg[OD_MSG], emsg[DB_MSG];
    cursor *c;
    SQLRETURN   ret;
    SQLINTEGER   err;     

    s = db_get_string (sql);

    /* allocate cursor */
    c = alloc_cursor();
        if (c == NULL)
            return DB_FAILED;   
		    
    ret = SQLExecDirect(c->stmt,s,SQL_NTS);
    if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
    {
	SQLGetDiagRec(SQL_HANDLE_STMT, c->stmt, 1, NULL, &err, msg, sizeof(msg), NULL);
	snprintf(emsg, sizeof(emsg), "SQLExecDirect():\n%s\n%s (%d)\n",s,msg,err);
	report_error(emsg);
	return DB_FAILED;
    }   

    free_cursor (c);

    return DB_OK;
}
