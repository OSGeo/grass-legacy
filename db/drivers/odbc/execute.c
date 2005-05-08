#include <dbmi.h>
#include <stdio.h>
#include "gis.h"
#include "odbc.h"
#include "globals.h"
#include "proto.h"


int
db__driver_execute_immediate (dbString *sql)
{
    char *s, msg[OD_MSG];
    char *emsg;
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
	G_asprintf(&emsg, "SQLExecDirect():\n%s\n%s (%d)\n", s, msg, err);
	report_error(emsg);
        G_free(emsg);

	return DB_FAILED;
    }   

    free_cursor (c);

    return DB_OK;
}
