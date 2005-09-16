#include <sqlite3.h>

/* cursors */
typedef struct _cursor {
    //PGresult *res;
    sqlite3_stmt *statement;
    int nrows;   /* number of rows in query result, -1 if unknown */
    int row;               /* current row */
    dbToken token;
    int type;              /* type of cursor: SELECT, UPDATE, INSERT */
    int *kcols;             /* indexes of known (type) columns */ 
    int nkcols;             /* number of known columns */

} cursor;  


#ifdef MAIN
    //PGconn *pg_conn; /* Database connection */
    sqlite3 *sqlite;
    dbString *errMsg = NULL; /* error message */
#else
    extern sqlite3 *sqlite;
    extern dbString *errMsg;
#endif 


