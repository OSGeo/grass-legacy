#include <stdlib.h>
#include <dbmi.h>
#include "sql.h"

#define SQLBASE_DIR    "SQLBASE"
#define SQLBASE_SERVER "SQLBASE_SERVER"
#define NULL_TERMINATED 0

/* cursors */
typedef struct _mycursor {
  SQLTCUR database_cursor;
  SQLTCUR update_cursor;
  dbToken token;
  int type;             /* type of cursor: SELECT, UPDATE, INSERT */
  SQLTROW next_row;	/* next row to be fetched */
  SQLTROW nrows;	/* number of rows in a select */
  char name[10];	/* symbolic name for the cursor */
  dbString tableName;
} mycursor;

#ifndef GLOBAL
# define GLOBAL extern
#else
# define GLOBAL
#endif

GLOBAL SQLTCUR database_cursor;
GLOBAL char *database_name;
GLOBAL SQLTSVH server_handle;
GLOBAL char *server_name;

GLOBAL FILE *debug_fd;

#include "proto.h"
