#include <sqlp.h>
#include <mysql.h>
#include <mysqld_error.h>

#define MYSQL_MSG 1024		/* max length of message for report_error() */

#define MYSQL_COL_NAME 64		/* maximum column name length. NB: see mysql what*/

#define MYSQL_UNKNOWN 0
#define MYSQL_CHAR   1
#define MYSQL_INT    2
#define MYSQL_DOUBLE 3

#define NAMELEN 1024


typedef struct
{
    char name[MYSQL_COL_NAME];
    int type;
    int width;
    int decimals; 
}
COLUMN;

typedef union
{
    char *c;
    int i;
    double d;
}
VALUE;

typedef struct
{
    int alive;
    VALUE *values;
}
ROW;

typedef struct
{
    char name[NAMELEN];		/* table name */
    int read;			/* TRUE if user has read access to the file */
    int write;			/* TRUE if user has write access to the file */
    int alive;
    int described;		/* columns definitions were loaded to heads */
    int loaded;			/* data were loaded to rows */
    int updated;
    COLUMN *cols;
    ROW *rows;
    int acols;			/* allocated columns */
    int ncols;			/* number of columns */
    int arows;			/* allocated rows */
    int nrows;			/* number of rows */
}
TABLE;

typedef struct
{
    char name[NAMELEN];		/* db name = full path to db dir */
    TABLE *tables;
    int atables;		/* allocated space for tables */
    int ntables;		/* number of tables */
}
DATABASE;

/* cursors */
typedef struct
{
    SQLPSTMT *st;
    int table;			/* table */
    int *set;			/* array of indexes to table for selected rows */
    int nrows;			/* number of rows in set */
    int cur;			/* position of cursor */
    int *cols;			/* array of indexes of selected columns */
    int ncols;
    dbToken token;
    int type;			/* type of cursor: SELECT, UPDATE, INSERT */
}
cursor;

#ifdef MAIN
DATABASE db;
char errMsg[MYSQL_MSG];
MYSQL mysql_conn;
#else
extern DATABASE db;
extern char errMsg;
extern MYSQL mysql_conn;
#endif
