#include <sqlp.h>
#include <libpq-fe.h>

#define PG_MSG 1024		/* max length of message for report_error() */

#define PG_COL_NAME 32		/* maximum column name length. NB: see pg what */

#define PG_UNKNOWN   0
#define PG_CHAR   1
#define PG_INT    2
#define PG_DOUBLE 3
#define PG_DATE   4
#define PG_TIME   5

#define NAMELEN 1024

/* Definitions in:
 *   /usr/include/pgsql/server/catalog/pg_type.h
 */

#define BOOLOID                 16
#define BYTEAOID                17
#define CHAROID                 18
#define NAMEOID                 19
#define INT8OID                 20
#define INT2OID                 21
#define INT2VECTOROID   	22
#define INT4OID                 23
#define REGPROCOID              24
#define TEXTOID                 25
#define OIDOID                  26
#define TIDOID          	27
#define XIDOID 			28
#define CIDOID 			29
#define OIDVECTOROID    	30
#define POINTOID                600
#define LSEGOID                 601
#define PATHOID                 602
#define BOXOID                  603
#define POLYGONOID              604
#define LINEOID                 628
#define FLOAT4OID 		700
#define FLOAT8OID 		701
#define ABSTIMEOID              702
#define RELTIMEOID              703
#define TINTERVALOID    	704
#define UNKNOWNOID              705
#define CIRCLEOID               718
#define CASHOID 		790
#define INETOID 		869
#define CIDROID 		650
#define ACLITEMSIZE 		8
#define BPCHAROID               1042
#define VARCHAROID              1043
#define DATEOID                 1082
#define TIMEOID                 1083
#define TIMESTAMPOID    	1114
#define TIMESTAMPTZOID  	1184
#define INTERVALOID             1186
#define TIMETZOID               1266
#define BITOID   		1560
#define VARBITOID         	1562
#define NUMERICOID              1700
#define REFCURSOROID    	1790
#define POSTGISPOINTOID		17409 /* PostGIS */
#define POSTGISUNKNOWNOID	7405753 /* PostGIS, appears in geometry_column */

typedef struct
{
    char name[PG_COL_NAME];
    int type;
    int width;
}
COLUMN;

typedef union
{
    char *c;
    int i;
    double d;
    dbDateTime t;
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
char errMsg[PG_MSG];
PGconn *pg_conn;
#else
extern DATABASE db;
extern char *errMsg;
extern PGconn *pg_conn;
#endif
