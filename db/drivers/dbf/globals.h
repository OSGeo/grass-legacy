#include <sqlp.h>

#define DBF_MSG 2000  /* max length of message for report_error() */

#define DBF_COL_NAME 20 /* maximum column name (in fact shouldn't be > 10) */

#define DBF_CHAR   1
#define DBF_INT    2
#define DBF_DOUBLE 3

/* Driver mode */
#define DBF_MODE_DBF 0
#define DBF_MODE_SHP 1

/* Virtual column name for shp driver */
#define DBF_FID_NAME "shp_fid"

typedef struct {
    char name[DBF_COL_NAME]; 
    int  type; 
    int  width; 
    int  decimals; 
} COLUMN;

typedef union {
    char  *c; 
    int    i; 
    double d; 
} VALUE;

typedef struct {
    int   alive; 
    VALUE *values;
} ROW;

typedef struct {
    char  name[1024]; /* table name (without .dbf) */
    char  file[1024]; /* full path to file (including .dbf) */
    int   read;       /* TRUE if user has read access to the file */  
    int   write;      /* TRUE if user has write access to the file */
    int   alive; 
    int   described;  /* columns definitions were loaded to heads */
    int   loaded;     /* data were loaded to rows */
    int   updated;     
    COLUMN *cols;
    ROW  *rows;
    int   acols;      /* allocated columns */
    int   ncols;      /* number of columns */
    int   arows;      /* allocated rows */
    int   nrows;      /* number of rows */
} TABLE;

typedef struct {
    char   name[1024]; /* db name = full path to db dir */
    TABLE *tables;
    int    atables;    /* allocated space for tables */
    int    ntables;    /* number of tables */
} DATABASE;

/* cursors */
typedef struct {
    SQLPSTMT *st;
    int table;   /* table */
    int *set;    /* array of indexes to table for selected rows */
    int nrows;   /* number of rows in set */
    int cur;     /* position of cursor */
    int *cols;   /* array of indexes of selected columns */
    int ncols;
    dbToken token;
    int type;    /* type of cursor: SELECT, UPDATE, INSERT */
} cursor;

#ifdef MAIN
    DATABASE db;
    char     errMsg[DBF_MSG];
    int    drv_mode;
#else
    extern DATABASE db;
    extern char errMsg;
    extern int drv_mode;
#endif 

