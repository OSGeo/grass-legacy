/* header types for points file */
#define PNT_HEAD_NO 0 /* no header */
#define PNT_HEAD_NAMES 1 /* only names */
#define PNT_HEAD_TYPES 2 /* names + types (sql stype) */

int points_analyse ( FILE *ascii_in, FILE *ascii, char *fs, int head_type,
                     int *rowlength, int *ncolumns, int *minncolumns,
                     int **column_type, int **column_length );

int points_to_bin( FILE *ascii, int rowlen, struct Map_info *Map, dbDriver *driver, char *table,
                   char *fs, int head_type,
                   int ncols, int *coltype,
                   int xcol, int ycol, int zcol, int catcol );

int read_head ( FILE * dascii, struct Map_info *Map );

int asc_to_bin( FILE *ascii, struct Map_info *Map);

