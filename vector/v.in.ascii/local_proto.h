#define FORMAT_POINT 0
#define FORMAT_ALL   1

int points_analyse ( FILE *ascii_in, FILE *ascii, char *fs, int *rowlength,
		     int *ncolumns, int *minncolumns, int **column_type,
		     int **column_length, int skip_lines );

int points_to_bin( FILE *ascii, int rowlen, struct Map_info *Map, dbDriver *driver,
		   char *table, char *fs, int ncols, int *coltype,
		   int xcol, int ycol, int zcol, int catcol, int skip_lines );

int read_head ( FILE * dascii, struct Map_info *Map );

int asc_to_bin( FILE *ascii, struct Map_info *Map);

