/* error.c */
void init_error ( void );
void append_error ( char *msg );
void report_error ( void );

/* cursor.c */
cursor * alloc_cursor ();
void free_cursor ( cursor *c );

/* describe.c*/
int describe_table( PGresult *res, dbTable **table, cursor *c );
int get_column_info ( PGresult *res, int col, int *pgtype, int *type, int *size);
int get_pg_type ( int pg_type );

/* parse.c */
int parse_conn ( char *str, PGCONN *pgconn );
