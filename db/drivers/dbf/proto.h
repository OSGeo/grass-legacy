void report_error ( char * );
int save_string (VALUE *, char *);

cursor * alloc_cursor ();
void free_cursor ( cursor * );

int add_column ( int table, int type,  char *name , int width, int decimals );
int find_column ( int, char * );


int add_table ( char *);
int execute ( char *, cursor *);
int free_table ( int );
int find_table ( char * );
int load_table_head( int );
int load_table( int );
int save_table( int );
int describe_table( int, int *, int, dbTable ** );


