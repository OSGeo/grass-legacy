#ifdef __STDC__
# define	P(s) s
#else
# define P(s) ()
#endif


/* bind.c */
SQLTRCD bind_int_for_fetch P((SQLTCUR cursor , int bindcol , int *x , SQLTFSC *fc ));
SQLTRCD bind_string_for_fetch P((SQLTCUR cursor , int bindcol , char *string , int size , SQLTFSC *fc ));
int bind_for_fetch_failure P((SQLTRCD rcd ));
SQLTRCD bind_column_for_insert P((SQLTCUR cursor , int bindvar , dbColumn *column ));
SQLTRCD bind_column_for_update P((SQLTCUR cursor , int bindvar , dbColumn *column ));

/* case.c */
void string_to_lowercase P((char *s ));
void string_to_uppercase P((char *s ));
void char_to_lowercase P((char *s ));
void char_to_uppercase P((char *s ));

/* closecur.c */
int db_driver_close_cursor P((dbCursor *dbc ));

/* closedb.c */
int db_driver_close_database P((void ));
void close_database P((void ));
void disconnect_from_database P((SQLTCUR *cur ));

/* column.c */
int db_driver_add_column P((dbString *tableName , dbColumn *column ));
int db_driver_drop_column P((dbString *tableName , dbString *columnName ));
int build_column_type_info P((dbColumn *column , dbString *cmd ));
int build_column_list P((void *object , dbString *list , int ncols , char *(*colname )()));
int list_columns P((char *tableName , dbString **tlist , int *tcount ));

/* compilecur.c */
int compile_cursor P((mycursor *c , dbString *stmt , dbCursor *dbc ));

/* createdb.c */
int db_driver_create_database P((dbHandle *handle ));

/* current_dt.c */
int current_datetime P((char *dt , SQLTNML *len ));

/* dbname.c */
void decompose_database_name P((char *fullname , char *dbname , char *username , char *passwd ));
void make_database_name P((char *fullname , char *name ));
int get_default_database P((char *dbname ));
void get_database_user P((char *user ));

/* defaults.c */
void get_table_default_values P((dbTable *table ));
int column_has_default P((char *tbname , char *tbcreator , char *colname ));
void set_column_default P((dbColumn *column ));

/* delete.c */
int db_driver_delete P((dbCursor *dbc ));

/* deletedb.c */
int db_driver_delete_database P((dbHandle *handle ));

/* describe.c */
int db_driver_describe_table P((dbString *table_name , dbTable **table ));
int describe_table P((char *tableName , dbTable **table ));
int describe P((SQLTCUR cursor , dbTable **table ));
int describe_column P((SQLTCUR cursor , dbColumn *column , int col ));

/* die.c */
int die P((void ));

/* error.c */
void report_error P((SQLTRCD rcd , char *err ));
void report_error_with_carot P((SQLTCUR cur , SQLTRCD rcd , char *text ));

/* execute.c */
int db_driver_execute_immediate P((dbString *sql_statement ));
int execute_immediate P((dbString *sql_statement ));

/* fetch.c */
int db_driver_fetch P((dbCursor *dbc , int position , int *more ));
int fetch_eof P((int rcd ));
int fetch_error P((int rcd ));
int is_datetime P((dbColumn *column ));
int convert_datetime P((dbColumn *column ));
SQLTRCD fetch P((SQLTCUR cursor , int *eof ));

/* finddb.c */
int db_driver_find_database P((dbHandle *handle , int *found ));

/* finish.c */
int db_driver_finish P((void ));

/* index.c */
int db_driver_create_index P((dbIndex *index ));
int db_driver_drop_index P((dbString *name ));
int create_index P((dbIndex *index ));
int drop_index P((char *name ));
char *get_index_name P((dbIndex *index ));
char *make_index_name P((void ));

/* init.c */
int db_driver_init P((int argc , char *argv []));

/* insert.c */
int db_driver_open_insert_cursor P((dbCursor *dbc ));
int compile_insert_cursor P((mycursor *c , dbCursor *dbc ));
int db_driver_insert P((dbCursor *dbc ));

/* isnull.c */
int is_null_value P((SQLTFSC fc ));

/* listdb.c */
int db_driver_list_databases P((dbString *dbpath , int npaths , dbHandle **dblist , int *dbcount ));
int list_dbnames P((char *server , dbHandle *list , int *count ));

/* listidx.c */
int db_driver_list_indexes P((dbString *tableName , dbIndex **list , int *count ));
int list_indexes P((char *tableName , dbIndex **idxlist , int *idxcount ));

/* listtab.c */
int db_driver_list_tables P((dbString **tlist , int *tcount , int system ));

/* main.c */
int main P((int argc , char *argv []));

/* mycursor.c */
mycursor *alloc_mycursor P((void ));
void free_mycursor P((mycursor *c ));
mycursor *open_mycursor P((void ));
int open_mycursor_update_cursor P((mycursor *c ));
void close_mycursor P((mycursor *c ));

/* opendb.c */
int db_driver_open_database P((dbHandle *handle ));
int open_database P((char *name , SQLTCUR *cursor ));
int database_is_open P((void ));
int check_for_open_database P((void ));

/* passwd.c */
void get_database_password P((char *dbname , char *username , char *passwd ));
void get_server_password P((char *server , char *passwd ));

/* privs.c */
void get_table_privs P((dbTable *table ));
void grant_all_table_privs P((dbTable *table ));
void deny_all_table_privs P((dbTable *table ));
int user_is_dba P((char *user ));

/* quotes.c */
void dup_single_quotes P((char *src , char *dst ));

/* remarks.c */
int add_remarks_to_table P((char *tableName , char *remarks ));
int add_remarks_to_column P((char *tableName , char *columnName , char *remarks ));
char *get_remarks_for_table P((char *tableName ));
char *get_remarks_for_column P((char *tableName , char *columnName ));

/* select.c */
int db_driver_open_select_cursor P((dbString *select_string , dbCursor *dbc , int mode ));

/* server.c */
int connect_to_server P((char *server , SQLTSVH *server_handle ));
void disconnect_from_server P((SQLTSVH *svh ));
char *get_server_name_from_list P((int n , dbString *list , int count ));
void decompose_server_name P((char *fullname , char *server , char *passwd ));

/* sqlbase.c */
char *get_sqlbase_directory P((void ));
char *get_sqlbase_server P((void ));
int chdir_sqlbase P((void ));

/* strcmp.c */
int nocase_strcmp P((char *a , char *b ));

/* table.c */
int db_driver_create_table P((dbTable *table ));
int db_driver_drop_table P((dbString *name ));

/* tbname.c */
void decompose_tablename P((char *string , char *creator , char *name ));
void compose_tablename P((char *string , char *creator , char *name ));

/* update.c */
int db_driver_open_update_cursor P((dbString *table_name , dbString *select_string , dbCursor *dbc , int mode ));
int db_driver_bind_update P((dbCursor *dbc ));
int db_driver_update P((dbCursor *dbc ));

/* view.c */
int is_view P((char *name ));

/* whoami.c */
char *whoami P((void ));

#undef P
