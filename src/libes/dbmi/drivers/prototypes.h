#ifdef __STDC__
# define	P(s) s
#else
# define P(s) ()
#endif

int   db_driver_add_column      P((dbString *tableName , dbColumn *column ));
int   db_driver_bind_update     P((dbCursor *cursor ));
int   db_driver_close_cursor    P((dbCursor *cursor ));
int   db_driver_close_database  P((void ));
int   db_driver_create_database P((dbHandle *handle ));
int   db_driver_create_index    P((dbIndex *index ));
int   db_driver_create_table    P((dbTable *table ));
int   db_driver_delete          P((dbCursor *cursor ));
int   db_driver_delete_database P((dbHandle *handle ));
int   db_driver_describe_table  P((dbString *name , dbTable *table ));
int   db_driver_drop_column     P((dbString *tableName , dbString *columnName ));
int   db_driver_drop_index      P((dbString *name ));
int   db_driver_drop_table      P((dbString *name ));
int   db_driver_execute_immediate P((dbString *SQLstatement ));
int   db_driver_fetch           P((dbCursor *cursor , int position , int *more ));
int   db_driver_find_database   P((dbHandle *handle , int *found ));
int   db_driver_finish          P((void ));
int   db_driver_init            P((int argc , char *argv []));
int   db_driver_insert          P((dbCursor *cursor ));
int   db_driver_list_databases  P((dbString *path , int npaths , dbHandle **handles , int *num ));
int   db_driver_list_indexes    P((dbString *tableName , dbIndex **indexes , int *count ));
int   db_driver_list_tables     P((dbString **names , int *count , int system ));
int   db_driver_open_database   P((dbHandle *handle ));
int   db_driver_open_insert_cursor P((dbCursor *cursor ));
int   db_driver_open_select_cursor P((dbString *select , dbCursor *cursor , int *mode ));
int   db_driver_open_update_cursor P((dbString *name , dbString *select , dbCursor *cursor , int *mode ));
int   db_driver_update          P((dbCursor *cursor ));

#undef P
