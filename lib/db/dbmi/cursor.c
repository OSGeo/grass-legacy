#include "dbmi.h"

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_init_cursor (cursor)
    dbCursor *cursor;
{
    cursor->driver = NULL;
    cursor->token = -1;
    cursor->type = 0;
    cursor->mode = 0;
    cursor->table = NULL;
    cursor->column_flags = NULL;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
db_alloc_cursor_table (cursor, ncols)
    dbCursor *cursor;
    int ncols;
{
    cursor->table = db_alloc_table (ncols);
    if (cursor->table == NULL)
	return db_get_error_code();
    return DB_OK;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_free_cursor (cursor)
    dbCursor *cursor;
{
    if (cursor->table)
	db_free_table (cursor->table);
    if (cursor->column_flags)
	db_free_cursor_column_flags (cursor);
    db_init_cursor(cursor);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
dbTable *
db_get_cursor_table(cursor)
    dbCursor *cursor;
{
    return cursor->table;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_table(cursor, table)
    dbCursor *cursor;
    dbTable *table;
{
    cursor->table = table;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
dbToken
db_get_cursor_token(cursor)
    dbCursor *cursor;
{
    return cursor->token;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_token(cursor, token)
    dbCursor *cursor;
    dbToken token;
{
    cursor->token = token;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_type_readonly (cursor)
    dbCursor *cursor;
{
    cursor->type = DB_READONLY;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_type_update (cursor)
    dbCursor *cursor;
{
    cursor->type = DB_UPDATE;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_type_insert (cursor)
    dbCursor *cursor;
{
    cursor->type = DB_INSERT;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_type_fetch(cursor)
    dbCursor *cursor;
{
    return (cursor->type == DB_READONLY || cursor->type == DB_UPDATE);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_type_update(cursor)
    dbCursor *cursor;
{
    return (cursor->type == DB_UPDATE);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_type_insert(cursor)
    dbCursor *cursor;
{
    return (cursor->type == DB_INSERT);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_mode(cursor,mode)
    dbCursor *cursor;
    int mode;
{
    cursor->mode = mode;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_mode_scroll(cursor)
    dbCursor *cursor;
{
    cursor->mode |= DB_SCROLL;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_unset_cursor_mode_scroll(cursor)
    dbCursor *cursor;
{
    cursor->mode &= ~DB_SCROLL;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_unset_cursor_mode(cursor)
    dbCursor *cursor;
{
    cursor->mode = 0;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_mode_insensitive(cursor)
    dbCursor *cursor;
{
    cursor->mode |= DB_INSENSITIVE;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_unset_cursor_mode_insensitive(cursor)
    dbCursor *cursor;
{
    cursor->mode &= ~DB_INSENSITIVE;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_mode_scroll(cursor)
    dbCursor *cursor;
{
    return (cursor->mode & DB_SCROLL);
}


/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_mode_insensitive(cursor)
    dbCursor *cursor;
{
    return (cursor->mode & DB_INSENSITIVE);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_alloc_cursor_column_flags (cursor)
    dbCursor *cursor;
{
    int ncols;
    int col;

    ncols = db_get_cursor_number_of_columns (cursor);
    cursor->column_flags = (short *) db_calloc (ncols, sizeof(short));
    if (cursor->column_flags == NULL)
	return db_get_error_code();
    for (col = 0; col < ncols; col++)
	db_unset_cursor_column_flag (cursor, col);
    return DB_OK ;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_free_cursor_column_flags (cursor)
    dbCursor *cursor;
{
    if(cursor->column_flags)
	free(cursor->column_flags);
    cursor->column_flags = NULL;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_column_for_update (cursor, col)
    dbCursor *cursor;
    int col;
{
    db_set_cursor_column_flag (cursor, col);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_unset_cursor_column_for_update (cursor, col)
    dbCursor *cursor;
    int col;
{
    db_unset_cursor_column_flag (cursor, col);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_column_for_update (cursor, col)
    dbCursor *cursor;
    int col;
{
    return db_test_cursor_column_flag (cursor, col);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_any_column_for_update (cursor)
    dbCursor *cursor;
{
    return db_test_cursor_any_column_flag (cursor);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_cursor_column_flag (cursor, col)
    dbCursor *cursor;
    int col;
{
    if (cursor->column_flags)
	cursor->column_flags[col] = 1;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_unset_cursor_column_flag (cursor, col)
    dbCursor *cursor;
    int col;
{
    if (cursor->column_flags)
	cursor->column_flags[col] = 0;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_test_cursor_column_flag (cursor, col)
    dbCursor *cursor;
    int col;
{
    return cursor->column_flags && cursor->column_flags[col] ? 1 : 0 ;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_get_cursor_number_of_columns (cursor)
    dbCursor *cursor;
{
    dbTable *table;

    table = db_get_cursor_table (cursor);
    if (table)
	return db_get_table_number_of_columns(table);
    return 0;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
/* is any cursor column flag set? */
int
db_test_cursor_any_column_flag (cursor)
    dbCursor *cursor;
{
    int ncols, col;

    ncols = db_get_cursor_number_of_columns(cursor);
    for (col = 0; col < ncols; col++)
	if (db_test_cursor_column_flag(cursor, col))
	    return 1;
    return 0;
}
