#include "dbmi.h"

static dbDriverState state;

void
db__init_driver_state()
{
    db_zero((void *)&state, sizeof(state));
}

dbDriverState *
db__get_driver_state()
{
    return &state;
}

db__test_database_open ()
{
    return state.open ? 1 : 0 ;
}

void
db__mark_database_open (dbname, dbpath)
    char *dbname;
    char *dbpath;
{
    state.dbname = dbname;
    state.dbpath = dbpath;
    state.open = 1;
}

void
db__mark_database_closed ()
{
    free(state.dbname);
    free(state.dbpath);
    state.open = 0;
}

void
db__add_cursor_to_driver_state(cursor)
    dbCursor *cursor;
{
    dbCursor **list;
    int i;

/* find an empty slot in the cursor list */
    list = state.cursor_list;
    for (i = 0; i < state.ncursors; i++)
	if (list[i] == NULL)
	    break;

/* if not found, extend list */
    if (i >= state.ncursors)
    {
	list = (dbCursor **) db_realloc ((void *)list, (i+1) * sizeof(dbCursor *));
	if (list == NULL)
	    return;
	state.cursor_list = list;
	state.ncursors    = i+1;
    }

/* add it in */
    list[i] = cursor;
}

void
db__drop_cursor_from_driver_state(cursor)
    dbCursor *cursor;
{
    int i;

    for (i = 0; i < state.ncursors; i++)
	if (state.cursor_list[i] == cursor)
	    state.cursor_list[i] = NULL;
}

void
db__close_all_cursors()
{
    int i;

    for (i = 0; i < state.ncursors; i++)
	if (state.cursor_list[i])
	    db_driver_close_cursor (state.cursor_list[i]);
    
    if (state.cursor_list)
	free (state.cursor_list);
    
    state.ncursors = 0;
    state.cursor_list = NULL;
}
