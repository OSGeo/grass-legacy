#include "globals.h"


mycursor *
alloc_mycursor()
{
    mycursor *c;
    static int cursor_num = 0;

/* allocate the cursor */
    c = (mycursor *) db_malloc(sizeof(mycursor));
    if (c == NULL)
	return c;

/* tokenize it */
    c->token = db_new_token(c);
    if (c->token < 0)
    {
	free (c);
	c = NULL;
	return c;
    }

    c->database_cursor = 0;
    c->update_cursor = 0;
    c->nrows = 0;
    c->next_row = 0;

    sprintf (c->name, "C%d", ++cursor_num);
    db_init_string (&c->tableName);

/* return cursor */
    return c;
}

void
free_mycursor(c)
    mycursor *c;
{
    db_drop_token(c->token);
    db_free_string (&c->tableName);
    free(c);
}


/* make a new connection to the database with a new cursor */
mycursor *
open_mycursor ()
{
    mycursor *c;
    SQLTRCD rcd; /* return code */

    c = alloc_mycursor();
    if (c == NULL)
	return NULL;
    if(open_database (database_name, &c->database_cursor) != DB_OK)
	goto fail;


/* give a symbolic name to the 'database_cursor' */
    if (rcd = sqlscn (c->database_cursor, c->name, NULL_TERMINATED))
    {
	report_error (rcd, "sqlscn(): Unable to name a cursor");
	goto fail;
    }
    return c;
fail:
    close_mycursor(c);
    return NULL;
}

int
open_mycursor_update_cursor (c)
    mycursor *c;
{
    disconnect_from_database (&c->update_cursor);
    return open_database (database_name, &c->update_cursor);
}

void
close_mycursor (c)
    mycursor *c;
{
    disconnect_from_database (&c->database_cursor);
    disconnect_from_database (&c->update_cursor);
    free_mycursor(c);
}
