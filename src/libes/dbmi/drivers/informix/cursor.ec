#include <dbmi.h>
#include "globals.h"

cursor *
make_cursor()
{
    cursor *c;

/* allocate the cursor */
    c = (cursor *) db_malloc(sizeof(cursor));
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

/* build cursor, statement, and descriptor names */
    sprintf (c->cursor_name,     "c%d", c->token);
    sprintf (c->descriptor_name, "d%d", c->token);
    sprintf (c->statement_name,  "s%d", c->token);
    sprintf (c->update_name,  "u%d", c->token);
    sprintf (c->update_descriptor_name,  "ud%d", c->token);
    sprintf (c->delete_name,  "r%d", c->token);  /* r for remove */

/* return cursor */
    return c;
}

void
free_cursor(c)
    cursor *c;
{
    db_drop_token(c->token);
    free(c);
}
