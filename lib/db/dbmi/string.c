#include "dbmi.h"

void
db_init_string (x)
    dbString *x;
{
    x->string = "";
    x->nalloc = 0;
}
/* db_set_string(dbString *x, char *s, int copy)
 *  inserts 's' into 'x'
 *   if 'copy' is true, then memory is allocated to copy into
 *   else 'x' is made to point to 's'
 * returns DB_OK or DB_MEMORY_ERR
 */
static int set_string();

db_set_string (x, s)
    dbString *x;
    char *s;
{
    return set_string (x, s, 1);
}

db_set_string_no_copy (x, s)
    dbString *x;
    char *s;
{
    return set_string (x, s, 0);
}

unsigned int
db_sizeof_string (x)
    dbString *x;
{
    if (x->nalloc < 0) return 0;
    return (unsigned int) x->nalloc;
}

db_zero_string (x)
    dbString *x;
{
    db_zero ((void *)db_get_string(x), db_sizeof_string(x));
}

static int
set_string (x, s, copy)
    dbString *x;
    char *s;
{
    int len;
    int stat;

    if (s == NULL)
    {
	s = "";
	copy = 1;
    }

    len = strlen(s)+1;

    if (copy)
    {
	stat = db_enlarge_string (x, len);
	if (stat != DB_OK)
	    return stat;
	strcpy (x->string, s);
    }
    else
    {
	db_free_string(x);
	x->string = s;
	x->nalloc = -1;
    }
    return DB_OK;
}

db_enlarge_string (x, len)
    dbString *x;
    int len;
{
    if (x->nalloc < len)
    {
	if (x->nalloc <= 0)
	    x->string = db_store("");
	x->string = db_realloc ((void *)x->string, len);
	if (x->string == NULL)
	    return DB_MEMORY_ERR;
	x->nalloc = len;
    }
    return DB_OK;
}

char *
db_get_string(x)
    dbString *x;
{
    return x->string;
}

void
db_free_string(x)
    dbString *x;
{	
    if (x->nalloc > 0)
	free(x->string);
    db_init_string (x);
}

void
db_free_string_array (a, n)
    dbString *a;
{
    int i;

    if (a)
    {
	for (i = 0; i<n; i++)
	    db_free_string(&a[i]);
	free (a);
    }
}

dbString *
db_alloc_string_array (count)
    int count;
{
    int i;
    dbString *a;

    if (count < 0) count = 0;
    a = (dbString *) db_calloc (count, sizeof(dbString));
    if (a)
    {
	for (i = 0; i < count; i++)
	    db_init_string(&a[i]);
    }
    return a;
}

int
db_append_string (x, s)
    dbString *x;
    char *s;
{
    int len;
    int stat;

    len = strlen (db_get_string(x)) + strlen(s) + 1;
    stat = db_enlarge_string (x, len);
    if (stat != DB_OK)
	return stat;
    strcat (db_get_string(x), s);
    return DB_OK;
}

db_copy_string (dst, src)
    dbString *dst, *src;
{
    return db_set_string (dst, db_get_string(src));
}
