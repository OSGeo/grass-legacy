#include "dbmi.h"

static char *path = NULL;
static int ndirs = 0;
extern char *db_store();

/* get DBPATH, replace all ':' with NULL
 * return both the path (with NULLs embedded) and number of dirs in path
 *  e.g. DBPATH = /x/y:/a/b
 *  return path="/x/y\0/a/b\0" and *n = 2
 */
char *
get_dbpath(n)
    int *n;
{
    char *x;
    char *getenv();
    int more;

    *n = 0;
    if (path == NULL) {
      x = getenv("DBPATH");
      if (x == NULL) return x;

      path = db_store(x);
      if (path == NULL) return path;
      x = path;
      while (*x) {
	    if (*x == ':') {
          more = 0;
          *x = 0;
          ndirs++;
	    } else more = 1;
	    x++;
	  }
      ndirs += more;
    }

    *n = ndirs;
    return path;
}

/* concat all paths into one "string" with embedded NULLs between the paths */

char *
build_dbpath(path, ndirs)
    dbString *path;
    int ndirs;
{
    int len, i;
    char *dbpath, *p;

    len = 0;
    for (i = 0; i < ndirs; i++)
	len += strlen (db_get_string (&path[i])) + 1;
    
    p = dbpath = db_malloc(len);
    for (i = 0; i < ndirs; i++)
    {
	strcpy (p, db_get_string(&path[i]));
	p += strlen(p) + 1;
    }
    return dbpath;
}
