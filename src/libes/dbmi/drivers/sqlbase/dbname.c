#include "globals.h"

#define SLASH '/'


void
decompose_database_name (fullname, dbname, username, passwd)
    char *fullname;
    char *dbname;
    char *username;
    char *passwd;
{
    char *a, *b;

/* full database names have the format
 *	database/username/password
 *
 * decompose the name into its parts
 * Resolve all missing parts (except the passwd)
 *   name -> name/whoami
 *   name/user -> name/user
 *   name//passwd -> name/whoami/passwd
 *
 */
    *dbname = *username = *passwd = 0;
    a = fullname;

    b = dbname;
    while (*a && *a != SLASH)
	*b++ = *a++;
    *b = 0;
    if (*a == SLASH)
	a++;

    b = username;
    while (*a && *a != SLASH)
	*b++ = *a++;
    *b = 0;
    if (*a == SLASH)
	a++;

    b = passwd;
    while (*a && *a != SLASH)
	*b++ = *a++;
    *b = 0;
    if (*a == SLASH)
	a++;

/* if no database name, use the default database name */
    if (*dbname == 0)
	get_default_database(dbname);

/* if no user, insert whoami() */
    if (*username == 0)
	strcpy (username, whoami());
}

void
make_database_name (fullname, name)
    char *fullname;
    char *name;
{
    char dbname[256];
    char username[256];
    char passwd[256];

/* full database names have the format
 *	database/username/password
 */
    decompose_database_name (name, dbname, username, passwd);

/* now get the password for this database for this user */
    if (*passwd == 0)
	get_database_password (dbname, username, passwd);

    sprintf (fullname, "%s/%s%s%s", dbname, username, *passwd?"/":"", passwd);
}

get_default_database (dbname)
    char *dbname;
{
    SQLTDAL len;
    sqlget(0, SQLPDDB, dbname, &len);
}

void
get_database_user (user)
    char *user;
{
    char fullname[256];
    char dbname[256];
    char passwd[256];

    make_database_name (fullname, database_name);
    decompose_database_name (fullname, dbname, user, passwd);

    db_Cstring_to_uppercase (user);
}
