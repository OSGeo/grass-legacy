/* This routine gets a password from $HOME/.sqlbase/db.passwd
 * This file has the format
 *
 *  dbname//password          # for the current user
 *  dbname/user/password      # for the specified user
 *
 * No comments are allowed in the file - the # above should NOT be in the file
 */

#define DB_PASSWD_FILE "db.passwd"
#define SERVER_PASSWD_FILE "server.passwd"

#include "globals.h"

void
get_database_password (dbname, username, passwd)
    char *dbname;
    char *username;
    char *passwd;
{
    char *b;
    char buf[1024];
    struct {
	char dbname[256];
	char username[256];
	char passwd[256];
    } item;

    char *home;
    FILE *fd;

    *passwd = 0;

    home = getenv ("HOME");
    if (home == NULL) return;

    sprintf (buf, "%s/.sqlbase/%s", home, DB_PASSWD_FILE);
    fd = fopen (buf, "r");
    if (fd == NULL) return ;

    while (fgets (buf, sizeof(buf), fd))
    {
	for (b = buf; *b; b++)
	    if (*b == '\n')
		*b = 0;
	decompose_database_name (buf, item.dbname, item.username, item.passwd);
	if (db_nocase_compare(item.dbname, dbname)
	&&  db_nocase_compare(item.username, username))
	{
	    strcpy (passwd, item.passwd);
	    break;
	}
    }
    fclose (fd);
}

void
get_server_password (server, passwd)
    char *server;
    char *passwd;
{
    char *b;
    char buf[1024];
    struct {
	char server[256];
	char passwd[256];
    } item;

    char *home;
    FILE *fd;

    *passwd = 0;

    home = getenv ("HOME");
    if (home == NULL) return;

    sprintf (buf, "%s/.sqlbase/%s", home, SERVER_PASSWD_FILE);
    fd = fopen (buf, "r");
    if (fd == NULL) return ;

    while (fgets (buf, sizeof(buf), fd))
    {
	for (b = buf; *b; b++)
	    if (*b == '\n')
		*b = 0;
	decompose_server_name (buf, item.server, item.passwd);
	if (db_nocase_compare(item.server, server))
	{
	    strcpy (passwd, item.passwd);
	    break;
	}
    }
    fclose (fd);
}
