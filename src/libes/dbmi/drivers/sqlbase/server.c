#include "globals.h"

connect_to_server(server, server_handle)
    char *server;
    SQLTSVH *server_handle; 
{
    char name[256];
    char passwd[256];
    char *dir;
    char msg[1024];
    char server_src[512];
    SQLTRCD rcd;

    *server_handle = 0;
    *server_src = 0;
    if (server == NULL)
    {
	server = get_sqlbase_server();
	sprintf (server_src, "%s=", SQLBASE_SERVER);
    }
    if (server && *server)
    {
	decompose_server_name (server, name, passwd);
	if (*passwd == 0)
	    get_server_password (server, passwd);

	if(rcd = sqlcsv (server_handle, server, passwd))
	{
	    *server_handle = 0;
	    dir = get_sqlbase_directory();
	    sprintf(msg,
		"Connection to server (%s%s) failed.\nCheck the SQL.INI file%s%s",
		server_src, server, dir?" in ":"", dir?dir:"");
	    report_error (rcd, msg);
	    return DB_FAILED;
	}
    }
    return DB_OK;
}

void
disconnect_from_server(svh)
    SQLTSVH *svh; 
{
    if (*svh)
    {
	sqldsv (*svh);
	*svh = 0;
    }
}

char *
get_server_name_from_list (n, list, count)
    int n;
    dbString *list;
    int count;
{
    if (count == 0 && n == 0)
	return get_sqlbase_server();
    if (n < count)
	return db_get_string (&list[n]);

    return (char *) NULL;
}

#define SLASH '/'

void
decompose_server_name (fullname, server, passwd)
    char *fullname;
    char *server;
    char *passwd;
{
    char *a, *b;

/* full server names have the format
 *	server/password
 *
 * decompose the fullname into its parts
 */
    *server = *passwd = 0;
    a = fullname;

    b = server;
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
}
