#include "dbmi.h"
#include <string.h>
#include <stdlib.h>

static
char *dbmscap_files[] = {
    "/etc/dbmscap",
    "/lib/dbmscap",
    "/usr/lib/dbmscap",
    "/usr/local/lib/dbmscap",
    "/usr/local/dbmi/lib/dbmscap",
    NULL };

static void add_entry();

static char *
dbmscap_filename(err_flag)
{
    char *file;
    int i;

    file = getenv ("DBMSCAP");
    if (file)
	return file;

    for (i = 0; file = dbmscap_files[i]; i++)
    {
	if (access (file, 0) == 0)
	    return file;
    }
    if(err_flag)
	db_error ("DBMSCAP not set");
    return ((char *)NULL);
}

char *
db_dbmscap_filename()
{
    return dbmscap_filename(1);
}

int
db_has_dbms()
{
    return (dbmscap_filename(0) != NULL);
}

void
db_copy_dbmscap_entry(dst, src)
    dbDbmscap *dst, *src;
{
    strcpy (dst->driverName, src->driverName);
    strcpy (dst->comment, src->comment);
    strcpy (dst->startup, src->startup);
}

dbDbmscap *
db_read_dbmscap()
{
    FILE *fd;
    char *file;
    char buf[1024];
    char name[1024];
    char startup[1024];
    char comment[1024];
    int line;

    dbDbmscap *list = NULL;

/* get the full name of the dbmscap file */
    file = db_dbmscap_filename();
    if (file == NULL)
	return (dbDbmscap *) NULL;

/* open the dbmscap file */
    fd = fopen (file, "r");
    if (fd == NULL)
    {
	db_syserror (file);
	return (dbDbmscap *) NULL;
    }

/* find all valid entries
 * blank lines and lines with # as first non blank char are ignored
 * format is:
 *   driver name:startup command:comment
 */
    for (line = 1; fgets (buf, sizeof buf, fd); line++)
    {
	if (sscanf (buf, "%1s", comment) != 1 || *comment == '#')
	    continue;
	if (sscanf (buf, "%[^:]:%[^:]:%[^:\n]", name, startup, comment) == 3)
	    add_entry (&list, name, startup, comment);
	else if (sscanf (buf, "%[^:]:%[^:\n]", name, startup) == 2)
	    add_entry (&list, name, startup, "");
	else
	{
	    fprintf (stderr, "%s: line %d: invalid entry\n", file, line);
	    fprintf (stderr,"%d:%s\n", line, buf);
	}
	if (list == NULL) /* add_entry failed */
	    break;
    }
    fclose (fd);
    return list;
}

static void
add_entry (list, name, startup, comment)
    dbDbmscap **list;
    char *name;
    char *startup;
    char *comment;
{
    dbDbmscap *head, *cur, *tail;

/* add this entry to the head of a linked list */
    tail = head = *list;
    while (tail && tail->next)
	tail = tail->next;
    *list = NULL;

    cur = (dbDbmscap *) db_malloc (sizeof(dbDbmscap));
    if (cur == NULL)
	return; /* out of memory */
    cur->next = NULL;

/* copy each item to the dbmscap structure */
    strcpy (cur->driverName, name);
    strcpy (cur->startup, startup);
    strcpy (cur->comment, comment);

/* handle the first call (head == NULL) */
    if (tail)
	tail->next = cur;
    else
	head = cur;

    *list = head;
}

void
db_free_dbmscap (list)
    dbDbmscap *list;
{
    dbDbmscap *next, *cur;

    for (cur = list; cur; cur = next)
    {
	next = cur->next;
	free (cur);
    }
}
