/* look for at least one file in the element */
#include <string.h>
#include <unistd.h>
#include "gis.h"
int 
empty (char *elem)
{
    FILE *ls, *popen();
    char command[1024];
    char *dir;
    int any;

    dir = command;
    sprintf (dir, "ls ");
    dir += strlen (dir);
    G__file_name (dir, elem, "", G_mapset());

    any = 0;
    if (access(dir,0) == 0 && (ls = popen(command, "r")))
    {
	any = fgets(command, 10, ls) != 0;
	pclose (ls);
    }
    return any == 0;
}
