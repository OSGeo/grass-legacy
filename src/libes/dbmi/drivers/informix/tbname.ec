#include "dbmi.h"
#include "globals.h"

/* convert name into creator, tablename pair */
void
decompose_tablename (string, creator, name)
    char *string;
    char *creator;
    char *name;
{
    if (sscanf (string,"%[^.].%s", creator, name) != 2)
    {
	strcpy (name, string);
	strcpy (creator, db_whoami());
    }
}

void
compose_tablename (string, creator, name)
    char *string;
    char *creator;
    char *name;
{
    sprintf (string, "%s.%s", creator, name);
}

