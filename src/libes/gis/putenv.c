#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
/*******************************************************************
 * G_putenv (name, value)
 *   char *name, *value
 *
 * this routine sets the UNIX envirnoment varaible name to value
 * for the current process and all later children.
 *
 * NOTE: this is a routine that UNIX should have provided, but didn't
 *       as such we are not yet sure of it's portability.
 *
 * This code makes the following assumptions
 *  1. There is an external array of character strings called environ
 *  2. Each string in the environ is of the form name=value
 *  3. We can move the environ pointer to memory which we allocate
 *     so long as this memory is structured as char ** (ie, array
 *     of string pointers) and the exec() routines will use this
 *     new pointer value.
 ******************************************************************/

extern char **environ;

static char *store(char *);

int G_putenv ( char *name, char *value)
{
    int i;
    char *env;
    static int first = 1;
    char **newenv;
    char temp[3];
    char buf[1024];

    if (first)
    {
	for (i = 0; (env = environ[i]); i++)
		;
	newenv = (char **) malloc ((i+1) * sizeof (char *));
	for (i = 0; (env = environ[i]); i++)
	    newenv[i] = store (env);
	newenv[i] = NULL;
	environ = newenv;
	first = 0;
    }
    for (i = 0; (env = environ[i]); i++)
    {
	if (sscanf (env, "%[^=]=%1s", buf, temp) < 1)
	    continue;
	if (strcmp (buf, name))
	    continue;
	free (environ[i]);
	sprintf (buf, "%s=%s", name, value);
	environ[i] = store (buf);
	return 0;
    }
    environ = (char **) realloc (environ, (i+2) * sizeof (char *));
    sprintf (buf, "%s=%s", name, value);
    environ[i++] = store (buf);
    environ[i] = NULL;

    return 1;
}

static char *store(char *buf)
{
    char *s;

    s = malloc (strlen (buf)+1);
    strcpy (s, buf);
    return s;
}
