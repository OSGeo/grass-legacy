#include <string.h>
#include "gis.h"
#include "local_proto.h"

int ask_title (char *input, char *output, char *method, int nsize, char *title)
{
    char default_title[1024];

    sprintf (default_title,"%dx%d neighborhood: %s of %s",
	    nsize, nsize, method, input);

    do
    {
	fprintf (stdout,"enter title for <%s> [%s]\n> ", output, default_title);
    }
    while (!G_gets(title));

    G_squeeze (title);
    if (*title == 0)
	strcpy (title, default_title);

    return 0;
}
