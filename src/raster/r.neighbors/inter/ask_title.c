#include "gis.h"

ask_title (input, output, method, nsize, title)
    char *input, *output, *method, *title;
{
    char default_title[1024];

    sprintf (default_title,"%dx%d neighborhood: %s of %s",
	    nsize, nsize, method, input);

    do
    {
	printf("enter title for <%s> [%s]\n> ", output, default_title);
    }
    while (!G_gets(title));

    G_squeeze (title);
    if (*title == 0)
	strcpy (title, default_title);
}
