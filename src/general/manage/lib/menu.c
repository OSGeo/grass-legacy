#include "list.h"

static int count;
static char **text;
static int first, last;

menu(type)
{
    int i;
    int n;
    int x;
    char buf[100];

    build_menu();

    first = 0;

    while (1)
    {
	last = first + 30;
	if (last > count) last = count;

	G_clear_screen ();
	switch (type)
	{
	case RENAME: printf ("RENAME"); break;
	case REMOVE: printf ("REMOVE"); break;
	case COPY:   printf ("COPY"); break;
	case LIST:   printf ("LIST"); break;
	}
	printf (" FACILITY\n\n");
	printf ("This program allows you to ");
	switch (type)
	{
	case RENAME:
		printf ("rename files found in your mapset");
		break;
	case REMOVE:
		printf ("remove files found in your mapset");
		break;
	case COPY:
		printf ("copy files from other mapsets into your mapset");
		break;
	case LIST:
		printf ("list files from mapsets in your search path");
		break;
	}
	printf ("\n\n");
	printf ("Please select the type of file to be ");
	switch (type)
	{
	case RENAME: printf ("renamed"); break;
	case REMOVE: printf ("removed"); break;
	case COPY:   printf ("copied"); break;
	case LIST:   printf ("listed"); break;
	}

	printf ("\n\n");
	display_menu();
	printf ("\n");

	if (first > 0)
	    printf ("  -    to see previous menu page\n");
	if (last < count)
	    printf ("  +    to see next menu page\n");
	printf ("RETURN to exit\n\n");

	printf ("> ");

	if (!G_gets(buf)) continue;
	if (*buf == 0)
	{
	    free_menu();
	    return -1;
	}
	G_strip (buf);
	if (first > 0 && strcmp (buf,"-") == 0)
	{
	    first -= 30;
	    if (first < 0) first = 0;
	    continue;
	}
	if (last < count && strcmp (buf,"+") == 0)
	{
	    first = last;
	    continue;
	}
	if (sscanf (buf, "%d", &x) != 1) continue;
	i = 1;
	for (n = 0; n < nlist; n++)
	    if (list[n].status && (i++ == x))
	    {
		free_menu();
		return n;
	    }
    }
}

build_menu()
{
    char buf[100];
    int n;

    count = 0;
    text = 0;
    for (n = 0; n < nlist; n++)
	if (list[n].status)
	{
	    sprintf (buf, "%3d %-.30s", ++count, list[n].text);
	    text = (char **) G_realloc (text, count * sizeof(char *));
	    text[count-1] = G_store (buf);
	}
}

free_menu()
{
    while (count-- > 0)
	free (text[count]);
}
display_menu()
{
    int left, right;
    int i;

    left  = first;
    right = first + 15;

    for (i = 0; i < 15; i++)
    {
	if (left >= last) break;
	if (right >= last)
	    printf ("  %-35.35s\n", text[left]);
	else
	    printf ("  %-35.35s  %-35.35s\n", text[left], text[right]);

	left++;
	right++;
    }
}
