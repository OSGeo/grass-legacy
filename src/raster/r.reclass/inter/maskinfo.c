#include "gis.h"
maskinfo()
{
    struct Reclass reclass;
    char text[80];
    char *label;
    static char *mainlabel = "masking category(ies):";
    int next;
    int len;

    len = strlen (mainlabel);
    printf ("%-*s %s\n", len, "current mask:", G_mask_info());

    if (G_get_reclass ("MASK", G_mapset(), &reclass) <= 0)
	    return;

    next = 0;
    label = mainlabel;
    do
    {
	next = reclass_text (text, &reclass, next);
	if (*label && *text == 0) strcpy (text, "none");
	printf ("%-*s %s\n", len, label, text);
	label = "";
    }
    while (next >= 0);
    G_free_reclass (&reclass);
}

reclass_text (text, reclass, next)
    char *text;
    struct Reclass *reclass;
{
    int i;
    int n;
    int first;

    *text = 0;

    n = reclass->num ;

    first = -1;
    for (i = next; i < n; i++)
    {
	if (reclass->table[i])
	{
	    if (first < 0)
		first = i;
	}
	else if (first >= 0)
	{
	    do_text (text, (long)(first+reclass->min),(long)(i-1+reclass->min));
	    first = -1;
	    if (strlen (text) > 60)
		return i;
	}
    }
    if (first >= 0)
	do_text (text, (long)(first+reclass->min), (long)(i-1+reclass->min));
    return -1;
}

do_text (text, first, last)
    char *text;
    long first, last;
{
    char work[40];

    if (*text)
	strcat (text, " ");

    if (first == last)
	sprintf (work, "%ld", first);
    else
	sprintf (work, "%ld-%ld", first, last);

    strcat (text, work);
}
