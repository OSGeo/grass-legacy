#include "glob.h"

set_cat (result, cat, pcats)
    CELL result;
    CELL *cat;
    struct Categories *pcats;
{
    int i,n;
    static char *buf = NULL;
    static int len = 0;
    char *get_label();
    char *lbl;


    if (result == 0) return;

    n = 0;	
    for (i = 0; i < nfiles; i++)
    {
	lbl = get_label (cat[i], &labels[i]);
	n += strlen(lbl) + 2;
    }
    if (len < n) buf = G_realloc (buf, len = n);

    *buf = 0;
    for (i = 0; i < nfiles; i++)
    {
	if (i) strcat (buf, "; ");
	lbl = get_label (cat[i], &labels[i]);
	strcat(buf, lbl);
    }
    G_set_cat (result, buf, pcats);
}

static char *
get_label(cat, labels)
    CELL cat;
    struct Categories *labels;
{
    char *lbl;
    static char temp[256];

    lbl = G_get_cat (cat, labels);
    if (*lbl == 0)
	sprintf (lbl = temp, "category %ld", (long) cat);
    return lbl;
}
