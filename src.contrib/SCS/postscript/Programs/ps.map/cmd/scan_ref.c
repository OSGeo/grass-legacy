/* This stuff is taken from the p.map/cmd files text.h, scan_ref.c and case.c
**
** Paul W. Carlson	March 1992
*/

#define LEFT 0
#define RIGHT 1
#define LOWER 0
#define UPPER 1
#define CENTER 2

static xok, yok;

scan_ref (buf, xref, yref)
    char *buf;
    int *xref, *yref;
{
    char word1[50], word2[50];

    xok = yok = 0;

    *xref = *yref = CENTER;
    switch (sscanf (buf, "%s%s", word1, word2))
    {
    case 2:
	lowercase (word2);
	if (!(xmatch (word2, xref) || ymatch (word2, yref)))
	    return 0;
    case 1:
	lowercase (word1);
	if (xmatch (word1, xref) || ymatch (word1, yref))
	    return 1;
    default:
	return 0;
    }
}

static
xmatch (word, xref)
    char *word;
    int *xref;
{
    if (strcmp (word, "center") == 0)
	return 1;
    if (strcmp (word, "middle") == 0)
	return 1;
    if (xok) return 0;

    if (strcmp (word, "left") == 0)
	*xref = LEFT;
    else if (strcmp (word, "right") == 0)
	*xref = RIGHT;
    else
	return 0;
    xok = 1;
    return 1;
}

static
ymatch (word, yref)
    char *word;
    int *yref;
{
    if (strcmp (word, "center") == 0)
	return 1;
    if (strcmp (word, "middle") == 0)
	return 1;
    if (yok) return 0;

    if (strcmp (word, "upper") == 0)
	*yref = UPPER;
    else if (strcmp (word, "top") == 0)
	*yref = UPPER;
    else if (strcmp (word, "lower") == 0)
	*yref = LOWER;
    else if (strcmp (word, "bottom") == 0)
	*yref = LOWER;
    else
	return 0;
    yok = 1;
    return 1;
}



lowercase (s)
    register char *s;
{
    for ( ; *s; s++)
	if (*s >= 'A' && *s <= 'Z')
	    *s += 'a' - 'A';
}
