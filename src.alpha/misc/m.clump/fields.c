#include "glob.h"

int *
get_field_list (fields, count)
    char **fields;
    int *count;
{
    int *list;
    int n;

    *count = 0;
    if (fields)
	while (fields[*count])
	    (*count)++;
    list = NULL;
    if (*count)
    {
	list = (int *) G_calloc (*count, sizeof(int));
	for (n = 0; n < *count; n++)
	    if (sscanf (fields[n], "%d", &list[n]) != 1)
		list[n] = -1;
	    else
		list[n]--;
    }
    return list;
}

/* multi char fs search */
char *
find_fs (buf, fs)
    char *buf;
    char *fs;
{
    char *c;
    if (buf == NULL) return NULL;

    while (*buf)
    {
	for (c = fs; *c; c++)
	    if (*c == *buf)
		return buf;
	buf++;
    }
    return NULL;
}


get_field (buf, which, fs, word)
    char *buf;
    int which;
    char *fs;
    char *word;
{
    char *ep;
    int n;

    *word = 0;
    if (which < 0) return 0;

/* fs==NULL means blanks and tabs */
    if (fs == NULL)
    {
	while (*buf == ' ' || *buf == '\t')
	    buf++;
    }


/* find the start of this field */
    for (n = 0; buf != NULL && n < which; n++)
    {
	buf = find_fs (buf, fs?fs:" \t");
	if (!buf) return 0;
	buf++;
	if (fs == NULL)
	    while (*buf == ' ' || *buf == '\t')
		buf++;
    }

    if (!buf)
	return 0;
    if (*buf == 0)
	return 0;

/* find the end of this field */
    ep = find_fs (buf, fs?fs:" \t");
    if (ep == NULL)
	for (ep = buf; *ep; ep++)
	    {}
    while (buf < ep)
	*word++ = *buf++;
    *word = 0;
    return 1;
}
