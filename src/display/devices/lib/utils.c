
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gis.h"
#include "utils.h"

char *
xalloc(void *buf, int *cur, int new, int len)
{
    if (*cur >= new)
        return buf;
    if (*cur)
        buf = (char *) G_realloc((void *) buf, (size_t) (new * len));
    else
        buf = (char *) G_malloc((size_t) (new*len));
    *cur = new;
    if (buf != NULL)
        return buf;
    fprintf(stderr, "Monitor: Out of Memory\n");
    exit(1);
}

char *
store(char *s)
{
    char *buf;

    buf = (char *) G_malloc((size_t) (strlen(s) + 1));
    if (buf != NULL)
        strcpy(buf, s);
    return buf;
}

