#include "xgrass_lib.h"

#ifdef _NO_PROTO
char **
_XgTokenize (buf, delim)
char *buf;
char *delim;
#else
char **
_XgTokenize ( char *buf, char *delim)
#endif
{
    int i;
    char **tokens;
    char *tmp;

    i = 0;
    while (*buf == ' ' || *buf == '\t')  /* needed for free () */
        buf++;

    tmp = _XgMalloc(strlen(buf)+1);
    strcpy(tmp,buf);
    buf = tmp;

    tokens = (char **) _XgMalloc (sizeof (char *));

    while (1)
    {
        while (*buf == ' ' || *buf == '\t')
            buf++;
        if (*buf == 0)
            break;
        tokens[i++] = buf;
        tokens = (char **) _XgRealloc ((void *) tokens, (i+1) * sizeof (char *));

        while (*buf && (G_index(delim,*buf) == NULL))
            buf++;
        if (*buf == 0)
            break;
        *buf++ = 0;
    }
    tokens[i] = NULL;

    return (tokens);
}

#ifdef _NO_PROTO
void
_XgFreeTokens (tokens)
char **tokens;
#else
void
_XgFreeTokens ( char **tokens)
#endif
{
    if (tokens[0] != NULL)
        _XgFree (tokens[0]);
    _XgFree ((void *) tokens);
}

#ifdef _NO_PROTO
int
_XgNumberOfTokens(tokens)
char **tokens;
#else
int
_XgNumberOfTokens( char **tokens)
#endif
{
    int n;
    for ( n = 0; tokens[n]; n++);
    return n;
}
