#include <stdio.h>
input(blank1, word1, blank2, word2, rest)
    char *blank1, *blank2;
    char *word1, *word2, *rest;
{
    char buf[1024];
    char *b,*w1, *w2;

    if (isatty(0))
	fprintf (stderr, "> ");
    
    *blank1 = *blank2 = 0;
    *word1 = *word2 = *rest = 0;

    if (!gets(buf))
    {
	*buf = 0;
	return 0;
    }
    b = buf;
    w1 = word1;
    w2 = word2;

    while(*b == ' ' || *b == '\t')
	*blank1++ = *b++;
    *blank1 = 0;

    while (*b != 0 && *b != ' ' && *b != '\t')
	*word1++ = *b++;
    *word1 = 0;

    while(*b == ' ' || *b == '\t')
	*blank2++ = *b++;
    *blank2 = 0;

    while (*b != 0 && *b != ' ' && *b != '\t')
	*word2++ = *b++;
    *word2 = 0;

    while (*rest++ = *b++)
	;

    if (isatty(0) && strcmp ("end", w1) == 0 && *w2 == 0)
	return 0;
    
    return 1;
}
