/*
 * squeeze - edit superfluous white space out of strings
 *
** char *squeeze (s)
**     char *s;
 *
 * scan a string of text, converting tabs to spaces and
 * compressing out leading spaces, redundant internal spaces,
 * and trailing spaces.
 * returns the address of the resulting compressed string.
 *
 * last modification: 12 aug 81, j w hamilton
 *
 */

#include <ctype.h>

char *
G_squeeze (line)
    char *line;
{
    register char *f = line, *t = line;

    while (isspace (*f))
        f++;

    if (! *f)
    {
        *t = '\0';
        return (line);
    }

    while (*f)
        if (! isspace (*f))
            *t++ = *f++;
        else
            if (*++f)
                if (! isspace (*f))
                    *t++ = ' ';
    *t = '\0';

    return (line);
}
