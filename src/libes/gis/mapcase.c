/*
 * Map uppercase A-Z to lower case a-z
 *
 */

#include <ctype.h>

char *
G_tolcase (string)
    char *string;
{
    register char *p;

    for (p = string; *p; p++)
        if (isupper (*p))
            *p = tolower (*p);

    return (string);
}

/*
 * Map lowercase a-z to uppercase A-Z
 *
 */

char *
G_toucase (string)
    char *string;
{
    register char *p;

    for (p = string; *p; p++)
        if (islower (*p))
            *p = toupper (*p);

    return (string);
}
