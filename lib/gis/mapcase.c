#include "gis.h"
/*
 * Map uppercase A-Z to lower case a-z
 *
 */
static int toupper(char);
static int tolower(char);

char *
G_tolcase (string)
    char *string;
{
    register char *p;

    for (p = string; *p; p++)
	*p = tolower (*p);

    return (string);
}

static int tolower(char c)
{
    if (c >= 'A' && c <= 'Z')
	c -= 'A' - 'a';
    return c;
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
	*p = toupper (*p);

    return (string);
}

static int toupper(char c)
{
    if (c >= 'a' && c <= 'z')
	c += 'A' - 'a';
    return c;
}
