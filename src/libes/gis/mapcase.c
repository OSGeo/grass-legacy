/*
 * Map uppercase A-Z to lower case a-z
 *
 */

char *
G_tolcase (string)
    char *string;
{
    register char *p;

    for (p = string; *p; p++)
	*p = tolower (*p);

    return (string);
}
static
tolower(c)
    char c;
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
static
toupper(c)
    char c;
{
    if (c >= 'a' && c <= 'z')
	c += 'A' - 'a';
    return c;
}
