/**********************************************************************
 *
 *   char *
 *   G_legal_filename (name)
 *      char *name           filename to be checked
 *
 *   returns:    1  if name is OK
 *              -1  if name begins with ".", if name contains a "/",
 *                  if name contains a quote character,
 *                  or if name contains a non-printing character.
 **********************************************************************/

G_legal_filename (s)
    char *s;
{
    if (*s == '.' || *s == 0)
	return -1;

    for ( ; *s; s++)
	if (*s == '/' || *s == '"' || *s == '\'' || *s <= ' ' || *s > 0176)
	    return -1;

    return 1;
}
