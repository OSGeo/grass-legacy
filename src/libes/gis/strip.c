/* Correction from Clifton Wood <cbwood@vt.edu> in Row 25, if condition
 * 6/1998 */
/*******************************************************************
 *  G_strip(buf)
 *     char *buf         buffer to be worked on
 *
 *  'buf' is rewritten in place with leading and trailing white
 *  space removed.
 ******************************************************************/

G_strip (buf)
    register char *buf;
{
    register char *a, *b;

/* remove leading white space */
    for (a = b = buf; *a == ' ' || *a == '\t'; a++)
	    ;
    if (a != b)
	while (*b++ = *a++)
	    ;
/* remove trailing white space */
    for (a = buf; *a; a++)
	    ;
    if ((a != buf) && *a)
    {
	for (a--; *a == ' ' || *a == '\t'; a--)
		;
	a++;
	*a = 0;
    }
}
