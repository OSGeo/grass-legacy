dots (old, new, n)
    char *old, *new;
{
    int i;

/* copy the old to the new leaving at least 2 bytes at end */
    *new = 0;
    for (i = 0; *old && n > 2; i++, n--)
	*new++ = *old++;

/* add a blank to end of text */
    *new++ = ' ';
    n--; i++;

/* add dots every other byte */
    for ( ; n > 0; n--, i++)
	*new++ = i%2 ? ' ' : '.' ;

    *new = 0;
}
