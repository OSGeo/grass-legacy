
/* copy a structure from a to b */
dig_struct_copy (from, to, cnt)
    unsigned char *from, *to;
    int cnt;
{
    register int i;

    for (i = 0 ; i < cnt ; i++)
	*to++ = *from++;
}

dig_rmcr (str)
    char *str;
{
    int i;

    i = strlen (str) - 1;
    if (str[i] == '\n')
	str[i] = '\0';
}
