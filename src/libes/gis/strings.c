/*
 * string/chring movement functions
 *
** strcpy (T, F)
** strncpy (T, F, n)	copy F up to null or n, always copy null
** chrcpy (T, F, n)
** strmov (T, F)
** chrmov (T, F, n)
** strcat (T, F)
** chrcat (T, F, n)
**     char *T, *F;
**     int n;
 *
 * strcpy (T, F)    copy F up to null, copy null
 * chrcpy (T, F, n) copy F up to n,    copy null
 * 
 * strmov (T, F)    copy F up to null
 * chrmov (T, F, n) copy F up to n
 * 
 * strcat (T, F)    cat F up to null, copy null
 * chrcat (T, F, n) cat F up to n,    copy null
 *
 * the -cpy and -cat functions are for null-terminated destinations;
 * the -mov functions are for non-null-terminated ('chring') destinations.
 * all functions return 'T'.
 *
 * last modification: 12 aug 81, j w hamilton
 *
 */
static char *
G_strend (S)
    register char *S;
{
    while (*S)
	S++;
    return (S);
}

char *
G_strcpy (T, F)
    register char *T, *F;
{
    register char *d = T;

    while (*d++ = *F++)
        ;
    return (T);
}

char *
G_chrcpy (T, F, n)
    register char *T, *F;
    register int n;
{
    register char *d = T;

    while (n--)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

char *
G_strncpy (T, F, n)
    register char *T, *F;
    register int n;
{
    register char *d = T;

    while (n-- && *F)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

char *
G_strmov (T, F)
    register char *T, *F;
{
    register char *d = T;

    while (*F)
        *d++ = *F++;
    return (T);
}

char *
G_chrmov (T, F, n)
    register char *T, *F;
    register int n;
{
    register char *d = T;

    while (n--)
        *d++ = *F++;
    return (T);
}

char *
G_strcat (T, F)
    register char *T, *F;
{
    G_strcpy (G_strend (T), F);
    return (T);
}

char *
G_chrcat (T, F, n)
    register char *T, *F;
    register int n;
{
    G_chrcpy (G_strend (T), F, n);
    return (T);
}
