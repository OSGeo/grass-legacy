int lowercase (register char *s)
{
    for ( ; *s; s++)
	if (*s >= 'A' && *s <= 'Z')
	    *s += 'a' - 'A';

    return 0;
}

int uppercase (register char *s)
{
    for ( ; *s; s++)
	if (*s >= 'a' && *s <= 'z')
	    *s += 'A' - 'a';

    return 0;
}
