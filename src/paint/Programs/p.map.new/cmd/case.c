lowercase (s)
    register char *s;
{
    for ( ; *s; s++)
	if (*s >= 'A' && *s <= 'Z')
	    *s += 'a' - 'A';
}

uppercase (s)
    register char *s;
{
    for ( ; *s; s++)
	if (*s >= 'a' && *s <= 'z')
	    *s += 'A' - 'a';
}
