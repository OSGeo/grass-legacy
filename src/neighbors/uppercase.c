/* %W% %G% */
uppercase (s)	char *s;
{
    for ( ;*s; s++)
	if (*s >= 'a' && *s <= 'z')
	    *s = *s - 'a' + 'A';
}
