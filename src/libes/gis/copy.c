/*
 *********************************************************
 *
 * G_copy (a, b, n)
 *
 * copies n bytes starting at address b into address a
 ********************************************************/

G_copy (a, b, n)
    char *a, *b;
{
    while (n-- > 0)
	*a++ = *b++;
}
