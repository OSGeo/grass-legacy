/* %W% %G% */
zero (buf, n)
    char *buf;
{
    while (n-- > 0)
	*buf++ = 0;
}
