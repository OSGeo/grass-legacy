clear (buf, n)
    register char *buf;
    register int n;
{
    while (n-- > 0)
	*buf++ = 0000;
}
