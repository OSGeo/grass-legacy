idx (cc, cellsize, ncolors)
    unsigned char *cc;
{
    register int n;
    register int i;

    n = *cc++;

    for (i = 1; i < cellsize; i++)
	n = n*256 + *cc++;
    if (n >= ncolors)
	n = 0;
    return (n);
}
