value (data, n, sflag)
    register unsigned char *data;
{
    register int v;
    int negative;

    if (negative = (sflag && (*data & 0200)))
	v = *data++ & 0177;
    else
	v = *data++;

    while (--n > 0)
	v = v * 256 + *data++;

    return negative ? -v : v;
}
