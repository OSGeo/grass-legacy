char *
dig_float_point (buf, size, num)
    char *buf;
    int size;
    double num;
{
    char tmp[100];
    int whole, frac;

    sprintf (tmp, "%lf", num);
    whole = n_index (tmp, '.');
    frac = size - whole - 1;
    sprintf (buf, "%*.*lf", whole, frac, num);
    return (buf);
}

static
n_index (str, chr)
    char *str;
    int chr;
{
    register int cnt;

    for (cnt = 0 ; *str ; str++, cnt++)
	if (*str == chr)
	    return (cnt);
    return (-1);
}
