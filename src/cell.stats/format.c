format_double (v, buf, n)
    double v;
    char *buf;
    int n;
{
    char fmt[15];
    int k;

    sprintf (fmt, "%%%d.2lf", n);
    sprintf (buf, fmt, v);

    for (k = n; strlen (buf) > n; k--)
    {
	sprintf (fmt, "%%%d.%dg", n, k);
	sprintf (buf, fmt, v);
    }
}
