/* %W% %G% */

static
mod (x, m)		/* % fails for long */
    long x;
{
    return (int) (x - x / m * m);
}

static
round (x, m)
    long x;
{
    if (x < 0)
	return  mod(-x, m) ;

    if (mod(x, m))
	return m - mod(x, m) ;

    return 0;
}

round_up (x, m)
    long *x;
{
    *x += round (*x, m);
}

round_down (x, m)
    long *x;
{
    *x -= round (-(*x), m);
}
