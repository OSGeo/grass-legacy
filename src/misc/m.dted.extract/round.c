static int mod (		/* % fails for long */
    long x,
    int m
)
{
    return (int) (x - x / m * m);
}

static int round (long x, int m)
{
    if (x < 0)
	return  mod(-x, m) ;

    if (mod(x, m))
	return m - mod(x, m) ;

    return 0;
}

int round_up (long *x, int m)
{
    *x += round (*x, m);

    return 0;
}

int round_down (long *x, int m)
{
    *x -= round (-(*x), m);

    return 0;
}
