readcolors (red, grn, blu, ncols, max)
    register unsigned char *red, *grn, *blu;
    register int ncols;
{
    /*register int c; */

    if (max < 255)
    {
	while (ncols-- > 0)
	{
	    *red++ = (getchar() * 255) / max;
	    *grn++ = (getchar() * 255) / max;
	    *blu++ = (getchar() * 255) / max;
	    /*
	    c = getchar(); c = c * 255; c /= max; *red++ = c;
	    c = getchar(); c = c * 255; c /= max; *grn++ = c;
	    c = getchar(); c = c * 255; c /= max; *blu++ = c;
	    */
	}
    }
    else
    {
	while (ncols-- > 0)
	{
	    *red++ = getchar();
	    *grn++ = getchar();
	    *blu++ = getchar();
	}
    }
}
