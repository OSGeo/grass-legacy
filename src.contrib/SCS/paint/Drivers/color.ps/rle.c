Prle (buf, n)
    unsigned char *buf;
{
    int value, repeat;

    while (n-- > 0)
    {
	repeat = *buf++;
	value = *buf++;
	while (repeat-- > 0)
	    color(value);
    }
}
