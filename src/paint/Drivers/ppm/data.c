Pdata (buf, n) 
    unsigned char *buf;
{

    while (n-- > 0)
	color((int) *buf++);
}
