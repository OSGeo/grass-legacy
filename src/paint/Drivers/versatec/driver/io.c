Popen(port)
    char *port;
{
}

Pclose()
{
}

Pflush ()
{
}

Pout (buf, len)
    char *buf ;
{
    while (len-- > 0)
    {
	Poutc (*buf);
	buf++;
    }
}

Poutc ( c)
    unsigned char c;
{
}

Pouts (s)
    unsigned char *s ;
{
    while (*s)
	Poutc (*s++);
}
