Ptext (buf) char *buf;
{
    char *s;
    int i;
    for (i = 0; i < 2; i++)	/* BOLD */
    {
	for (s = buf; *s >= ' ' && *s < 0177; s++)
	    Poutc (*s);
	Poutc('\r');
    }
    Poutc('\n');
}
