static int ascii_int = 0 ;
static int ascii_float = 0 ;

P__opcode (c)
{
    unsigned char opcode;

    opcode = c;

    P__writedev (&opcode, 1);
}

P__get (buf, n)
    char *buf;
{
    P__readdev (buf, n);
}

P__gets (buf)
    char *buf;
{
    do
	P__readdev (buf, 1) ;
    while (*buf++);
}

P__geti ()
{
    int i;

    if (ascii_int)
    {
	char buf[100];

	P__gets (buf);
	sscanf (buf, "%d", &i);
    }
    else
	P__readdev (&i, sizeof i);

    return i;
}

double
P__getf ()
{
    double f;

    if (ascii_float)
    {
	char buf[100];

	P__gets (buf);
	sscanf (buf, "%lf", &f);
    }
    else
	P__readdev (&f, sizeof f);
    return f;
}

P__send (buf, n)
    char *buf;
{
    P__writedev (buf, n);
}

P__sendi (i)
{
    if (ascii_int)
    {
	char buf[100];
	sprintf (buf, "%d", i);
	P__sends (buf) ;
    }
    else
    {
	int ii;
	ii = i;
	P__writedev (&ii, sizeof ii );
    }
}

P__sendf (f)
    double f;
{
    if (ascii_float)
    {
	char buf[100];
	sprintf (buf, "%lf", f);
	P__sends (buf) ;
    }
    else
    {
	double ff;
	ff = f;
	P__writedev (&ff, sizeof ff);
    }
}

P__sends (buf)
    char *buf;
{
    while (*buf >= 040 && *buf < 0177)
	P__writedev (buf++, 1);
    P__writedev ("",1);
}

P__transparent (t)
{
    ascii_float = ascii_int = 0;
    switch (t)
    {
    case 'f':
    case 'F':
	ascii_float = 1;
	break;
    case 'i':
    case 'I':
	ascii_int = 1;
	break;
    case 'a':
    case 'A':
	ascii_float = 1;
	ascii_int = 1;
	break;
    }
}
