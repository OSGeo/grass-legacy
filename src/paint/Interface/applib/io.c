#include <stdio.h>
#include "interface.h"
static int ascii_int = 0 ;
static int ascii_float = 0 ;

int P__opcode (int c)
{
    unsigned char opcode;

    opcode = c;

    P__writedev (&opcode, 1);

    return 0;
}

int P__get (char *buf, int n)
{
    P__readdev (buf, n);

    return 0;
}

int P__gets (char *buf)
{
    do
	P__readdev (buf, 1) ;
    while (*buf++);

    return 0;
}

int P__geti (void)
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

double P__getf (void)
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

int P__send (char *buf, int n)
{
    P__writedev (buf, n);

    return 0;
}

int P__sendi (int i)
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

    return 0;
}

int P__sendf (double f)
{
    if (ascii_float)
    {
	char buf[100];
	sprintf (buf, "%f", f);
	P__sends (buf) ;
    }
    else
    {
	double ff;
	ff = f;
	P__writedev (&ff, sizeof ff);
    }

    return 0;
}

int P__sends (char *buf)
{
    while (*buf >= 040 && *buf < 0177)
	P__writedev (buf++, 1);
    P__writedev ("",1);

    return 0;
}

int P__transparent (int t)
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

    return 0;
}
