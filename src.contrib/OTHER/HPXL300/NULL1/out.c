#include "P.h"

Pout (buf, n)
    char *buf;
    int n;
{
	int i;
	for (i = 0; i < n; i++) fputc(buf[i],out);
}

Pouts(buf)
	char *buf;
{
	Pout(buf,strlen(buf));
}

Poutc(buf)
	unsigned char buf;
{
	Pout(&buf,1);
}
