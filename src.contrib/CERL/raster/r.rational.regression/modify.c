#include <stdio.h>

modify (name,x,i)
    char *name;
    double *x;
{
    char buf[100];
    double v;

    while(1)
    {
	fprintf (stdout,"%s[%d]=%g  new value? ",name,i,x[i]);
	if (!fgets(buf,80,stdin)) exit(0);
	if (*buf == 0) return;
	if (sscanf (buf, "%lf", &v) == 1) break;
	fprintf (stdout,"??\n");
    }
    x[i] = v;
}
