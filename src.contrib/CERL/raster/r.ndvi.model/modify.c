modify (name,x,i)
    char *name;
    double *x;
{
    char buf[100];
    double v;

    while(1)
    {
	printf ("%s[%d]=%g  new value? ",name,i,x[i]);
	if (!gets(buf)) exit(0);
	if (*buf == 0) return;
	if (sscanf (buf, "%lf", &v) == 1) break;
	printf ("??\n");
    }
    x[i] = v;
}
