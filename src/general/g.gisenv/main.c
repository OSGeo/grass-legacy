#include <stdio.h>

main(argc,argv) char *argv[];
{
    int n;
    int tty;
    char *name;
    char *value;
    char *G__env_name();
    char *G__getenv();
    char xname[30], xvalue[70];

    for (n = 1; n < argc; n++)
    {
	if (sscanf (argv[n], "%[^= \t]=%s", xname, xvalue) == 2)
	    G_setenv (xname, xvalue);
	else if (sscanf (argv[n], "%[^=]%s", xname, xvalue) == 2)
	    G_unsetenv (xname);
	else if (value = G__getenv (argv[n]))
	    fprintf (stdout, "%s\n", value);
    }
    if (argc < 2)
    {
	tty = isatty(1);
	for (n=0; name = G__env_name (n); n++)
	    if (value = G__getenv(name))
		if (tty)
		    fprintf (stdout, "%s=%s\n", name, value);
		else
		    fprintf (stdout, "%s='%s'; export %s;\n", name, value, name);
    }
    return 0;
}
