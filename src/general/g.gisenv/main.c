#include "gis.h"
#include <unistd.h>

int main(int argc, char *argv[])
{
    int n;
    int tty;
    char *name;
    char *value;
    char *help[2];
    char xname[30], xvalue[70];
    struct GModule *module;
    
    module = G_define_module();
    module->description =
		"Outputs the user's current GRASS variable settings.";
    
    G_gisinit (argv[0]);

    if (argc > 1) /* print out the help stuff */
    {
    if (G_parser(argc, argv) < 0)
       exit(-1);
    }

    for (n = 1; n < argc; n++)
    {
	if (sscanf (argv[n], "%[^= \t]=%s", xname, xvalue) == 2)
	    G_setenv (xname, xvalue);
	else if (sscanf (argv[n], "%[^=]%s", xname, xvalue) == 2)
	    G_unsetenv (xname);
	else if (value = G__getenv (argv[n]))
	    fprintf (stdout,"%s\n", value);
    }
    if (argc < 2)
    {
	tty = isatty(1);
	for (n=0; name = G__env_name (n); n++)
	    if (value = G__getenv(name))
		if (tty)
		    fprintf (stdout,"%s=%s\n", name, value);
		else
		    fprintf (stdout,"%s='%s'; export %s;\n", name, value, name);
    }
    return 0;
}
