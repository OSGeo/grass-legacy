#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "gis.h"

int main(int argc, char *argv[])
{
    int n;
    int tty;
    char *name, *value, *ptr;
    struct Option *get, *set;
    struct GModule *module;
    
    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		"Outputs the user's current GRASS variable settings.";

    get               = G_define_option() ;
    get->key          = "get" ;
    get->type         = TYPE_STRING ;
    get->description  = "GRASS Environment variable to get" ;
    get->required     = NO;

    set               = G_define_option() ;
    set->key          = "set" ;
    set->type         = TYPE_STRING ;
    set->description  = "GRASS Environment variable to set" ;
    set->required     = NO;
    
    /* Print or optionally set environment variables */
    if (argc == 1)
    {
        tty = isatty(1);
        for (n=0; name = G__env_name (n); n++)
        {
            if (value = G__getenv(name))
                if (tty)
                    fprintf (stdout,"%s=%s\n", name, value);
                else
                    fprintf (stdout,"%s='%s'; export %s;\n", name, value, name);
        }
        return 0;
    }

    if (G_parser(argc, argv) < 0)
       exit(-1);

    if (get->answer != NULL)
    {
	value = G__getenv (get->answer) ;
	if (value != NULL)
	    fprintf (stdout, "%s\n", value);
	return 0;
    }

    if (set->answer != NULL)
    {
	value = NULL ;
	name = set->answer ;
	ptr = strchr (name, '=') ;
	if (ptr != NULL)
	{ 
	    *ptr = '\0';
	    value = ptr + 1;
	}
	/* Allow unset without '=' sign */
	if (value != NULL && *value == '\0')
	  value == NULL ;
	  
        G_setenv (name, value) ;

	return 0;
    }
	
    /* Something's wrong if we got this far */
    G_usage();
    return -1;
}
