#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "glocale.h"
#include "gis.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    char path[1024];
    int perms; /* full mapset permissions */
    int group, other; /* bool. want group/other permission */
    struct Option *group_opt, *other_opt;
    struct GModule *module;

    /* init the GRASS library */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = "This program allows the user to control access to the current mapset.";

    group_opt = G_define_option();
    group_opt->key = "group";
    group_opt->type = TYPE_STRING;
    group_opt->required = NO;
    group_opt->options = "grant,revoke";
    group_opt->description = "Access for group";

    other_opt = G_define_option();
    other_opt->key = "other";
    other_opt->type = TYPE_STRING;
    other_opt->required = NO;
    other_opt->options = "grant,revoke";
    other_opt->description = "Access for others";

    if (G_parser(argc, argv) < 0)
	exit(-1);

    /* get the unix file name for the mapset directory */
    G__file_name (path, "", "", G_mapset());
    
    /* this part is until PERMANENT no longer holds DEFAULT_WIND and MYNAME */
    if (strcmp (G_mapset(), "PERMANENT") == 0)
    {
	G_warning ( "access to %s must be open, nothing changed", G_mapset());
	exit(0);
    }

    /* get the current permissions */
    if(get_perms (path, &perms, &group, &other) < 0)
	G_fatal_error ("Can't determine mapset permssions");

    if ( group_opt->answer ) {
	if ( group_opt->answer[0] == 'g' )
	    group = 1;
	else
	    group = 0;
    }
    if ( other_opt->answer ) {
	if ( other_opt->answer[0] == 'g' )
	    other = 1;
	else
	    other = 0;
    }
    
    set_perms (path, perms, group, other);

    return 0;
}
