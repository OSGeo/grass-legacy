#define MAIN
#include "list.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    int n;

    G_gisinit (argv[0]);

    read_list(0);	/* read element list from etc */

    G_set_list_hit_return(1);
    while((n = menu(LIST)) >= 0)
	do_list (n,"");

    return 0;
}
