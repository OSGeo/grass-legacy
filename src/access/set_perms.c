#include "access.h"
set_perms (path, perms, group, other)
    char *path;
{
    char *explain_perms();

    perms |= OWNER_PERMS;

    perms &= ~GROUP_BITS;
    perms &= ~OTHER_BITS;

    if (group)
	perms |= GROUP_PERMS;
    if (other)
	perms |= OTHER_PERMS;

    if(chmod (path, perms) == 0)
	printf ("%s\n", explain_perms (group, other, 0));
    else
	G_fatal_error ("unable to change mapset permissions");
}
