/******************************************************************
 *
 *  char *
 *  G_whoami()
 *
 *  Returns a string containing the name of the current user.
 *
 *  Try getlogin() first, then go to paswd file
 *  However, some masscomp getlogin() fails in ucb universe
 *  This is because the ttyname(0) rotuine fails in ucb universe.
 *  So check for this too.
 *
 *****************************************************************/
#include <unistd.h>
#include <pwd.h>
#include "gis.h"

char *G_whoami(void)
{
    static char *name= 0;

#ifdef COMMENTED_OUT
    char *getlogin();
    char *ttyname();

    if (name == NULL)
    {
	char *x;
	x = ttyname(0);
	if (x && *x)
	{
	    x = getlogin();
	    if (x && *x)
		name = G_store (x);
	}
    }
#endif COMMENTED_OUT

    if (name == NULL)
    {
	struct passwd *getpwuid();
	struct passwd *p;
	if((p = getpwuid (getuid())))
	    name = G_store (p->pw_name);
    }
    if (name == NULL)
	name = G_store ("?");

    return name;
}
