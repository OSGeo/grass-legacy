#include "gis.h"
/**********************************************************************
 *
 *   char *
 *   G_program_name()
 *
 *   returns the current program name
 *
 **********************************************************************
 *
 *   G_set_program_name(name)
 *        char *name 
 *
 *   program name set to name (name will be returned by G_program_name
 *
 **********************************************************************/
#include <string.h>
static char *name = "?" ;


/*!
 * \brief return module name
 *
 * Routine returns the name
 * of the module as set by the call to <i>G_gisinit.</i>
 *
 *  \param ~
 *  \return char * 
 */

char *G_program_name()
{
    return name;
}

int G_set_program_name ( char *s)
{
    char *G_store();
    int i;

    i = strlen (s);
    while (--i >= 0)
    {
	if (s[i] == '/')
	{
	    s += i+1;
	    break;
	}
    }
    name = G_store (s);

    return 0;
}
