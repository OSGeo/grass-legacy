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
#include <grass/gis.h>

static const char *name = "?" ;


/*!
 * \brief return module name
 *
 * Routine returns the name
 * of the module as set by the call to <i>G_gisinit.</i>
 *
 *  \param ~
 *  \return char * 
 */

const char *G_program_name(void)
{
    return name;
}

int G_set_program_name(const char *s)
{
    int i;

    i = strlen (s);
    while (--i >= 0)
    {
	if (G_is_dirsep(s[i]))
	{
	    s += i+1;
	    break;
	}
    }
    G_basename(s, "exe");
    name = G_store (s);

    return 0;
}
