/**********************************************************************
 *
 *   char *
 *   G_myname()
 *
 *   returns pointer to string containing the descriptive
 *   name of the data base.
 *
 *   note:
 *    This name is the first line in the file 
 *    $GISDBASE/$LOCATION_NAME/PERMANENT/MYNAME
 *
 **********************************************************************/
#include <string.h>
#include "gis.h"
#include "glocale.h"


/*!
 * \brief location title
 *
 * Returns a one line title for
 * the database location. This title is read from the file MYNAME in the
 * PERMANENT mapset. See also Permanent_Mapset for a discussion of the
 * PERMANENT mapset.
 *
 *  \param void
 *  \return char * 
 */

char *
G_myname()
{
    static char name[GNAME_MAX];
    char path[500];
    FILE *fd;
    int ok;

    ok = 0;

    G__file_name (path,"","MYNAME","PERMANENT");
    if ((fd = fopen(path,"r")))
    {
	ok = G_getl(name, sizeof name, fd);
	fclose (fd);
    }
    if (!ok)
	strcpy(name, _("Unknown Location")) ;

    return name;
}
