#include "gis.h"
#include "Vect.h"
#include "glocale.h"
/**********************************************************************
 *
 *   char *
 *   Vect_legal_filename (name)
 *      char *name           filename to be checked
 *
 *   returns:    1  if name is OK
 *              -1  if name does not start with letter A..Za..z
 *                  or if name does not continue with A..Za..z0..9_
 *                  Rule:  [A-Za-z][A-Za-z0-9_]*
 **********************************************************************/

int Vect_legal_filename (char *s)
{
    if (*s == '.' || *s == 0) {
	fprintf(stderr, _("Illegal map name. May not contain '.' or 'NULL'.\n"));
	return -1;
    }

    /* file name must start with letter */
    if (! ((*s >= 'A' && *s <= 'Z') || (*s >= 'a' && *s <= 'z')) ) {
	fprintf(stderr, _("Illegal map name. Must start with a letter.\n"));
	return -1;
    }

    for (s++ ; *s; s++)
	if (! ((*s >= 'A' && *s <= 'Z') || (*s >= 'a' && *s <= 'z') || (*s >= '0' && *s <= '9') || *s == '_' ) ) {
		fprintf(stderr, _("Illegal map name. Character <%c> not allowed.\n"), *s);
	    return -1;
	}

    return 1;
}
