#include "gis.h"
#include <string.h>


/*!
 * \brief copy string to allocated memory
 *
 * This routine allocates enough memory to hold the string <b>s</b>,
 * copies <b>s</b> to the allocated memory, and returns a pointer
 * to the allocated memory. In case the string <b>s</b> is NULL,
 * the function returns NULL.
 * 
 *  \param s
 *  \return char * 
 */

char *G_store (s) char *s;
{
    char *buf;

    if ( s == NULL ) return NULL ;
    
    buf = G_malloc (strlen(s) + 1);
    strcpy (buf, s);
    return buf;
}
