/* TODO: should this go into strings.c ? */

#include <grass/gis.h>


/*!
 * \brief delimiter
 *
 * position of delimiter
 *
 *  \param str
 *  \param delim
 *  \return char * 
 */

char *
G_index  (char *str, int delim)

{
    while (*str && *str != delim)
	str++;
    if (delim == 0)
	return str;
    return (*str ? str : NULL);
}


/*!
 * \brief ???
 *
 * ???
 *
 *  \param str
 *  \param delim
 *  \return char * 
 */

char *
G_rindex  (char *str, int delim)

{
    char *p;

    p = NULL;
    while (*str)
    {
	if (*str == delim)
	    p = str;
	str ++;
    }
    if (delim == 0)
	return str;
    return (p);
}
