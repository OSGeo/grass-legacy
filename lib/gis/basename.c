/**
 * \file basename.c
 *
 * \brief Program basename routines.
 *
 * This program is free software under the GNU General Public License
 * (>=v2). Read the file COPYING that comes with GRASS for details.
 *
 * \author GRASS GIS Development Team
 *
 * \date 1999-2007
 */

#include <ctype.h>
#include <string.h>


/**
 * \fn char * G_basename (char *filename, const char *desired_ext)
 *
 * \brief Truncates filename to the base part (before the last '.')
 * if it matches the extension, otherwise leaves it unchanged.
 * 
 * Checks if a filename matches a certain file extension
 * (case insensitive) and if so, truncates the string to the
 * base file name (cf. basename Unix command)
 *
 * \param[in] filename string containing filename
 * \param[in] desired_ext string containing extension to look for (case
 * insensitive and  as long as needs to be, e.g. tif will
 * match .tif and .tiff, sh will match .shp and .shx, htm will
 * match .htm and .html)
 * \return Pointer to filename
 */

char * G_basename(char *filename, const char *desired_ext)
{
    /* Find the last . in the filename */
    char *dot = strrchr(filename, '.');

    /* Check there is a . and it's not the last character
     * in the string, i.e. there is an extension */
    if(dot && ((dot - filename) < strlen(filename)) )
    {
        char *ext = dot + 1;
        int i, match = 1;

        /* if the extension matches (case insensitive)
         * then truncate the filename to the basename */
        for( i = 0; i < strlen(desired_ext); i++ )
        {
            if( (ext[i] == '\0') || (tolower(ext[i]) != tolower(desired_ext[i])) )
            {
                match = 0;
                break;
            }
        }
        
        if( match )
            *dot = '\0';

    }

    return filename;
}
