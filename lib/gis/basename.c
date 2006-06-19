#include <string.h>

/**
 * \brief Truncates filename to the base part (before the last .)
 * if it matches the extension, otherwise leaves it unchanged.
 * 
 * Checks if a filename matches a certain file extension
 * (case insensitive) and if so, truncates the string to the
 * base file name (cf. basename Unix command)
 *
 * \param filename String containing filename
 *
 * \param ext String containing extension to look for (case
 * insensitive and  as long as needs to be, e.g. tif will
 * match .tif and .tiff, sh will match .shp and .shx, htm will
 * match .htm and .html)
 *
 * \return Pointer to filename
 **/

char * G_basename(char *filename, const char *desired_ext)
{
    /* Find the last . in the filename */
    char *dot = strrchr(filename, '.');

    /* Check there is a . and it's not the last character
     * in the string, i.e. there is an extension */
    if(dot && ((dot - filename) < strlen(filename)) )
    {
        char *ext = dot + 1;

        /* if the extension matches (case insensitive)
         * then truncate the filename to the basename */
        if( strncasecmp(ext, desired_ext, strlen(desired_ext)) == 0 )
            *dot = '\0';

    }

    return filename;
}

