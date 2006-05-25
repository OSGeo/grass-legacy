#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <grass/gis.h>

int test_path_file ( const char *path, const char *file )
{
    int ret;
    char *buf;

    buf = (char *) G_malloc ( strlen(path) + strlen(file) + 2 );
    sprintf ( buf, "%s/%s", path, file );

    ret = access(buf,F_OK);
    G_free ( buf );

    if ( ret == 0 ) return 1;

    return 0;
}

/*!
 * \brief Test if specified directory is GISBASE
 * \param path Path to directory
 * \return 1 The directory is GISBASE
 * \return 0 The directory is not GISBASE
 */
int G_is_gisbase (const char *path)    
{
    return test_path_file ( path, "etc/element_list" );
}

/*!
 * \brief Test if specified directory is location
 * \param path Path to directory
 * \return 1 The directory is location
 * \return 0 The directory is not location
 */
int G_is_location (const char *path)    
{
    return test_path_file ( path, "PERMANENT/DEFAULT_WIND" );
}

/*!
 * \brief Test if specified directory is mapset
 * \param path Path to directory
 * \return 1 The directory is mapset
 * \return 0 The directory is not mapset
 */
int G_is_mapset (const char *path)    
{
    return test_path_file ( path, "WIND" );
}

