/***********************************************************************

File            :       CellEdit.c
Function        :       char *MakeCellPath()

Author          :       Frank Goodman -- spanki@ced.berkeley.edu
Creation        :       28 January 1990
Last Revised    :       23 February 1990
Abstract        :       Build the path to the cell files.
Returns         :       the path.

***********************************************************************/
#include "/home/grass3/src/libes/gis.h"
char *MakeCellPath()
    {
    char *user;         /* user name */
    char *locPath;      /* loaction path */
    char *fullPath;     /* full path to cell files */

    /* get user */
    user = (char *)G_calloc((strlen(G_whoami()) + 5 ), sizeof(char));
    (char *)G_strcpy(user,G_whoami());
    (char *)G_strcat(user,"/cell");

    /* get path to location  */
    locPath = (char *)G_calloc(strlen(G_location_path()+1), sizeof(char));
    (char *)G_strcpy(locPath,G_location_path());;
    (char *)G_strcat(locPath,"/");

    fullPath =
        (char *)G_calloc((strlen(user) + strlen(locPath)), sizeof(char));
    (char *)G_strcpy(fullPath, locPath);
    (char *)G_strcat(fullPath, user);

    free(user);
    free(locPath);
    return(fullPath);
    }
