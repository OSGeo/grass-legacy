/***********************************************************************

File     	:	get_filename.c
Function 	:	char *GetFileName(string)
Args	 	:	    char *string; -- fullpathname of file.

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	21 April 1990
Last Revised	:
Abstract 	:	Extracts and returns the filename from a full path
			specification.
Returns  	:	1 if successful, 0 if not.

***********************************************************************/
#include <stdio.h>
#include <strings.h>

char *GetFileName(string)
    char *string;
    {
    char name[20];
    char *tmp;
    register int i = 0;

    tmp = (char *)rindex(string, '/');
    while ( *(++tmp) != NULL)
       {
       name[i] = *tmp;
       i++;
       }
    name[i] = (char)NULL; /* null terminate */
    return(name);
    }
