/*
* $Id$
*
*****************************************************************************
*
* MODULE:   	Grass Initialization
* AUTHOR(S):	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
* PURPOSE:  	To print the last modification time for the given file. This
*   	    	program is strictly a UNIX program and does not need any links
*   	    	to GRASS. It is POSIX compliant, thus it should run on most
*   	    	systems. It is meant to be used to determine if one file is
*   	    	older than another file. For example, to generate a binary
*   	    	distribution of GRASS, we use the installed binaries. If
*   	    	someone compiled GRASS again but did not install it, then the
*   	    	user should be informed to run "make install" so that the 
*   	    	binary distribution uses the new compiled code. In order to 
*   	    	detect this situation, we compare the modification times of
*   	    	the installed grass executable and the compiled grass
*   	    	executable. This program prints -1 on error and the
*   	    	modification time on success.
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*============================= Include Files ==============================*/

/* System include files */
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

/* Grass and local include files */
/* none */

/*======================= Internal Constants/Defines =======================*/

/* none */

/*========================== Internal Typedefs =============================*/

/* none */

/*====================== Static Variable Declaration =======================*/

/* none */

/*============================== Prototypes ================================*/

/* none */

/*======================= Internal Static Functions ========================*/

/* none */

/*============================ Main Function ===============================*/

int main (int argc, char *argv[])
{
    struct stat statBuf;    /* structure to store stat information */
    int     	status;     /* return status for stat call */
    
    /* Check for the correct number of arguments */
    if (argc != 2)
    {
    	/* Error, print -1 - return 0 so Make can trap the error */
	fprintf(stdout, "-1\n");
	return(0);
    }
    
    /* Get the information from stat */
    status = stat(argv[1], &statBuf);
    
    if (status < 0)
    {
    	/* Error, print -1 - return 0 so Make can trap the error */
	fprintf(stdout, "-1\n");
	return(0);
    }
    
    /* Print out the modification time */
    fprintf(stdout, "%ld\n", statBuf.st_mtime);
    
    return(0);
}
