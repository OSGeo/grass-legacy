/* %W% %G% */
#include "usgs.h"

getargs (argc, argv)
    char *argv[];
{
    int i,tempint;
    char temp[100];

    *tapename = blocksize = 0;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "if=%s",temp) == 1)
	{
	    if (*tapename) return 0;
	    strcpy (tapename, temp);
	    continue;
	}

	if (sscanf (argv[i], "bs=%d",&tempint) == 1)
	{
	    if (blocksize) return 0;
	    blocksize = tempint;
	    continue;
	}
    }

    if (*tapename == 0) strcpy (tapename, "/dev/rmt0");
    if (blocksize == 0) return 0;          
    return 1;
}
