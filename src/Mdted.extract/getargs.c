/* %W% %G% */

#include "dma.h"

getargs (argc, argv)
    char *argv[];
{
    int i;
    int k;
    char nflag, eflag, sflag, wflag;
    int d1,m1,s1;
    int d2,m2,s2;
    int lat, lon;
    char hemi1[3], hemi2[3];
    char temp[100];

    nflag = sflag = eflag = wflag = 0;

    verbose = 0;
    stopok = 1;
    *tapename = *outname = *headname = 0;

    for (i = 1; i < argc; i++)
    {
	if (strcmp (argv[i],"-f") == 0)
	{
	    stopok = 0;
	    continue;
	}

	if (strcmp (argv[i],"-v") == 0)
	{
	    verbose++;
	    continue;
	}

	if (sscanf (argv[i], "if=%s", temp) == 1)
	{
	    if (*tapename) return 0;
	    strcpy (tapename, temp);
	    continue;
	}

	if (sscanf (argv[i], "of=%s", temp) == 1)
	{
	    if (*outname) return 0;
	    strcpy (outname, temp);
	    continue;
	}

	if (sscanf (argv[i], "hf=%s", temp) == 1)
	{
	    if (*headname) return 0;
	    strcpy (headname, temp);
	    continue;
	}

	if (sscanf (argv[i], "n=%s", temp) == 1)
	{
	    if (nflag++) return 0;
	    if (!scan_geo (temp, "sn", &north))
		return 0;
	    continue;
	}

	if (sscanf (argv[i], "s=%s", temp) == 1)
	{
	    if (sflag++) return 0;
	    if (!scan_geo (temp, "sn", &south))
		return 0;
	    continue;
	}

	if (sscanf (argv[i], "e=%s", temp) == 1)
	{
	    if (eflag++) return 0;
	    if (!scan_geo (temp, "ew", &east))
		return 0;
	    continue;
	}

	if (sscanf (argv[i], "w=%s", temp) == 1)
	{
	    if (wflag++) return 0;
	    if (!scan_geo (temp, "ew", &west))
		return 0;
	    continue;
	}
	sprintf (temp, "%s - unrecognized parameter\n", argv[i]);
	error (temp, 0);
	return 0;
    }

    if (verbose == 1)
	verbose = 0;
    else if (verbose == 0)
	verbose = 1;

    if (!(nflag && sflag && eflag && wflag)) return 0;
    if (*outname == 0) return 0;
    if (*headname == 0) return 0;
    if (*tapename == 0) strcpy (tapename, "/dev/rmt0");
    return 1;
}
