#ifndef lint
static char *SCCSID = "@(#)rgroup.c	AMG v.3.1";
#endif
/* get group of real data */
# include <stdio.h>
rgroup(fid, ptr, n) FILE *fid; double *ptr; {
	while (n--)
		if (fscanf(fid,"%le",ptr++) != 1)
			return 1;
	return 0;
}
