#ifndef lint
static char *SCCSID = "@(#)pgroup.c	AMG v.3.1";
#endif
# include <stdio.h>
pgroup(fid, ptr, l) FILE *fid; double *ptr; {
	int i;

	for (i = 0; l--; i++)
		fprintf(fid,(((i&3)==3)|| !l)?" %.15e\n":" %.15e",*ptr++);
}
